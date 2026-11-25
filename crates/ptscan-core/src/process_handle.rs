//! High-level interface to processes.

use crate::{
    error::Error,
    filter_expr::FilterExpr,
    platform::{self, Thread},
    process::MemoryType,
    progress_reporter::ProgressReporter,
    scanner::{Chunk, ScanSink},
    system, utils, Address, AddressRange, Addresses, Base, BitSet, BlockAddresses,
    BlockAddressesBuilder, BlockError, Cached, DefaultScanner, FollowablePointer,
    MemoryInformation, MemoryReader, Pointer, PointerInfo, PointerWidth, PortableBase, Process,
    ProcessId, ProcessInfo, Scanner, Size, Test, ThreadId, Token, Type, TypeHint, TypedFilterExpr,
    Value, ValueExpr, ValueRef, Values,
};
use anyhow::{anyhow, bail, Context as _};
use crossbeam_queue::SegQueue;
use dynamicvec::DynamicConverter;
use hashbrown::{HashMap, HashSet};
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::convert::{TryFrom as _, TryInto as _};
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc;

/// A cancellation token, either supplied by the caller or owned locally.
pub enum Cancel<'a> {
    Borrowed(&'a Token),
    Owned(Token),
}

impl std::ops::Deref for Cancel<'_> {
    type Target = Token;

    fn deref(&self) -> &Token {
        match self {
            Cancel::Borrowed(token) => token,
            Cancel::Owned(token) => token,
        }
    }
}

/// Shared environment of a scan: where it runs, how to cancel it and how to
/// report progress.
pub struct ScanContext<'a, P> {
    pub pool: &'a rayon::ThreadPool,
    pub cancel: Cancel<'a>,
    pub progress: P,
}

impl<'a, P> ScanContext<'a, P> {
    /// Build a context; without a `cancel` token the scan cannot be cancelled.
    pub fn new(pool: &'a rayon::ThreadPool, cancel: Option<&'a Token>, progress: P) -> Self {
        Self {
            pool,
            cancel: match cancel {
                Some(cancel) => Cancel::Borrowed(cancel),
                None => Cancel::Owned(Token::new()),
            },
            progress,
        }
    }
}

/// The results of a scan: addresses with their initial and last values.
///
/// Invariants: all three have the same length, and the addresses are
/// strictly increasing (which [`BlockAddresses`] guarantees), so results can
/// be found by binary search.
#[derive(Clone)]
pub struct ScanResults {
    addresses: BlockAddresses,
    /// Values at the initial scan, `None` while they are the same as `last`.
    initial: Option<Values>,
    last: Values,
}

impl ScanResults {
    pub fn new(addresses: BlockAddresses, initial: Values, last: Values) -> Self {
        debug_assert_eq!(addresses.len(), initial.len());
        debug_assert_eq!(addresses.len(), last.len());

        Self {
            addresses,
            initial: Some(initial),
            last,
        }
    }

    /// Results whose initial values are the same as `values`, which are not
    /// stored twice until `last` is about to change.
    pub fn unchanged(addresses: BlockAddresses, values: Values) -> Self {
        debug_assert_eq!(addresses.len(), values.len());

        Self {
            addresses,
            initial: None,
            last: values,
        }
    }

    /// The addresses of the results, strictly increasing.
    pub fn addresses(&self) -> &BlockAddresses {
        &self.addresses
    }

    /// The step of the scan the addresses were found by.
    pub fn step(&self) -> u64 {
        self.addresses.step()
    }

    /// The number of results whose address is below `address`, which is the
    /// index of the first result at or after it.
    pub fn lower_bound(&self, address: Address) -> usize {
        self.addresses.lower_bound(address)
    }

    /// The index of the result at `address`, if any.
    pub fn index_of(&self, address: Address) -> Option<usize> {
        self.addresses.index_of(address)
    }

    /// The values at the initial scan.
    pub fn initial(&self) -> &Values {
        self.initial.as_ref().unwrap_or(&self.last)
    }

    /// The values at the last scan.
    pub fn last(&self) -> &Values {
        &self.last
    }

    /// Bytes used by the results.
    pub fn bytes(&self) -> usize {
        self.addresses
            .bytes()
            .saturating_add(self.initial.as_ref().map_or(0, Values::bytes))
            .saturating_add(self.last.bytes())
    }

    /// Store the initial values separately, which must be done before `last`
    /// is changed. Returns the addresses, initial values and last values.
    pub fn split_mut(&mut self) -> (&BlockAddresses, &Values, &mut Values) {
        let Self {
            addresses,
            initial,
            last,
        } = self;
        let initial = &*initial.get_or_insert_with(|| last.clone());
        (addresses, initial, last)
    }

    pub fn len(&self) -> usize {
        self.addresses.len()
    }

    pub fn is_empty(&self) -> bool {
        self.addresses.is_empty()
    }

    /// Remove the results whose index is set in `marked`, keeping the order
    /// of the others.
    ///
    /// The addresses are re-encoded first; if that fails nothing is removed.
    pub fn remove_marked(&mut self, marked: &BitSet) -> Result<(), BlockError> {
        if marked.is_empty() {
            return Ok(());
        }

        self.addresses
            .retain_by_index(|index| !marked.test(index))?;

        if let Some(initial) = &mut self.initial {
            initial.retain_by_index(|index| !marked.test(index));
            debug_assert_eq!(self.addresses.len(), initial.len());
        }

        self.last.retain_by_index(|index| !marked.test(index));
        debug_assert_eq!(self.addresses.len(), self.last.len());
        Ok(())
    }

    /// Replace the addresses with `addresses`, which must be the current ones
    /// without those whose index is set in `marked`, and remove the marked
    /// values.
    ///
    /// Fails without changing anything if `addresses` has a different step,
    /// or a length other than the number of unmarked results.
    pub fn retain_with(
        &mut self,
        addresses: BlockAddresses,
        marked: &BitSet,
    ) -> Result<(), BlockError> {
        if addresses.step() != self.addresses.step() {
            return Err(BlockError::StepMismatch(
                self.addresses.step(),
                addresses.step(),
            ));
        }

        let removed = marked.count_below(self.len());

        if Some(addresses.len()) != self.len().checked_sub(removed) {
            return Err(BlockError::Corrupt);
        }

        self.addresses = addresses;

        if removed == 0 {
            return Ok(());
        }

        if let Some(initial) = &mut self.initial {
            initial.retain_by_index(|index| !marked.test(index));
            debug_assert_eq!(self.addresses.len(), initial.len());
        }

        self.last.retain_by_index(|index| !marked.test(index));
        debug_assert_eq!(self.addresses.len(), self.last.len());
        Ok(())
    }
}

/// Size of the chunks of values handed to a task, so that a few chunks per
/// thread are available without sending a message for every value.
fn chunk_size(pool: &rayon::ThreadPool, total: usize) -> usize {
    (total / pool.current_num_threads().max(1).saturating_mul(4)).clamp(64, 4096)
}

/// Run `work` over `0..total` split into chunks in parallel, reporting
/// progress per chunk. `work` returns how many values were accepted and the
/// indices rejected, which are handed to `on_rejected` in the calling thread.
fn run_chunks<P: ScanProgress + Send>(
    pool: &rayon::ThreadPool,
    cancel: &Token,
    reporter: &mut ProgressReporter<'_, '_, P>,
    total: usize,
    work: impl Fn(std::ops::Range<usize>) -> anyhow::Result<(u64, Vec<usize>)> + Sync,
    mut on_rejected: impl FnMut(usize) + Send,
) {
    let chunk = chunk_size(pool, total);
    let chunks = total.div_ceil(chunk);
    let work = &work;

    pool.install(|| {
        rayon::scope(|s| {
            let (tx, rx) = mpsc::channel::<anyhow::Result<(usize, u64, Vec<usize>)>>();

            s.spawn(move |_| {
                (0..chunks).into_par_iter().for_each_with(tx, |tx, c| {
                    if cancel.test() {
                        return;
                    }

                    let start = c.saturating_mul(chunk);
                    let end = start.saturating_add(chunk).min(total);

                    let _ = tx.send(work(start..end).map(|(accepted, rejected)| {
                        (end.saturating_sub(start), accepted, rejected)
                    }));
                });
            });

            let mut accepted = 0u64;

            for result in rx {
                if let Some((count, a, rejected)) = reporter.eval(result) {
                    accepted = accepted.saturating_add(a);

                    for index in rejected {
                        on_rejected(index);
                    }

                    reporter.tick_n(count, accepted);
                }
            }
        })
    });
}

/// Which parts of the module images [`ProcessHandle::scan_ranges`] keeps.
#[derive(Debug, Clone, Copy)]
pub(crate) enum ModuleParts {
    /// Only the parts mapped writable, where an initial scan looks: code and
    /// read-only data are left out.
    Writable,
    /// The whole images, so a rescan keeps results anywhere in them.
    Whole,
}

/// Size of the chunks memory is read in when scanning.
const DEFAULT_BUFFER_SIZE: usize = 0x100_000;

#[derive(Debug, Clone, Default)]
pub struct InitialScanConfig {
    /// Only scan module images. Either way only the writable parts of module
    /// images are scanned, not their code or read-only data.
    pub modules_only: bool,
    pub tasks: Option<usize>,
    pub buffer_size: Option<usize>,
    pub alignment: Option<usize>,
    pub aligned: Option<bool>,
}

/// Trait used for interfacing with dynamic values in a scan.
pub trait ValueHolder {
    type Pointer: FollowablePointer;

    /// Access the address of the value.
    fn pointer(&self) -> &Self::Pointer;

    /// The type of the last value.
    fn value_type(&self) -> Type;

    /// Get information on last value.
    fn value(&self) -> ValueRef<'_>;

    /// Insert an updated value into the holder.
    fn insert(&mut self, value: Value);
}

pub trait ScanProgress {
    /// Report the total number of bytes to process.
    fn report_bytes(&mut self, bytes: Size) -> anyhow::Result<()>;

    /// Report that the process has progresses to the given percentage.
    fn report(&mut self, percentage: usize, results: u64) -> anyhow::Result<()>;
}

#[derive(Debug, Clone, Default)]
pub struct ModulesState {
    pub process_name: Option<String>,
    pub process_file_name: Option<OsString>,
    pub modules: Vec<ModuleInfo>,
    pub modules_index: HashMap<String, u16>,
    pub modules_address: HashMap<String, Address>,
    pub kernel32: Option<AddressRange>,
}

/// A handle for a process.
#[derive(Debug)]
pub struct ProcessHandle {
    pub name: OsString,
    /// Handle to the process.
    pub process: Process,
    /// Name of the process (if present).
    pub modules: ModulesState,
    /// Threads.
    pub threads: Vec<ProcessThread>,
}

impl ProcessHandle {
    /// Open the given process id and create a simplified handle to it.
    ///
    /// This returns `None` if:
    ///
    /// * We try to open a special PID like 0.
    /// * We don't have sufficient privileges to open the process.
    /// * The process no longer exists.
    pub fn open(pid: ProcessId) -> Result<Option<ProcessHandle>, Error> {
        // IDLE process, cannot be opened.
        if pid == 0 {
            return Ok(None);
        }

        let process = match Process::builder().all_access().build(pid) {
            Ok(process) => process,
            Err(e) if platform::process_unavailable(&e) => return Ok(None),
            Err(e) => return Err(e),
        };

        let name = process.module_base_name()?;

        Ok(Some(ProcessHandle {
            name,
            process,
            modules: Default::default(),
            threads: Vec::new(),
        }))
    }

    /// Find the first process matching the given name.
    pub fn open_by_name(name: &str) -> anyhow::Result<Option<ProcessHandle>> {
        let name = OsStr::new(name);

        for pid in system::processes()? {
            let handle = match ProcessHandle::open(pid).with_context(|| Error::OpenProcess(pid))? {
                Some(handle) => handle,
                // process cannot be opened.
                None => continue,
            };

            if name != handle.name {
                continue;
            }

            return Ok(Some(handle));
        }

        Ok(None)
    }

    /// Get the current module state for the process.
    pub fn get_modules(&self) -> anyhow::Result<ModulesState> {
        let mut process_name = None;
        let mut process_file_name = None;
        let mut modules = Vec::with_capacity(self.modules.modules.len());
        let mut modules_index = HashMap::new();
        let mut modules_address = HashMap::new();
        let mut kernel32 = None;

        for (id, module) in self.process.modules()?.into_iter().enumerate() {
            let id = u16::try_from(id)?;

            let name = module
                .name(&self.process)
                .with_context(|| "failed to get module name")?;

            let file_name = module.file_name().ok();

            if process_name.is_none() {
                process_name = Some(name.to_string_lossy().to_string());
                process_file_name = file_name.clone();
            }

            let info = module
                .info(&self.process)
                .with_context(|| "failed to get module info")?;

            let range = AddressRange {
                base: info.base_of_dll,
                size: info.size_of_image,
            };

            if name.as_os_str() == OsStr::new("KERNEL32.DLL") {
                kernel32 = Some(range);
            }

            let name = name.to_string_lossy().to_string();
            let file_name = file_name.map(|n| n.to_string_lossy().to_string());

            modules_address.insert(name.clone(), range.base);
            modules_index.insert(name.clone(), id);
            modules.push(ModuleInfo {
                id,
                name,
                file_name,
                range,
            });
        }

        modules.sort_by_key(|m| m.range.base);

        Ok(ModulesState {
            process_name,
            process_file_name,
            modules,
            modules_address,
            modules_index,
            kernel32,
        })
    }

    /// Update modules with the result from a get_modules call.
    pub fn update_modules(&mut self, modules: ModulesState) {
        self.modules = modules;
    }

    /// Refresh module state in-place.
    ///
    /// This is a combination of `get_modules` and `update_modules` for,
    /// convenience, but requires extended exclusive access which might not be
    /// desirable.
    pub fn refresh_modules(&mut self) -> anyhow::Result<()> {
        let update = self.get_modules()?;
        self.update_modules(update);
        Ok(())
    }

    /// Get threads with custom modules state.
    pub fn get_threads_with_modules(
        &self,
        modules: &ModulesState,
    ) -> anyhow::Result<Vec<ProcessThread>> {
        let mut threads = Vec::new();
        Self::load_threads_into(&self.process, &mut threads, modules.kernel32.as_ref())?;
        Ok(threads)
    }

    /// Update thread information..
    pub fn update_threads(&mut self, threads: Vec<ProcessThread>) {
        self.threads = threads;
    }

    /// Refresh the known threads in place.
    pub fn refresh_threads(&mut self) -> anyhow::Result<()> {
        Self::load_threads_into(
            &self.process,
            &mut self.threads,
            self.modules.kernel32.as_ref(),
        )
    }

    /// Refresh information about known threads.
    pub fn load_threads_into(
        process: &Process,
        threads: &mut Vec<ProcessThread>,
        kernel32: Option<&AddressRange>,
    ) -> anyhow::Result<()> {
        threads.clear();

        for t in system::threads(process.process_id())? {
            if t.process_id() != process.process_id() {
                continue;
            }

            let id = t.thread_id();

            let thread = Thread::builder()
                .all_access()
                .query_information()
                .get_context()
                .build(id)
                .with_context(|| Error::BuildThread(id))?;

            // Threads whose stack the platform cannot find are left out.
            let stack = match thread.thread_stack(process) {
                Err(e) if matches!(e.downcast_ref(), Some(Error::Unsupported)) => continue,
                result => result.with_context(|| Error::ThreadStack(thread.thread_id()))?,
            };

            let stack_exit = Self::scan_for_exit(process, stack, kernel32)
                .with_context(|| Error::ScanForExit)?;

            threads.push(ProcessThread {
                id,
                stack,
                stack_exit,
            });
        }

        threads.sort_by_key(|t| t.stack.base);
        Ok(())
    }

    /// Find the module that the address is contained in.
    pub fn find_location(&self, address: Address) -> Location<'_> {
        if let Some(module) = self.find_module(address) {
            return Location::Module(module);
        }

        // TODO: need exact stack for thread.
        if let Some(thread) = self.find_thread_by_stack(address) {
            return Location::Thread(thread);
        }

        Location::None
    }

    /// Convert an address into a pointer base.
    pub fn address_to_base(&self, address: Address) -> Base {
        match self.find_location(address) {
            Location::Module(module) => {
                let offset = address.offset_of(module.range.base);

                Base::Module {
                    id: module.id,
                    offset,
                }
            }
            _ => Base::Address { address },
        }
    }

    /// Convert an address into a portable pointer base.
    pub fn address_to_portable_base(&self, address: Address) -> PortableBase {
        match self.find_location(address) {
            Location::Module(module) => {
                let offset = address.offset_of(module.range.base);

                PortableBase::Module {
                    name: module.name.to_string(),
                    offset,
                }
            }
            _ => PortableBase::Address { address },
        }
    }

    /// Find if address is contained in a module.
    pub fn find_module(&self, address: Address) -> Option<&ModuleInfo> {
        AddressRange::find_in_range(&self.modules.modules, |m| &m.range, address)
    }

    /// Find if address is contained in a module.
    pub fn find_thread_by_stack(&self, address: Address) -> Option<&ProcessThread> {
        AddressRange::find_in_range(&self.threads, |m| &m.stack, address)
    }

    /// Scan for the base of the stack.
    ///
    /// TODO: look into using VirtualQueryEx to get the range of the stack.
    /// https://msdn.microsoft.com/en-us/library/windows/desktop/aa366907(v=vs.85).aspx
    ///
    /// Ported from:
    /// https://github.com/cheat-engine/cheat-engine/blob/0d9d35183d0dcd9eeed4b4d0004fa3a1981b018a/Cheat%20Engine/CEFuncProc.pas
    pub fn scan_for_exit(
        process: &Process,
        stack: AddressRange,
        kernel32: Option<&AddressRange>,
    ) -> anyhow::Result<Option<Address>> {
        let ptr_width = Size::try_from(process.pointer_width.size())?;
        let mut buf = vec![0u8; stack.size.as_usize()];
        process.read_process_memory(stack.base, &mut buf)?;

        if !stack.base.is_aligned(ptr_width) {
            bail!(
                "for some reason, the stack {:?} is not aligned with {}",
                stack,
                ptr_width
            );
        }

        let kernel32 = match kernel32 {
            Some(kernel32) => kernel32,
            None => return Ok(None),
        };

        for (n, w) in buf.chunks(ptr_width.as_usize()).enumerate().rev() {
            // TODO: make independent of host architecture (use u64).
            let ptr = process
                .pointer_width
                .decode_pointer::<byteorder::NativeEndian>(w);

            if kernel32.contains(ptr) {
                let stack_offset = Size::try_from(n * ptr_width.as_usize())?;

                let address = match stack.base.checked_size(stack_offset) {
                    Some(address) => address,
                    None => bail!(
                        "adding stack base `{}` + to stack offset `{}` overflowed",
                        stack.base,
                        stack_offset
                    ),
                };

                return Ok(Some(address));
            }
        }

        Ok(None)
    }

    pub fn null_address_proxy(&self) -> AddressProxy<'_, Pointer> {
        AddressProxy {
            pointer: Pointer::null_ref(),
            handle: self,
            followed: Cached::None,
            cache: None,
        }
    }

    /// Construct an address proxy for the given address.
    pub fn address_proxy<'a, P>(&'a self, pointer: &'a P) -> AddressProxy<'a, P>
    where
        P: FollowablePointer,
    {
        AddressProxy {
            pointer,
            handle: self,
            followed: Cached::None,
            cache: None,
        }
    }

    /// The memory to scan, sorted by base and without overlaps, each byte
    /// once: the relevant regions minus thread stacks, plus the module images
    /// as `parts` says. With `modules_only`, just the module images.
    ///
    /// Callers binary-search the result, so it has to stay sorted.
    pub(crate) fn scan_ranges(
        &self,
        modules_only: bool,
        parts: ModuleParts,
    ) -> anyhow::Result<Vec<AddressRange>> {
        let mut images = self
            .modules
            .modules
            .iter()
            .map(|m| m.range)
            .collect::<Vec<_>>();
        images.sort();

        // Modules may overlap each other; merge them so each byte is covered
        // once.
        let mut modules = Vec::<AddressRange>::with_capacity(images.len());
        let mut end = 0u64;

        for m in images {
            let m_end = m.base.0.saturating_add(m.size.0);

            if m_end <= end {
                continue;
            }

            let start = m.base.0.max(end);
            modules.push(AddressRange {
                base: Address(start),
                size: Size(m_end - start),
            });
            end = m_end;
        }

        let whole = matches!(parts, ModuleParts::Whole);
        let mut ranges = Vec::new();

        if !modules_only || !whole {
            let thread_stacks = self.threads.iter().map(|t| t.stack).collect::<HashSet<_>>();

            for m in self.process.virtual_memory_regions() {
                let m = m?;

                if !utils::is_scannable(&m) {
                    continue;
                }

                if !whole {
                    covered_ranges(m.range, &modules, &mut ranges);
                }

                if !modules_only
                    && !matches!(m.ty, Some(MemoryType::Mapped))
                    && !thread_stacks.contains(&m.range)
                {
                    uncovered_ranges(m.range, &modules, &mut ranges);
                }
            }
        }

        if whole {
            ranges.extend(modules);
        }

        ranges.sort();
        Ok(ranges)
    }

    pub fn rescan_values(
        &self,
        ctx: ScanContext<'_, impl ScanProgress + Send>,
        results: &mut ScanResults,
        filter: &FilterExpr,
    ) -> anyhow::Result<()> {
        if results.len() < 100_000 {
            self.rescan_small(ctx, results, filter)
        } else {
            self.rescan_large(ctx, results, filter)
        }
    }

    /// Read the value of many memory locations.
    pub fn rescan_large(
        &self,
        ctx: ScanContext<'_, impl ScanProgress + Send>,
        results: &mut ScanResults,
        filter: &FilterExpr,
    ) -> anyhow::Result<()> {
        self.rescan_windows(ctx, results, filter, DEFAULT_BUFFER_SIZE)
    }

    /// Rescan `results` by splitting the scanned memory into windows of
    /// `window_size` bytes, each read only over the span of the results inside
    /// it, and handed out to the pool one window at a time.
    ///
    /// Results are sorted by address, so the results of a window are the
    /// contiguous index range found by binary search on its bounds. Results
    /// outside every window are rejected. Each window task decodes the
    /// addresses of its blocks as it tests them and re-encodes the survivors,
    /// which are concatenated in window order into the new address set. The
    /// rejected values are removed in one stable pass at the end.
    pub(crate) fn rescan_windows(
        &self,
        ctx: ScanContext<'_, impl ScanProgress + Send>,
        results: &mut ScanResults,
        filter: &FilterExpr,
        window_size: usize,
    ) -> anyhow::Result<()> {
        /// Bytes read past the last result of a window, so a value starting
        /// in it decodes as it would from the whole region. Strings decode at
        /// most 256 bytes.
        const MIN_TAIL: usize = 0x1000;

        let ScanContext {
            pool: thread_pool,
            cancel,
            progress,
        } = ctx;
        let cancel = &*cancel;

        if results.is_empty() {
            return Ok(());
        }

        // The values are changed below, so the initial ones need their own copy.
        results.split_mut();

        let len = results.len();

        if len != results.last().len() || len != results.initial().len() {
            bail!(
                "argument length mismatch, addresses ({}) != values ({}) or initial ({})",
                len,
                results.last().len(),
                results.initial().len(),
            );
        }

        if u32::try_from(len).is_err() {
            bail!("cannot rescan more than {} results", u32::MAX);
        }

        let filter = filter.type_check(
            self,
            results.initial().ty(),
            results.last().ty(),
            results.last().ty(),
        )?;
        let filter = &filter;

        let tail = [results.initial().ty(), results.last().ty()]
            .iter()
            .filter_map(|ty| ty.size())
            .fold(MIN_TAIL, usize::max);
        let window_size = u64::try_from(window_size.max(1))?;

        let ranges = self.scan_ranges(false, ModuleParts::Whole)?;

        let mut windows = Vec::new();
        let mut rejected = BitSet::with_capacity(len);

        // Index ranges are clamped to start where the previous one ended, so
        // every result is in at most one window even if ranges overlapped.
        let mut covered = 0usize;
        let mut matched = 0usize;

        for range in &ranges {
            let end = range.base.0.saturating_add(range.size.0);
            let mut base = range.base.0;

            while base < end {
                let next = base.saturating_add(window_size).min(end);
                let first = results.lower_bound(Address(base)).max(covered);
                let last = results.lower_bound(Address(next)).max(first);

                for index in covered..first {
                    rejected.set(index);
                }

                if first < last {
                    windows.push(Window {
                        range_end: end,
                        indices: first..last,
                    });
                    matched = matched.saturating_add(last.saturating_sub(first));
                }

                covered = last;
                base = next;
            }
        }

        for index in covered..len {
            rejected.set(index);
        }

        let mut last_error = None;
        let mut outputs = Vec::new();
        outputs.resize_with(windows.len(), || None);
        let mut concat = Concat {
            builder: BlockAddressesBuilder::new(results.step()),
            next: 0,
        };

        if matched > 0 {
            let mut reporter = ProgressReporter::new(progress, matched, cancel, &mut last_error);

            let tasks = thread_pool.current_num_threads().max(1);
            let next_window = AtomicUsize::new(0);

            let work = Work {
                handle: self,
                windows: &windows,
                next_window: &next_window,
                results: &*results,
                filter,
                cancel,
                buffer_size: usize::try_from(window_size)?.saturating_add(tail),
                tail: u64::try_from(tail)?,
            };

            thread_pool.install(|| {
                rayon::scope(|s| {
                    let (tx, rx) = mpsc::channel::<Task>();

                    for _ in 0..tasks {
                        let tx = tx.clone();
                        let work = &work;

                        s.spawn(move |_| {
                            let result = work.run(&tx);
                            let _ = tx.send(Task::Done(result));
                        });
                    }

                    drop(tx);

                    let mut accepted = 0u64;

                    for task in rx {
                        match task {
                            Task::Window {
                                window,
                                count,
                                accepted: a,
                                rejected: r,
                                kept,
                            } => {
                                accepted = accepted.saturating_add(a);

                                for index in r {
                                    rejected.set(index as usize);
                                }

                                if let Some(slot) = outputs.get_mut(window) {
                                    *slot = Some(kept);
                                }

                                concat.drain(&mut outputs);
                                reporter.tick_n(count, accepted);
                            }
                            Task::Done(result) => {
                                reporter.eval(result);
                            }
                        }
                    }
                })
            });
        }

        // Windows without output (cancelled or failed) keep all of their
        // results, and none of them are marked as rejected.
        let removed = match concat.finish(&mut outputs, results.addresses(), &windows) {
            Ok(kept) => results.retain_with(kept, &rejected),
            Err(e) => Err(e),
        };

        if let Some(e) = last_error {
            return Err(e);
        }

        removed?;

        return Ok(());

        struct Window {
            range_end: u64,
            /// The indices of the results inside the window.
            indices: std::ops::Range<usize>,
        }

        enum Task {
            Window {
                window: usize,
                count: usize,
                accepted: u64,
                rejected: Vec<u32>,
                /// The addresses of the window which were not rejected.
                kept: BlockAddresses,
            },
            Done(anyhow::Result<()>),
        }

        /// Concatenates the kept addresses of each window in window order.
        struct Concat {
            builder: Result<BlockAddressesBuilder, BlockError>,
            /// The next window to append.
            next: usize,
        }

        impl Concat {
            /// Append the outputs of windows that are ready, in order.
            fn drain(&mut self, outputs: &mut [Option<BlockAddresses>]) {
                let Ok(builder) = &mut self.builder else {
                    return;
                };

                while let Some(kept) = outputs.get_mut(self.next).and_then(Option::take) {
                    if let Err(e) = builder.append(&kept) {
                        self.builder = Err(e);
                        return;
                    }

                    self.next = self.next.saturating_add(1);
                }
            }

            /// Append every remaining window, using the window's original
            /// addresses where it has no output.
            fn finish(
                mut self,
                outputs: &mut [Option<BlockAddresses>],
                addresses: &BlockAddresses,
                windows: &[Window],
            ) -> Result<BlockAddresses, BlockError> {
                loop {
                    self.drain(outputs);

                    let builder = self.builder.as_mut().map_err(|e| e.clone())?;

                    let Some(window) = windows.get(self.next) else {
                        break;
                    };

                    for address in addresses
                        .iter_from(window.indices.start)
                        .take(window.indices.len())
                    {
                        builder.push(address)?;
                    }

                    self.next = self.next.saturating_add(1);
                }

                self.builder?.finish()
            }
        }

        struct Work<'a> {
            handle: &'a ProcessHandle,
            windows: &'a [Window],
            next_window: &'a AtomicUsize,
            results: &'a ScanResults,
            filter: &'a TypedFilterExpr<'a>,
            cancel: &'a Token,
            buffer_size: usize,
            tail: u64,
        }

        impl Work<'_> {
            fn run(&self, tx: &mpsc::Sender<Task>) -> anyhow::Result<()> {
                const CHECK_INTERVAL: usize = 0x1000;

                let addresses = self.results.addresses();
                let initial = self.results.initial();
                let values = self.results.last();

                let mut buffer = vec![0u8; self.buffer_size];

                while !self.cancel.test() {
                    let w = self.next_window.fetch_add(1, Ordering::Relaxed);

                    let Some(window) = self.windows.get(w) else {
                        break;
                    };

                    let indices = window.indices.clone();

                    // Sorted, so the first and last results bound the span.
                    let (Some(Address(min)), Some(Address(max))) = (
                        addresses.iter_from(indices.start).next(),
                        indices
                            .end
                            .checked_sub(1)
                            .and_then(|i| addresses.iter_from(i).next()),
                    ) else {
                        continue;
                    };

                    let read_end = max.saturating_add(self.tail).min(window.range_end);
                    let span = usize::try_from(read_end.saturating_sub(min))?;

                    let Some(data) = buffer.get_mut(..span) else {
                        bail!("window span {} exceeds the buffer", span);
                    };

                    let read = self
                        .handle
                        .process
                        .read_process_memory(Address(min), data)?;
                    let data = data.get(..read).unwrap_or_default();

                    let mut accepted = 0u64;
                    let mut rejected = Vec::new();
                    let mut kept = BlockAddressesBuilder::new(addresses.step())?;

                    let count = indices.len();
                    let mut window_addresses = addresses.iter_from(indices.start);

                    for (n, i) in indices.enumerate() {
                        if n % CHECK_INTERVAL == 0 && self.cancel.test() {
                            // The rest of the window is kept as it is.
                            for address in window_addresses.by_ref().take(count.saturating_sub(n)) {
                                kept.push(address)?;
                            }

                            break;
                        }

                        let address = window_addresses.next();

                        let Ok(index) = u32::try_from(i) else {
                            bail!("result index {} out of range", i);
                        };

                        // Safety: every index is in exactly one window and
                        // each window is handed to one task, so no other
                        // mutator to this value exists.
                        let (Some(address), Some(initial), Some(mut value)) =
                            (address, initial.get(i), unsafe {
                                values.get_mutator_unsafe(i)
                            })
                        else {
                            rejected.push(index);
                            continue;
                        };

                        let buffer = usize::try_from(address.0.saturating_sub(min))
                            .ok()
                            .and_then(|offset| data.get(offset..))
                            .filter(|b| !b.is_empty());

                        let Some(buffer) = buffer else {
                            rejected.push(index);
                            continue;
                        };

                        let mut proxy = BufferProxy::new(address, buffer, self.handle);

                        if let Test::True =
                            self.filter
                                .test(initial.as_ref(), value.read().as_ref(), &mut proxy)?
                        {
                            accepted = accepted.saturating_add(1);
                            value.write(proxy.eval(value.ty)?.0);
                            kept.push(address)?;
                        } else {
                            rejected.push(index);
                        }
                    }

                    let _ = tx.send(Task::Window {
                        window: w,
                        count,
                        accepted,
                        rejected,
                        kept: kept.finish()?,
                    });
                }

                Ok(())
            }
        }
    }

    pub(crate) fn rescan_small(
        &self,
        ctx: ScanContext<'_, impl ScanProgress + Send>,
        results: &mut ScanResults,
        filter: &FilterExpr,
    ) -> anyhow::Result<()> {
        let ScanContext {
            pool: thread_pool,
            cancel,
            progress,
        } = ctx;
        let cancel = &*cancel;
        if results.is_empty() {
            return Ok(());
        }

        // The values are changed below, so the initial ones need their own copy.
        let (addresses, initial, values) = results.split_mut();

        if addresses.len() != values.len() || addresses.len() != initial.len() {
            return Err(anyhow!(
                "argument length mismatch, addresses ({}) != values ({}) or initial ({})",
                addresses.len(),
                values.len(),
                initial.len(),
            ));
        }

        let len = values.len();
        let mut last_error = None;
        let mut reporter = ProgressReporter::new(progress, len, cancel, &mut last_error);

        let mut rejected = BitSet::with_capacity(len);

        let filter = filter.type_check(self, initial.ty(), values.ty(), values.ty())?;
        let filter = &filter;
        let values = &*values;

        run_chunks(
            thread_pool,
            cancel,
            &mut reporter,
            len,
            |range| {
                let mut accepted = 0u64;
                let mut rejected = Vec::new();
                let mut chunk_addresses = addresses.iter_from(range.start);

                for index in range {
                    let address = chunk_addresses.next();

                    // Safety: the chunks are disjoint, so no other mutator to
                    // this value exists.
                    let (Some(address), Some(initial), Some(mut value)) =
                        (address, initial.get(index), unsafe {
                            values.get_mutator_unsafe(index)
                        })
                    else {
                        rejected.push(index);
                        continue;
                    };

                    let mut proxy = self.address_proxy(&address);

                    if let Test::True =
                        filter.test(initial.as_ref(), value.read().as_ref(), &mut proxy)?
                    {
                        value.write(proxy.eval(value.ty)?.0);
                        accepted = accepted.saturating_add(1);
                    } else {
                        rejected.push(index);
                    }
                }

                Ok((accepted, rejected))
            },
            |index| rejected.set(index),
        );

        let removed = results.remove_marked(&rejected);

        if let Some(e) = last_error {
            return Err(e);
        }

        removed?;

        Ok(())
    }

    /// Refresh a given set of existing values without a filter.
    pub fn refresh_values(
        &self,
        ctx: ScanContext<'_, impl ScanProgress + Send>,
        addresses: &Addresses,
        values: &mut Values,
        expr: &ValueExpr,
    ) -> anyhow::Result<()> {
        let ScanContext {
            pool: thread_pool,
            cancel,
            progress,
        } = ctx;
        let cancel = &*cancel;

        if addresses.is_empty() {
            return Ok(());
        }

        if addresses.len() != values.len() {
            bail!(
                "argument length mismatch, addresses ({}) != values ({})",
                addresses.len(),
                values.len()
            );
        }

        let mut last_error = None;
        let mut reporter =
            ProgressReporter::new(progress, addresses.len(), cancel, &mut last_error);

        let expr_type = expr
            .type_of(
                self,
                TypeHint::Explicit(Type::None),
                TypeHint::Explicit(Type::None),
                TypeHint::Explicit(values.ty()),
                TypeHint::NoHint,
            )?
            .ok_or_else(|| Error::TypeInference(expr.clone()))?;

        let expr = expr.type_check(self, Type::None, Type::None, values.ty(), expr_type)?;
        let expr = &expr;
        let values = &*values;

        run_chunks(
            thread_pool,
            cancel,
            &mut reporter,
            addresses.len(),
            |range| {
                let mut count = 0u64;

                for index in range {
                    // Safety: the chunks are disjoint, so no other mutator to
                    // this value exists.
                    let (Some(address), Some(mut value)) = (addresses.get(index), unsafe {
                        values.get_mutator_unsafe(index)
                    }) else {
                        bail!("missing value at index {}", index);
                    };

                    let mut proxy = self.address_proxy(&address);
                    let new_value = expr.eval(ValueRef::None, value.read().as_ref(), &mut proxy)?;
                    value.write(new_value);
                    count = count.saturating_add(1);
                }

                Ok((count, Vec::new()))
            },
            |_| (),
        );

        if let Some(e) = last_error {
            return Err(e);
        }

        Ok(())
    }

    /// Refresh a dynamic collection of values in-place.
    pub fn refresh_dynamic_values<V>(
        &self,
        ctx: ScanContext<'_, impl ScanProgress + Send>,
        values: &mut [V],
        expr: &ValueExpr,
    ) -> anyhow::Result<()>
    where
        V: Send + ValueHolder,
    {
        let ScanContext {
            pool: thread_pool,
            cancel,
            progress,
        } = ctx;
        let cancel = &*cancel;

        if values.is_empty() {
            return Ok(());
        }

        let mut last_error = None;
        let mut reporter = ProgressReporter::new(progress, values.len(), cancel, &mut last_error);

        // Type inference and checking is done once per value type.
        let mut exprs = HashMap::new();

        for value in values.iter() {
            let value_type = value.value_type();

            if exprs.contains_key(&value_type) {
                continue;
            }

            let expr_type = expr
                .type_of(
                    self,
                    TypeHint::Explicit(Type::None),
                    TypeHint::Explicit(Type::None),
                    TypeHint::Explicit(value_type),
                    TypeHint::NoHint,
                )?
                .ok_or_else(|| Error::TypeInference(expr.clone()))?;

            let typed = expr.type_check(self, Type::None, Type::None, value_type, expr_type)?;
            exprs.insert(value_type, typed);
        }

        let exprs = &exprs;
        let total = values.len();
        let chunk = chunk_size(thread_pool, total);

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel::<anyhow::Result<usize>>();

                s.spawn(move |_| {
                    values
                        .par_chunks_mut(chunk)
                        .for_each_with(tx, |tx, values| {
                            if cancel.test() {
                                return;
                            }

                            let result = (|| {
                                for value in values.iter_mut() {
                                    let Some(expr) = exprs.get(&value.value_type()) else {
                                        bail!("missing expression for value type");
                                    };

                                    let new_value = {
                                        let mut proxy = self.address_proxy(value.pointer());
                                        expr.eval(ValueRef::None, ValueRef::None, &mut proxy)?
                                    };

                                    value.insert(new_value);
                                }

                                Ok(values.len())
                            })();

                            let _ = tx.send(result);
                        });
                });

                let mut count = 0u64;

                for result in rx {
                    if let Some(n) = reporter.eval(result) {
                        count = count.saturating_add(u64::try_from(n).unwrap_or(u64::MAX));
                        reporter.tick_n(n, count);
                    }
                }
            })
        });

        if let Some(e) = last_error {
            return Err(e);
        }

        Ok(())
    }

    /// Scan for the given value in memory.
    ///
    /// # What happens on errors
    ///
    /// Errors raised in worker threads will be collected, and the last error
    /// propagated to the user. Any error causes the processing to cancel.
    /// We can't just ignore errors raised by a scan, since these might be
    /// important to the processing at hand.
    ///
    /// Therefore, any error raised by ScanProgress will be treated as any error
    /// raised from a worker thread. Only the last error will be propagated to
    /// the user. Modifications to the provided buffers will be applied even if
    /// an error happens.
    ///
    /// The addresses are block-encoded per chunk by the workers, so a full
    /// list of them never exists. Their step is the scan's step size, or a
    /// divisor of it if a configured alignment does not share it. If
    /// `addresses` is not empty, it must have been built with that step.
    pub fn initial_scan(
        &self,
        ctx: ScanContext<'_, impl ScanProgress + Send>,
        filter: &FilterExpr,
        addresses: &mut BlockAddresses,
        values: &mut Values,
        config: InitialScanConfig,
    ) -> anyhow::Result<()> {
        let ScanContext {
            pool: thread_pool,
            cancel,
            progress,
        } = ctx;
        let cancel = &*cancel;

        let mut last_error = None;

        let value_type = values.ty();
        let aligned = config
            .aligned
            .unwrap_or_else(|| values.ty().is_default_aligned());
        let alignment = values
            .ty()
            .alignment(self)
            .ok_or_else(|| anyhow!("cannot scan for none"))?;
        let step_size = if aligned { alignment } else { 1 };
        let alignment = config
            .alignment
            .or(if aligned { Some(alignment) } else { None });

        // Every address found is a multiple of this.
        let block_step = match alignment {
            Some(alignment) => gcd(step_size, alignment),
            None => step_size,
        };
        let block_step = u64::try_from(block_step)?;

        let special = filter.special(self, values.ty())?;
        let special = special.as_ref();

        let mut specialized_scanner = None;
        let mut current_scanner = None;

        let scanner: &dyn Scanner = match filter.scanner(alignment, self, value_type)? {
            Some(scanner) => &**specialized_scanner.get_or_insert(scanner),
            None => {
                let filter = filter.type_check(self, Type::None, Type::None, values.ty())?;
                current_scanner.get_or_insert(DefaultScanner::new(
                    filter, special, alignment, step_size, value_type,
                ))
            }
        };

        let tasks = config
            .tasks
            .unwrap_or_else(|| thread_pool.current_num_threads());

        let buffer_size = config.buffer_size.unwrap_or(DEFAULT_BUFFER_SIZE);

        let mut bytes = Size::zero();

        let ranges = self.scan_ranges(config.modules_only, ModuleParts::Writable)?;

        let mut total = 0;

        let queue = SegQueue::new();

        for range in &ranges {
            if let Some(alignment) = alignment {
                if usize::try_from(range.base)? % alignment != 0 {
                    bail!(
                        "range {:?} is not supported with alignment `{}`",
                        range,
                        alignment
                    );
                }
            }

            if !bytes.add_assign(range.size) {
                bail!("bytes `{}` + range size `{}` overflowed", bytes, range.size);
            }

            total += range.size.as_usize();

            let mut offset = 0usize;
            let range_size = range.size.as_usize();

            while offset < range_size && !cancel.test() {
                let len = usize::min(buffer_size, range_size - offset);

                let address = match range.base.checked_add(offset.try_into()?) {
                    Some(address) => address,
                    None => bail!("base `{}` + offset `{}` out of range", range.base, offset),
                };

                queue.push(Item {
                    seq: queue.len(),
                    address,
                    len,
                });
                offset += len;
            }
        }

        let queue = &queue;
        let pointer_width = self.process.pointer_width;

        // Chunk results indexed by queue sequence number. Ranges are sorted
        // and disjoint and chunks are queued in order, so appending slots in
        // sequence order yields strictly increasing addresses.
        let mut slots = (0..queue.len()).map(|_| None).collect::<Vec<_>>();
        let mut next = 0usize;
        let start = values.len();

        let mut builder = BlockAddressesBuilder::new(block_step)?;

        if !addresses.is_empty() {
            builder.append(addresses)?;
        }

        // Set once appending failed, after which nothing more is appended so
        // the addresses and values stay aligned.
        let mut append_failed = false;

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel::<Task>();

                for _ in 0..tasks {
                    let tx = tx.clone();

                    s.spawn(move |_| {
                        let result = work(Work {
                            handle: self,
                            tx: &tx,
                            queue,
                            value_type,
                            buffer_size,
                            cancel,
                            scanner,
                            pointer_width,
                            block_step,
                        });

                        tx.send(Task::Done(result)).expect("send done failed");
                    });
                }

                let mut reporter = ProgressReporter::new(progress, total, cancel, &mut last_error);

                reporter.report_bytes(bytes);

                let mut hits = 0u64;

                let mut task_count = tasks;

                while task_count > 0 {
                    match rx.recv().expect("channel closed") {
                        Task::Done(result) => {
                            reporter.eval(result);
                            task_count -= 1;
                        }
                        Task::Chunk(chunk) => {
                            let ChunkResult {
                                seq,
                                bytes,
                                hits: c,
                                addresses: a,
                                values: v,
                            } = chunk;

                            if let Some(slot) = slots.get_mut(seq) {
                                *slot = Some((a, v));
                            }

                            while let Some(Some((a, mut v))) = slots.get_mut(next).map(Option::take)
                            {
                                if !append_failed {
                                    let appended = builder.append(&a).map_err(anyhow::Error::from);
                                    append_failed = reporter.eval(appended).is_none();
                                }

                                if !append_failed {
                                    values.append(&mut v);
                                }

                                next += 1;
                            }

                            hits += c;
                            reporter.tick_n(bytes, hits);
                        }
                    }
                }

                Ok::<(), anyhow::Error>(())
            })
        })?;

        // Chunks left behind by a cancelled or failed worker leave gaps; what
        // was scanned is still appended in order.
        for (a, mut v) in slots.into_iter().skip(next).flatten() {
            if append_failed {
                break;
            }

            match builder.append(&a) {
                Ok(()) => values.append(&mut v),
                Err(e) => {
                    append_failed = true;
                    last_error = Some(e.into());
                }
            }
        }

        match builder.finish() {
            Ok(built) => *addresses = built,
            Err(e) => {
                // Drop what this scan added so the values match `addresses`.
                values.retain_by_index(|index| index < start);
                last_error = Some(e.into());
            }
        }

        debug_assert_eq!(addresses.len(), values.len());

        if let Some(e) = last_error {
            return Err(e);
        }

        return Ok(());

        struct Item {
            seq: usize,
            address: Address,
            len: usize,
        }

        struct ChunkResult {
            seq: usize,
            bytes: usize,
            hits: u64,
            addresses: BlockAddresses,
            values: Values,
        }

        enum Task {
            Done(anyhow::Result<()>),
            Chunk(ChunkResult),
        }

        struct Work<'a> {
            handle: &'a ProcessHandle,
            tx: &'a mpsc::Sender<Task>,
            queue: &'a SegQueue<Item>,
            value_type: Type,
            buffer_size: usize,
            cancel: &'a Token,
            scanner: &'a dyn Scanner,
            pointer_width: PointerWidth,
            block_step: u64,
        }

        #[inline(always)]
        fn work(work: Work<'_>) -> anyhow::Result<()> {
            let Work {
                handle,
                tx,
                queue,
                value_type,
                buffer_size,
                cancel,
                scanner,
                pointer_width,
                block_step,
            } = work;

            let mut buffer = vec![0u8; buffer_size];
            // Reused across chunks; only the encoded blocks are sent.
            let mut addresses = Addresses::new(pointer_width);

            while let Some(Item { seq, address, len }) = queue.pop() {
                if cancel.test() {
                    return Ok(());
                }

                addresses.clear();
                let mut values = Values::new(value_type);

                let len = usize::min(buffer.len(), len);
                let data = &mut buffer[..len];

                let mut hits = 0;

                let len = handle.process.read_process_memory(address, data)?;

                if len == 0 {
                    // Still fill the slot so the collector can move past it.
                    let _ = tx.send(Task::Chunk(ChunkResult {
                        seq,
                        bytes: 0,
                        hits: 0,
                        addresses: BlockAddresses::new(block_step)?,
                        values,
                    }));
                    continue;
                }

                let data = &data[..len];

                scanner.scan(
                    Chunk {
                        base: address,
                        data,
                        handle,
                    },
                    &mut ScanSink {
                        addresses: &mut addresses,
                        values: &mut values,
                        hits: &mut hits,
                    },
                    cancel,
                )?;

                let addresses = BlockAddresses::from_sorted(block_step, addresses.iter_copied())?;

                let _ = tx.send(Task::Chunk(ChunkResult {
                    seq,
                    bytes: data.len(),
                    hits,
                    addresses,
                    values,
                }));
            }

            Ok(())
        }
    }
}

/// Push the parts of `range` inside any of `covered`, which is sorted by base
/// and without overlaps.
fn covered_ranges(range: AddressRange, covered: &[AddressRange], out: &mut Vec<AddressRange>) {
    let end = range.base.0.saturating_add(range.size.0);

    for c in covered {
        let c_end = c.base.0.saturating_add(c.size.0);
        let start = range.base.0.max(c.base.0);
        let stop = end.min(c_end);

        if start < stop {
            out.push(AddressRange {
                base: Address(start),
                size: Size(stop - start),
            });
        }
    }
}

/// The greatest common divisor of `a` and `b`.
fn gcd(mut a: usize, mut b: usize) -> usize {
    while b != 0 {
        (a, b) = (b, a % b);
    }

    a
}

/// Push the parts of `range` not covered by any of `covered`, which is
/// sorted by base, so memory inside a module is only scanned once.
fn uncovered_ranges(range: AddressRange, covered: &[AddressRange], out: &mut Vec<AddressRange>) {
    let end = range.base.0.saturating_add(range.size.0);
    let mut start = range.base.0;

    for c in covered {
        let c_end = c.base.0.saturating_add(c.size.0);

        if c_end <= start || c.base.0 >= end {
            continue;
        }

        if c.base.0 > start {
            out.push(AddressRange {
                base: Address(start),
                size: Size(c.base.0 - start),
            });
        }

        start = start.max(c_end);
    }

    if start < end {
        out.push(AddressRange {
            base: Address(start),
            size: Size(end - start),
        });
    }
}

// NB: forward impl to inner `Process`.
impl ProcessInfo for ProcessHandle {
    type Process = Process;
    type ByteOrder = byteorder::NativeEndian;
    type PointerInfo = Process;

    fn process(&self) -> &Self::Process {
        &self.process
    }

    fn pointer_info(&self) -> &Self::PointerInfo {
        &self.process
    }

    fn virtual_query(&self, address: Address) -> anyhow::Result<Option<MemoryInformation>> {
        self.process.virtual_query(address)
    }
}

impl PointerInfo for ProcessHandle {
    type ByteOrder = <ProcessHandle as ProcessInfo>::ByteOrder;

    fn width(&self) -> PointerWidth {
        self.process.pointer_width
    }
}

impl DynamicConverter<Type> for ProcessHandle {
    fn convert(&self, ty: Type, from: Value) -> Value {
        ty.convert(self, from)
    }
}

pub trait Proxy {
    /// Get the address of the proxy.
    fn address(&mut self) -> anyhow::Result<Option<Address>>;

    /// Evaluate the value of the proxy with the given type.
    fn eval(&mut self, ty: Type) -> anyhow::Result<(Value, Option<usize>)>;

    /// Access the underlying process handle for the proxy.
    fn handle(&self) -> &ProcessHandle;
}

#[derive(Debug, Clone)]
pub struct AddressProxy<'a, P>
where
    P: FollowablePointer,
{
    pub pointer: &'a P,
    pub(crate) handle: &'a ProcessHandle,
    /// cached, followed address.
    pub followed: Cached<Option<Address>>,
    /// The last decoded value and the type it was decoded as.
    cache: Option<(Type, (Value, Option<usize>))>,
}

impl<P> Proxy for AddressProxy<'_, P>
where
    P: FollowablePointer,
{
    fn address(&mut self) -> anyhow::Result<Option<Address>> {
        Ok(match self.followed {
            Cached::Some(address) => address,
            Cached::None => {
                let address = self.pointer.follow_default(self.handle)?;
                self.followed = Cached::Some(address);
                address
            }
        })
    }

    fn eval(&mut self, ty: Type) -> anyhow::Result<(Value, Option<usize>)> {
        if let Some((cached, result)) = &self.cache {
            if *cached == ty {
                return Ok(result.clone());
            }
        }

        let result = match self.address()? {
            Some(address) => match ty.decode(&self.handle.process, address) {
                Ok(value) => value,
                Err(..) => (Value::None, None),
            },
            None => (Value::None, None),
        };

        self.cache = Some((ty, result.clone()));
        Ok(result)
    }

    fn handle(&self) -> &ProcessHandle {
        self.handle
    }
}

/// A proxy backed by a buffer.
pub struct BufferProxy<'a> {
    address: Address,
    buffer: &'a [u8],
    /// The last decoded value and the type it was decoded as.
    cache: Option<(Type, (Value, Option<usize>))>,
    handle: &'a ProcessHandle,
    /// Read from the process instead when a read runs past the buffer.
    fallback: bool,
}

impl<'a> BufferProxy<'a> {
    /// Create a proxy for a backing buffer. Reads past its end come up short.
    pub fn new(address: Address, buffer: &'a [u8], handle: &'a ProcessHandle) -> BufferProxy<'a> {
        Self {
            address,
            buffer,
            cache: None,
            handle,
            fallback: false,
        }
    }

    /// Create a proxy for a buffer holding a copy of process memory at
    /// `address`. A read the buffer can't satisfy in full goes to the process,
    /// so it decodes what an [`AddressProxy`] for `address` would.
    pub fn with_fallback(
        address: Address,
        buffer: &'a [u8],
        handle: &'a ProcessHandle,
    ) -> BufferProxy<'a> {
        Self {
            fallback: true,
            ..Self::new(address, buffer, handle)
        }
    }

    fn decode(&self, ty: Type) -> (Value, Option<usize>) {
        // Fast path: a fixed-size value wholly inside the buffer.
        if let Some(size) = ty.size() {
            if let Some(data) = self.buffer.get(..size) {
                return (
                    ty.decode_fixed::<<Process as MemoryReader>::ByteOrder>(data),
                    Some(size),
                );
            }
        }

        let buffer_reader = BufferReader {
            buffer: self.buffer,
            address: self.address,
            fallback: self.fallback.then_some(&self.handle.process),
        };

        return match ty.decode(&buffer_reader, self.address) {
            Ok(value) => value,
            Err(..) => (Value::None, None),
        };

        struct BufferReader<'a> {
            buffer: &'a [u8],
            address: Address,
            fallback: Option<&'a Process>,
        }

        impl MemoryReader for BufferReader<'_> {
            type ByteOrder = <Process as MemoryReader>::ByteOrder;

            fn read_memory(&self, address: Address, buf: &mut [u8]) -> anyhow::Result<usize> {
                let source = usize::try_from(address.offset_of(self.address))
                    .ok()
                    .and_then(|s| self.buffer.get(s..));

                if let Some(process) = self.fallback {
                    if source.is_none_or(|source| source.len() < buf.len()) {
                        return process.read_memory(address, buf);
                    }
                }

                let Some(source) = source else {
                    return Ok(0);
                };

                let e = usize::min(buf.len(), source.len());

                match (buf.get_mut(..e), source.get(..e)) {
                    (Some(buf), Some(source)) => buf.copy_from_slice(source),
                    _ => return Ok(0),
                }

                Ok(e)
            }
        }
    }
}

impl Proxy for BufferProxy<'_> {
    fn address(&mut self) -> anyhow::Result<Option<Address>> {
        Ok(Some(self.address))
    }

    fn eval(&mut self, ty: Type) -> anyhow::Result<(Value, Option<usize>)> {
        if let Some((cached, result)) = &self.cache {
            if *cached == ty {
                return Ok(result.clone());
            }
        }

        let result = self.decode(ty);
        self.cache = Some((ty, result.clone()));
        Ok(result)
    }

    fn handle(&self) -> &ProcessHandle {
        self.handle
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleInfo {
    pub id: u16,
    pub name: String,
    pub file_name: Option<String>,
    pub range: AddressRange,
}

impl fmt::Display for ModuleInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{} ", self.name)?;

        if let Some(file_name) = &self.file_name {
            write!(fmt, "{} ", file_name)?;
        }

        write!(fmt, "{}", self.range)?;
        Ok(())
    }
}

/// A helper struct to give a name to a process.
#[derive(Debug)]
pub struct ProcessName {
    pub id: ProcessId,
    pub name: String,
}

impl fmt::Display for ProcessName {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{} ({})", self.name, self.id)
    }
}

#[derive(Debug)]
pub enum Location<'a> {
    /// Location found in module.
    Module(&'a ModuleInfo),
    /// Location found in thread stack.
    Thread(&'a ProcessThread),
    /// Location not found.
    None,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessThread {
    pub id: ThreadId,
    pub stack: AddressRange,
    pub stack_exit: Option<Address>,
}

impl fmt::Display for ProcessThread {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{} {}", self.id, self.stack)?;

        if let Some(stack_exit) = self.stack_exit {
            write!(fmt, " {}", stack_exit)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::ScanResults;
    use crate::{Address, BitSet, BlockAddresses, Type, Value, Values};

    fn block(addresses: Vec<Address>) -> BlockAddresses {
        BlockAddresses::from_sorted(4, addresses).unwrap()
    }

    #[test]
    fn unchanged_initial_is_materialized_before_last_changes() {
        let mut addresses = Vec::new();
        let mut last = Values::new(Type::U32);

        for n in 0..10u32 {
            addresses.push(Address::new(u64::from(n) * 4));
            last.push(Value::U32(n));
        }

        let mut results = ScanResults::unchanged(block(addresses), last);
        let shared = results.bytes();
        let read = |v: &Values| v.iter().map(|v| v.read()).collect::<Vec<_>>();
        let before = read(results.last());
        assert_eq!(read(results.initial()), before);

        let mut marked = BitSet::new();
        marked.set(0);
        results.remove_marked(&marked).unwrap();
        assert_eq!(results.len(), 9);
        assert_eq!(read(results.initial()), read(results.last()));

        let (_, _, last) = results.split_mut();
        last.iter_mut().next().unwrap().write(Value::U32(100));
        assert!(results.bytes() > shared - 4);
        assert_eq!(results.initial().len(), 9);
        assert_eq!(results.initial().get(0), Some(Value::U32(1)));
        assert_eq!(results.last().get(0), Some(Value::U32(100)));
    }

    #[test]
    fn lower_bound_and_index_of() {
        let mut addresses = Vec::new();
        let mut values = Values::new(Type::U32);

        for n in 1..=50u32 {
            addresses.push(Address::new(u64::from(n) * 16));
            values.push(Value::U32(n));
        }

        let results = ScanResults::unchanged(block(addresses), values);

        assert_eq!(results.lower_bound(Address::new(0)), 0);
        assert_eq!(results.lower_bound(Address::new(16)), 0);
        assert_eq!(results.lower_bound(Address::new(17)), 1);
        assert_eq!(results.lower_bound(Address::new(800)), 49);
        assert_eq!(results.lower_bound(Address::new(801)), 50);

        assert_eq!(results.index_of(Address::new(16)), Some(0));
        assert_eq!(results.index_of(Address::new(320)), Some(19));
        assert_eq!(results.index_of(Address::new(800)), Some(49));
        assert_eq!(results.index_of(Address::new(321)), None);
        assert_eq!(results.index_of(Address::new(8)), None);
        assert_eq!(results.index_of(Address::new(1000)), None);

        let empty = ScanResults::unchanged(BlockAddresses::new(4).unwrap(), Values::new(Type::U32));
        assert_eq!(empty.lower_bound(Address::new(5)), 0);
        assert_eq!(empty.index_of(Address::new(5)), None);
    }

    #[test]
    fn retain_with_checks_and_keeps_pairs() {
        let mut addresses = Vec::new();
        let mut initial = Values::new(Type::U32);
        let mut last = Values::new(Type::U32);

        for n in 0..20u32 {
            addresses.push(Address::new(u64::from(n) * 0x4000));
            initial.push(Value::U32(n));
            last.push(Value::U32(n + 100));
        }

        let mut results = ScanResults::new(block(addresses.clone()), initial, last);
        let mut marked = BitSet::new();

        for index in [0, 3, 4, 5, 19] {
            marked.set(index);
        }

        let kept = (0..20).filter(|i| !marked.test(*i)).collect::<Vec<_>>();
        let kept_addresses = block(kept.iter().map(|&i| addresses[i]).collect());

        // Wrong length or step: nothing changes.
        assert_eq!(
            results.retain_with(block(addresses.clone()), &marked),
            Err(crate::BlockError::Corrupt)
        );
        let other_step =
            BlockAddresses::from_sorted(8, kept.iter().map(|&i| addresses[i])).unwrap();
        assert_eq!(
            results.retain_with(other_step, &marked),
            Err(crate::BlockError::StepMismatch(4, 8))
        );
        assert_eq!(results.len(), 20);
        assert_eq!(results.last().len(), 20);

        results
            .retain_with(kept_addresses.clone(), &marked)
            .unwrap();
        assert_eq!(results.addresses(), &kept_addresses);
        assert_eq!(
            results
                .initial()
                .iter()
                .map(|v| v.read())
                .collect::<Vec<_>>(),
            kept.iter()
                .map(|&i| Value::U32(i as u32))
                .collect::<Vec<_>>()
        );
        assert_eq!(
            results.last().iter().map(|v| v.read()).collect::<Vec<_>>(),
            kept.iter()
                .map(|&i| Value::U32(i as u32 + 100))
                .collect::<Vec<_>>()
        );

        // Nothing marked keeps every value.
        results.retain_with(kept_addresses, &BitSet::new()).unwrap();
        assert_eq!(results.len(), kept.len());
        assert_eq!(results.last().len(), kept.len());
    }

    #[test]
    fn remove_marked_keeps_order() {
        let mut addresses = Vec::new();
        let mut initial = Values::new(Type::U32);
        let mut last = Values::new(Type::U64);

        for n in 0..100u32 {
            addresses.push(Address::new(u64::from(n) * 8));
            initial.push(Value::U32(n));
            last.push(Value::U64(u64::from(n) * 2));
        }

        let mut results = ScanResults::new(block(addresses), initial, last);

        results.remove_marked(&BitSet::new()).unwrap();
        assert_eq!(results.len(), 100);

        let mut marked = BitSet::new();

        for index in (0..100).filter(|n| n % 7 == 0 || *n == 99) {
            marked.set(index);
        }

        results.remove_marked(&marked).unwrap();

        let kept = (0..100u32)
            .filter(|n| n % 7 != 0 && *n != 99)
            .collect::<Vec<_>>();
        assert_eq!(results.len(), kept.len());
        assert_eq!(
            results.addresses().iter().collect::<Vec<_>>(),
            kept.iter()
                .map(|&n| Address::new(u64::from(n) * 8))
                .collect::<Vec<_>>()
        );
        assert_eq!(
            results
                .initial()
                .iter()
                .map(|v| v.read())
                .collect::<Vec<_>>(),
            kept.iter().map(|&n| Value::U32(n)).collect::<Vec<_>>()
        );
        assert_eq!(
            results.last().iter().map(|v| v.read()).collect::<Vec<_>>(),
            kept.iter()
                .map(|&n| Value::U64(u64::from(n) * 2))
                .collect::<Vec<_>>()
        );
    }
}
