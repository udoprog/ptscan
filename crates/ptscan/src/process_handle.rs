//! High-level interface to processes.

use crate::{
    error::Error, filter_expr::FilterExpr, progress_reporter::ProgressReporter, system,
    thread::Thread, utils::IteratorExtension as _, Address, AddressRange, Addresses, Base, Cached,
    DefaultScanner, FollowablePointer, MemoryInformation, MemoryReader, Pointer, PointerInfo,
    PointerWidth, PortableBase, Process, ProcessId, ProcessInfo, Scanner, Size, Test, ThreadId,
    Token, Type, TypeHint, Value, ValueExpr, ValueRef, Values,
};
use anyhow::{anyhow, bail, Context as _};
use crossbeam_queue::SegQueue;
use dynamicvec::DynamicConverter;
use hashbrown::{HashMap, HashSet};
use serde::{Deserialize, Serialize};
use std::convert::{TryFrom as _, TryInto as _};
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::sync::mpsc;

use winapi::shared::winerror;

#[derive(Debug, Clone, Default)]
pub struct InitialScanConfig {
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
            Err(e) => match e.raw_os_error().map(|n| n as u32) {
                // NB: process no longer exists.
                Some(winerror::ERROR_INVALID_PARAMETER) => return Ok(None),
                // NB: we don't have access to the process.
                Some(winerror::ERROR_ACCESS_DENIED) => return Ok(None),
                _ => return Err(e),
            },
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

        for t in system::threads()? {
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

            let stack = thread
                .thread_stack(process)
                .with_context(|| Error::ThreadStack(thread.thread_id()))?;

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
            cache: Default::default(),
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
            cache: Default::default(),
        }
    }

    pub fn rescan_values(
        &self,
        thread_pool: &rayon::ThreadPool,
        addresses: &mut Addresses,
        initial: &mut Values,
        values: &mut Values,
        cancel: Option<&Token>,
        progress: (impl ScanProgress + Send),
        filter: &FilterExpr,
    ) -> anyhow::Result<()> {
        if addresses.len() < 100_000 {
            self.rescan_small(
                thread_pool,
                addresses,
                initial,
                values,
                cancel,
                progress,
                filter,
            )
        } else {
            self.rescan_large(
                thread_pool,
                addresses,
                initial,
                values,
                cancel,
                progress,
                filter,
            )
        }
    }

    /// Read the value of many memory locations.
    pub fn rescan_large(
        &self,
        thread_pool: &rayon::ThreadPool,
        addresses: &mut Addresses,
        initial: &mut Values,
        values: &mut Values,
        cancel: Option<&Token>,
        progress: (impl ScanProgress + Send),
        filter: &FilterExpr,
    ) -> anyhow::Result<()> {
        // Check every 100_000 addresses scanned.
        const CHECK_INTERVAL: u64 = 100_000;

        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        if addresses.is_empty() {
            return Ok(());
        }

        if addresses.len() != values.len() {
            return Err(anyhow!(
                "argument length mismatch, addresses ({}) != values ({})",
                addresses.len(),
                values.len()
            ));
        }

        // how many tasks to run in parallel.
        let mut tasks = thread_pool.current_num_threads();

        let mut to_remove = Vec::new();

        let filter = filter.type_check(self, initial.ty(), values.ty(), values.ty())?;
        let filter = &filter;

        let mut ranges = Vec::new();

        {
            let thread_stacks = self
                .threads
                .iter()
                .map(|t| t.stack.clone())
                .collect::<HashSet<_>>();

            for m in self.process.virtual_memory_regions().only_relevant() {
                let m = m?;

                if !thread_stacks.contains(&m.range) {
                    ranges.push(m.range);
                }
            }

            ranges.extend(self.modules.modules.iter().map(|m| m.range));
        }

        // Divide up into partitions.
        let mut partitions = ranges
            .iter()
            .map(|_| Vec::with_capacity(0x10))
            .collect::<Vec<_>>();

        for (index, address) in addresses.iter_copied().enumerate() {
            match AddressRange::find_index_in_range(&ranges, |r| r, address)
                .and_then(|i| partitions.get_mut(i))
            {
                Some(p) => p.push(index),
                None => to_remove.push(index),
            }
        }

        let mut last_error = None;
        let mut reporter = ProgressReporter::new(
            progress,
            addresses.len() - to_remove.len(),
            cancel,
            &mut last_error,
        );

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel::<anyhow::Result<Task>>();
                let (p_tx, p_rx) =
                    crossbeam_channel::unbounded::<Option<(&AddressRange, Vec<usize>)>>();

                for _ in 0..tasks {
                    let tx = tx.clone();
                    let p_rx = p_rx.clone();

                    let addresses = &*addresses;
                    let initial = &*initial;
                    let values = &*values;

                    s.spawn(move |_| {
                        while !cancel.test() {
                            let (range, task) = match p_rx.recv().expect("failed to receive") {
                                Some(task) => task,
                                None => break,
                            };

                            let work = |tx: &mpsc::Sender<anyhow::Result<Task>>| {
                                let address = range.base;
                                let size = range.size.as_usize();
                                let mut buffer = vec![0u8; size];
                                let read =
                                    self.process.read_process_memory(address, &mut buffer)?;
                                let buffer = &buffer[..read];

                                let mut accepted = 0u64;
                                let mut rejected = Vec::new();

                                let mut count = 0u64;

                                for index in task {
                                    if count > CHECK_INTERVAL {
                                        if cancel.test() {
                                            break;
                                        }

                                        tx.send(Ok(Task::Progress { count, accepted }))
                                            .expect("failed to send progress");
                                        count = 0;
                                        accepted = 0;
                                    }

                                    count += 1;

                                    let address = addresses.get(index).expect("missing address");
                                    let initial =
                                        initial.get(index).expect("missing initial value");
                                    let mut value = unsafe { values.get_mutator_unsafe(index) }
                                        .expect("missing value");

                                    let offset =
                                        match usize::try_from(address.offset_of(range.base)) {
                                            Ok(offset) => offset,
                                            Err(..) => {
                                                rejected.push(index);
                                                continue;
                                            }
                                        };

                                    if offset >= buffer.len() {
                                        rejected.push(index);
                                        continue;
                                    }

                                    let buffer = &buffer[offset..];
                                    let mut proxy = BufferProxy::new(address, buffer, self);

                                    if let Test::True = filter.test(
                                        initial.as_ref(),
                                        value.read().as_ref(),
                                        &mut proxy,
                                    )? {
                                        accepted += 1;
                                        value.write(proxy.eval(value.ty)?.0);
                                    } else {
                                        rejected.push(index);
                                    }
                                }

                                Ok(Task::Result {
                                    count,
                                    accepted,
                                    rejected,
                                })
                            };

                            let result = work(&tx);
                            tx.send(result).expect("failed to send");
                        }

                        tx.send(Ok(Task::Done)).expect("failed to send");
                    });
                }

                let mut it = partitions
                    .into_iter()
                    .enumerate()
                    .filter(|(_, p)| !p.is_empty());

                for (index, task) in (&mut it).take(tasks) {
                    p_tx.send(Some((&ranges[index], task)))
                        .expect("failed to send");
                }

                let mut results = 0u64;

                while tasks > 0 {
                    let result = rx.recv().expect("closed");

                    if let Some((index, task)) = it.next() {
                        p_tx.send(Some((&ranges[index], task)))
                            .expect("failed to send");
                    } else {
                        p_tx.send(None).expect("failed to send");
                    }

                    if let Some(result) = reporter.eval(result) {
                        match result {
                            Task::Result {
                                count,
                                accepted,
                                mut rejected,
                            } => {
                                results += accepted;
                                reporter.tick_n(count as usize, results);
                                to_remove.append(&mut rejected);
                            }
                            Task::Progress { count, accepted } => {
                                results += accepted;
                                reporter.tick_n(count as usize, results);
                            }
                            Task::Done => {
                                tasks -= 1;
                                continue;
                            }
                        }
                    }
                }

                Ok::<_, anyhow::Error>(())
            })?;

            Ok::<_, anyhow::Error>(())
        })?;

        to_remove.sort_by(|a, b| b.cmp(a));

        for index in to_remove {
            initial.swap_remove(index);
            values.swap_remove(index);
            addresses.swap_remove(index);
        }

        if let Some(e) = last_error {
            return Err(e);
        }

        return Ok(());

        enum Task {
            /// Address was accepted.
            Result {
                count: u64,
                accepted: u64,
                rejected: Vec<usize>,
            },
            Progress {
                count: u64,
                accepted: u64,
            },
            /// Task is done.
            Done,
        }
    }

    fn rescan_small(
        &self,
        thread_pool: &rayon::ThreadPool,
        addresses: &mut Addresses,
        initial: &mut Values,
        values: &mut Values,
        cancel: Option<&Token>,
        progress: (impl ScanProgress + Send),
        filter: &FilterExpr,
    ) -> anyhow::Result<()> {
        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        if addresses.is_empty() {
            return Ok(());
        }

        if addresses.len() != values.len() {
            return Err(anyhow!(
                "argument length mismatch, addresses ({}) != values ({})",
                addresses.len(),
                values.len()
            ));
        }

        let mut last_error = None;
        let mut reporter = ProgressReporter::new(progress, values.len(), cancel, &mut last_error);

        // how many tasks to run in parallel.
        let mut tasks = thread_pool.current_num_threads();

        let mut to_remove = Vec::new();

        let filter = filter.type_check(self, initial.ty(), values.ty(), values.ty())?;
        let filter = &filter;

        let mut ranges = Vec::new();

        {
            let thread_stacks = self
                .threads
                .iter()
                .map(|t| t.stack.clone())
                .collect::<HashSet<_>>();

            for m in self.process.virtual_memory_regions().only_relevant() {
                let m = m?;

                if !thread_stacks.contains(&m.range) {
                    ranges.push(m.range);
                }
            }

            ranges.extend(self.modules.modules.iter().map(|m| m.range));
        }

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel::<anyhow::Result<Task>>();
                let (p_tx, p_rx) = crossbeam_channel::unbounded::<
                    Option<(
                        usize,
                        Address,
                        dynamicvec::Accessor<'_, Type>,
                        dynamicvec::Mutator<'_, Type>,
                    )>,
                >();

                for _ in 0..tasks {
                    let tx = tx.clone();
                    let p_rx = p_rx.clone();

                    s.spawn(move |_| {
                        while !cancel.test() {
                            let (index, address, initial, mut value) =
                                match p_rx.recv().expect("failed to receive") {
                                    Some(task) => task,
                                    None => break,
                                };

                            let mut work = || {
                                let mut proxy = self.address_proxy(&address);

                                if let Test::True = filter.test(
                                    initial.read().as_ref(),
                                    value.read().as_ref(),
                                    &mut proxy,
                                )? {
                                    value.write(proxy.eval(value.ty)?.0);
                                    return Ok(Task::Accepted);
                                }

                                Ok(Task::Rejected(index))
                            };

                            tx.send(work()).expect("failed to send");
                        }

                        tx.send(Ok(Task::Done)).expect("failed to send");
                    });
                }

                let mut it = addresses
                    .iter_copied()
                    .zip(initial.iter().zip(values.iter_mut()))
                    .enumerate();

                for (index, (address, (initial, value))) in (&mut it).take(tasks) {
                    p_tx.send(Some((index, address, initial, value)))
                        .expect("failed to send");
                }

                let mut count = 0u64;

                while tasks > 0 {
                    let result = rx.recv().expect("closed");

                    if let Some((index, (address, (initial, value)))) = it.next() {
                        p_tx.send(Some((index, address, initial, value)))
                            .expect("failed to send");
                    } else {
                        p_tx.send(None).expect("failed to send");
                    }

                    if let Some(result) = reporter.eval(result) {
                        count += match result {
                            Task::Accepted => 1,
                            Task::Rejected(index) => {
                                to_remove.push(index);
                                0
                            }
                            Task::Done => {
                                tasks -= 1;
                                continue;
                            }
                        }
                    }

                    reporter.tick(count);
                }

                Ok::<_, anyhow::Error>(())
            })?;

            Ok::<_, anyhow::Error>(())
        })?;

        to_remove.sort_by(|a, b| b.cmp(a));

        for index in to_remove {
            initial.swap_remove(index);
            values.swap_remove(index);
            addresses.swap_remove(index);
        }

        if let Some(e) = last_error {
            return Err(e);
        }

        return Ok(());

        enum Task {
            /// Address was accepted.
            Accepted,
            /// Address was rejected.
            Rejected(usize),
            /// Task is done.
            Done,
        }
    }

    /// Refresh a given set of existing values without a filter.
    pub fn refresh_values(
        &self,
        thread_pool: &rayon::ThreadPool,
        addresses: &Addresses,
        values: &mut Values,
        cancel: Option<&Token>,
        progress: (impl ScanProgress + Send),
        expr: &ValueExpr,
    ) -> anyhow::Result<()> {
        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        if addresses.is_empty() {
            return Ok(());
        }

        let mut last_error = None;
        let mut reporter =
            ProgressReporter::new(progress, addresses.len(), cancel, &mut last_error);

        // how many tasks to run in parallel.
        let mut tasks = thread_pool.current_num_threads();
        let cancel = &cancel;

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

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel::<anyhow::Result<bool>>();
                let (p_tx, p_rx) = crossbeam_channel::unbounded::<
                    Option<(Address, dynamicvec::Mutator<'_, Type>)>,
                >();

                for _ in 0..tasks {
                    let tx = tx.clone();
                    let p_rx = p_rx.clone();

                    s.spawn(move |_| {
                        while !cancel.test() {
                            let (pointer, mut value) = match p_rx.recv().expect("closed") {
                                Some(task) => task,
                                None => {
                                    break;
                                }
                            };

                            let mut work = move || {
                                let mut proxy = self.address_proxy(&pointer);

                                let new_value =
                                    expr.eval(ValueRef::None, value.read().as_ref(), &mut proxy)?;

                                value.write(new_value);
                                Ok(false)
                            };

                            tx.send(work()).expect("failed to send");
                        }

                        tx.send(Ok(true)).expect("failed to send");
                    });
                }

                let mut it = addresses.iter_copied().zip(values.iter_mut());

                for (base, value) in (&mut it).take(tasks) {
                    p_tx.send(Some((base, value))).expect("closed");
                }

                let mut count = 0u64;

                while tasks > 0 {
                    let res = rx.recv().expect("failed to receive on channel");

                    if let Some((base, value)) = it.next() {
                        p_tx.send(Some((base, value)))
                            .expect("failed to send on channel");
                    } else {
                        p_tx.send(None).expect("failed to send on channel");
                    }

                    if reporter.eval(res).unwrap_or_default() {
                        tasks -= 1;
                        continue;
                    }

                    count += 1;
                    reporter.tick(count);
                }
            });
        });

        if let Some(e) = last_error {
            return Err(e);
        }

        Ok(())
    }

    /// Refresh a dynamic collection of values in-place.
    pub fn refresh_dynamic_values<V>(
        &self,
        thread_pool: &rayon::ThreadPool,
        values: &mut [V],
        cancel: Option<&Token>,
        progress: (impl ScanProgress + Send),
        expr: &ValueExpr,
    ) -> anyhow::Result<()>
    where
        V: Send + ValueHolder,
    {
        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        if values.is_empty() {
            return Ok(());
        }

        let mut last_error = None;
        let mut reporter = ProgressReporter::new(progress, values.len(), cancel, &mut last_error);

        // how many tasks to run in parallel.
        let mut tasks = thread_pool.current_num_threads();
        let cancel = &cancel;

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel::<anyhow::Result<bool>>();
                let (p_tx, p_rx) = crossbeam_channel::unbounded::<Option<&mut V>>();

                for _ in 0..tasks {
                    let tx = tx.clone();
                    let p_rx = p_rx.clone();

                    s.spawn(move |_| {
                        while !cancel.test() {
                            let value = match p_rx.recv().expect("closed") {
                                Some(task) => task,
                                None => {
                                    break;
                                }
                            };

                            let mut work = move || {
                                let new_value = {
                                    let value_type = value.value_type();

                                    let pointer = value.pointer();
                                    let mut proxy = self.address_proxy(pointer);

                                    let expr_type = expr
                                        .type_of(
                                            self,
                                            TypeHint::Explicit(Type::None),
                                            TypeHint::Explicit(Type::None),
                                            TypeHint::Explicit(value_type),
                                            TypeHint::NoHint,
                                        )?
                                        .ok_or_else(|| Error::TypeInference(expr.clone()))?;

                                    expr.type_check(
                                        self,
                                        Type::None,
                                        Type::None,
                                        value_type,
                                        expr_type,
                                    )?
                                    .eval(
                                        ValueRef::None,
                                        ValueRef::None,
                                        &mut proxy,
                                    )?
                                };

                                value.insert(new_value);
                                Ok(false)
                            };

                            tx.send(work()).expect("failed to send");
                        }

                        tx.send(Ok(true)).expect("failed to send");
                    });
                }

                let mut it = values.iter_mut();

                for value in (&mut it).take(tasks) {
                    p_tx.send(Some(value)).expect("closed");
                }

                let mut count = 0u64;

                while tasks > 0 {
                    let res = rx.recv().expect("failed to receive on channel");

                    if let Some(value) = it.next() {
                        p_tx.send(Some(value)).expect("failed to send on channel");
                    } else {
                        p_tx.send(None).expect("failed to send on channel");
                    }

                    if reporter.eval(res).unwrap_or_default() {
                        tasks -= 1;
                        continue;
                    }

                    count += 1;
                    reporter.tick(count);
                }
            });
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
    pub fn initial_scan(
        &self,
        thread_pool: &rayon::ThreadPool,
        filter: &FilterExpr,
        addresses: &mut Addresses,
        values: &mut Values,
        cancel: Option<&Token>,
        progress: (impl ScanProgress + Send),
        config: InitialScanConfig,
    ) -> anyhow::Result<()> {
        const DEFAULT_BUFFER_SIZE: usize = 0x100_000;

        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

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
            .or_else(|| if aligned { Some(alignment) } else { None });

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

        let mut ranges = Vec::new();

        {
            let thread_stacks = self
                .threads
                .iter()
                .map(|t| t.stack.clone())
                .collect::<HashSet<_>>();

            if config.modules_only {
                ranges.extend(self.modules.modules.iter().map(|m| m.range));
            } else {
                for m in self.process.virtual_memory_regions().only_relevant() {
                    let m = m?;

                    if !thread_stacks.contains(&m.range) {
                        ranges.push(m.range);
                    }
                }

                ranges.extend(self.modules.modules.iter().map(|m| m.range));
            }
        }

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

                queue.push((address, len));
                offset += len;
            }
        }

        let queue = &queue;
        let pointer_width = self.process.pointer_width;

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
                        Task::Done(result, ..) => {
                            if let Some((mut a, mut r)) = reporter.eval(result) {
                                addresses.append(&mut a);
                                values.append(&mut r);
                            }

                            task_count -= 1;

                            if task_count == 0 {
                                break;
                            }
                        }
                        Task::Tick(bytes, c, ..) => {
                            hits += c;
                            reporter.tick_n(bytes, hits);
                        }
                    }
                }

                Ok::<(), anyhow::Error>(())
            })
        })?;

        if let Some(e) = last_error {
            return Err(e);
        }

        return Ok(());

        enum Task {
            Done(anyhow::Result<(Addresses, Values)>),
            Tick(usize, u64),
        }

        struct Work<'a> {
            handle: &'a ProcessHandle,
            tx: &'a mpsc::Sender<Task>,
            queue: &'a SegQueue<(Address, usize)>,
            value_type: Type,
            buffer_size: usize,
            cancel: &'a Token,
            scanner: &'a dyn Scanner,
            pointer_width: PointerWidth,
        }

        #[inline(always)]
        fn work(work: Work<'_>) -> anyhow::Result<(Addresses, Values)> {
            let Work {
                handle,
                tx,
                queue,
                value_type,
                buffer_size,
                cancel,
                scanner,
                pointer_width,
            } = work;

            let mut addresses = Addresses::new(pointer_width);
            let mut values = Values::new(value_type);

            let mut buffer = vec![0u8; buffer_size];

            while let Ok((address, len)) = queue.pop() {
                if cancel.test() {
                    return Ok((addresses, values));
                }

                let len = usize::min(buffer.len(), len);
                let data = &mut buffer[..len];

                let mut hits = 0;

                let len = handle.process.read_process_memory(address, data)?;

                if len == 0 {
                    continue;
                }

                let data = &data[..len];

                scanner.scan(
                    address,
                    handle,
                    data,
                    &mut addresses,
                    &mut values,
                    &mut hits,
                    cancel,
                )?;

                tx.send(Task::Tick(data.len(), hits))
                    .expect("send tick failed");
            }

            Ok((addresses, values))
        }
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
    /// Cached value of different types.
    cache: HashMap<Type, (Value, Option<usize>)>,
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
        if let Some(result) = self.cache.get(&ty).cloned() {
            return Ok(result);
        }

        let address = match self.address()? {
            Some(address) => address,
            None => {
                self.cache.insert(ty, (Value::None, None));
                return Ok((Value::None, None));
            }
        };

        let result = match ty.decode(&self.handle.process, address) {
            Ok(value) => value,
            Err(..) => (Value::None, None),
        };

        self.cache.insert(ty, result.clone());
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
    /// Cached value of different types.
    cache: HashMap<Type, (Value, Option<usize>)>,
    handle: &'a ProcessHandle,
}

impl<'a> BufferProxy<'a> {
    /// Create a proxy for a backing buffer.
    pub fn new(address: Address, buffer: &'a [u8], handle: &'a ProcessHandle) -> BufferProxy<'a> {
        Self {
            address,
            buffer,
            cache: HashMap::new(),
            handle,
        }
    }
}

impl Proxy for BufferProxy<'_> {
    fn address(&mut self) -> anyhow::Result<Option<Address>> {
        Ok(Some(self.address))
    }

    fn eval(&mut self, ty: Type) -> anyhow::Result<(Value, Option<usize>)> {
        if let Some(result) = self.cache.get(&ty).cloned() {
            return Ok(result);
        }

        let buffer_reader = BufferReader {
            buffer: &self.buffer,
            address: self.address,
        };

        let result = match ty.decode(&buffer_reader, self.address) {
            Ok(value) => value,
            Err(..) => (Value::None, None),
        };

        self.cache.insert(ty, result.clone());
        return Ok(result);

        struct BufferReader<'a> {
            buffer: &'a [u8],
            address: Address,
        }

        impl MemoryReader for BufferReader<'_> {
            type ByteOrder = byteorder::NativeEndian;

            fn read_memory(&self, address: Address, buf: &mut [u8]) -> anyhow::Result<usize> {
                let s = match usize::try_from(address.offset_of(self.address)) {
                    Ok(s) => s,
                    Err(..) => return Ok(0),
                };

                if s >= self.buffer.len() {
                    return Ok(0);
                }

                let source = &self.buffer[s..];
                let e = usize::min(buf.len(), source.len());
                buf[..e].copy_from_slice(&source[..e]);
                Ok(e)
            }
        }
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
