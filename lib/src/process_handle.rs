//! High-level interface to processes.

use crate::{
    error::Error,
    filter_expr::FilterExpr,
    pointer::FollowablePointer,
    process::{MemoryInformation, MemoryReader},
    progress_reporter::ProgressReporter,
    system,
    thread::Thread,
    values, Address, AddressRange, Base, Cached, Pointer, PointerWidth, PortableBase, Process,
    ProcessId, ProcessInfo, Size, Special, Test, ThreadId, Token, Type, TypeHint, TypedFilterExpr,
    Value, ValueExpr, ValueRef, Values,
};
use anyhow::{anyhow, bail, Context as _};
use crossbeam_queue::SegQueue;
use hashbrown::{HashMap, HashSet};
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::{
    convert::{TryFrom as _, TryInto as _},
    ffi::{OsStr, OsString},
    fmt, iter,
    sync::mpsc,
    time::{Duration, Instant},
};

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
            let ptr = process.decode_pointer(w);

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
            memory_cache: None,
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
            memory_cache: None,
            followed: Cached::None,
            cache: Default::default(),
        }
    }

    /// Create a new cached session.
    pub fn session(&self) -> anyhow::Result<Session<'_>> {
        use crate::utils::IteratorExtension as _;

        let regions = self
            .process
            .virtual_memory_regions()
            .only_relevant()
            .collect::<anyhow::Result<_>>()?;

        Ok(Session {
            handle: self,
            memory_cache: MemoryCache::new(regions),
        })
    }

    /// Read the value of many memory locations.
    pub fn rescan_values(
        &self,
        thread_pool: &rayon::ThreadPool,
        bases: &mut Vec<Base>,
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

        if bases.is_empty() {
            return Ok(());
        }

        if bases.len() != values.len() {
            return Err(anyhow!(
                "argument length mismatch, bases ({}) != values ({})",
                bases.len(),
                values.len()
            ));
        }

        let mut last_error = None;
        let mut reporter = ProgressReporter::new(progress, values.len(), cancel, &mut last_error);

        // how many tasks to run in parallel.
        let mut tasks = thread_pool.current_num_threads();

        let mut to_remove = Vec::new();

        let filter = filter.type_check(initial.ty, values.ty, values.ty)?;
        let filter = &filter;

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel::<anyhow::Result<Task>>();
                let (p_tx, p_rx) = crossbeam_channel::unbounded::<
                    Option<(usize, &Base, values::Accessor<'_>, values::Mutator<'_>)>,
                >();

                for _ in 0..tasks {
                    let tx = tx.clone();
                    let p_rx = p_rx.clone();

                    s.spawn(move |_| {
                        while !cancel.test() {
                            let (index, base, initial, mut value) =
                                match p_rx.recv().expect("failed to receive") {
                                    Some(task) => task,
                                    None => break,
                                };

                            let mut work = || {
                                let mut proxy = self.address_proxy(base);

                                if let Test::True =
                                    filter.test(initial.as_ref(), value.as_ref(), &mut proxy)?
                                {
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

                let mut it = bases
                    .iter()
                    .zip(initial.iter().zip(values.iter_mut()))
                    .enumerate();

                for (index, (base, (initial, value))) in (&mut it).take(tasks) {
                    p_tx.send(Some((index, base, initial, value)))
                        .expect("failed to send");
                }

                let mut count = 0u64;

                while tasks > 0 {
                    let result = rx.recv().expect("closed");

                    if let Some((index, (base, (initial, value)))) = it.next() {
                        p_tx.send(Some((index, base, initial, value)))
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
            bases.swap_remove(index);
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
        bases: &[Base],
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

        if bases.is_empty() {
            return Ok(());
        }

        let mut last_error = None;
        let mut reporter = ProgressReporter::new(progress, bases.len(), cancel, &mut last_error);

        // how many tasks to run in parallel.
        let mut tasks = thread_pool.current_num_threads();
        let cancel = &cancel;

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel::<anyhow::Result<bool>>();
                let (p_tx, p_rx) =
                    crossbeam_channel::unbounded::<Option<(&Base, values::Mutator<'_>)>>();

                for _ in 0..tasks {
                    let tx = tx.clone();
                    let p_rx = p_rx.clone();

                    s.spawn(move |_| {
                        while !cancel.test() {
                            let (base, mut value) = match p_rx.recv().expect("closed") {
                                Some(task) => task,
                                None => {
                                    break;
                                }
                            };

                            let mut work = move || {
                                let mut proxy = self.address_proxy(base);

                                let expr_type = expr
                                    .type_of(
                                        TypeHint::Explicit(Type::None),
                                        TypeHint::Explicit(Type::None),
                                        TypeHint::Explicit(value.ty),
                                        TypeHint::NoHint,
                                    )?
                                    .ok_or_else(|| Error::TypeInference(expr.clone()))?;

                                let new_value = expr
                                    .type_check(Type::None, Type::None, value.ty, expr_type)?
                                    .eval(ValueRef::None, value.as_ref(), &mut proxy)?;

                                value.write(new_value);
                                Ok(false)
                            };

                            tx.send(work()).expect("failed to send");
                        }

                        tx.send(Ok(true)).expect("failed to send");
                    });
                }

                let mut it = bases.iter().zip(values.iter_mut());

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
                                            TypeHint::Explicit(Type::None),
                                            TypeHint::Explicit(Type::None),
                                            TypeHint::Explicit(value_type),
                                            TypeHint::NoHint,
                                        )?
                                        .ok_or_else(|| Error::TypeInference(expr.clone()))?;

                                    expr.type_check(Type::None, Type::None, value_type, expr_type)?
                                        .eval(ValueRef::None, ValueRef::None, &mut proxy)?
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
        bases: &mut Vec<Base>,
        values: &mut Values,
        cancel: Option<&Token>,
        progress: (impl ScanProgress + Send),
        config: InitialScanConfig,
    ) -> anyhow::Result<()> {
        const DEFAULT_BUFFER_SIZE: usize = 0x100_000;

        let handle = self;

        use crate::utils::IteratorExtension;

        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        let mut last_error = None;

        let value_type = values.ty;
        let aligned = config
            .aligned
            .unwrap_or_else(|| values.ty.is_default_aligned());
        let alignment = values
            .ty
            .alignment(&handle.process)
            .ok_or_else(|| anyhow!("cannot scan for none"))?;
        let step_size = if aligned { alignment } else { 1 };
        let alignment = config
            .alignment
            .or_else(|| if aligned { Some(alignment) } else { None });

        let special = filter.special(&handle.process, values.ty)?;
        let special = special.as_ref();

        let filter = filter.type_check(Type::None, Type::None, values.ty)?;
        let filter = &filter;

        let tasks = config
            .tasks
            .unwrap_or_else(|| thread_pool.current_num_threads());

        let buffer_size = config.buffer_size.unwrap_or(DEFAULT_BUFFER_SIZE);

        let mut bytes = Size::zero();

        let mut ranges = Vec::new();

        {
            let thread_stacks = handle
                .threads
                .iter()
                .map(|t| t.stack.clone())
                .collect::<HashSet<_>>();

            if config.modules_only {
                ranges.extend(handle.modules.modules.iter().map(|m| m.range));
            } else {
                for m in handle.process.virtual_memory_regions().only_relevant() {
                    let m = m?;

                    if !thread_stacks.contains(&m.range) {
                        ranges.push(m.range);
                    }
                }

                ranges.extend(handle.modules.modules.iter().map(|m| m.range));
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

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel::<Task>();

                for _ in 0..tasks {
                    let tx = tx.clone();

                    s.spawn(move |_| {
                        let start = Instant::now();
                        let result = work(Work {
                            handle,
                            tx: &tx,
                            queue,
                            filter,
                            value_type,
                            special,
                            alignment,
                            step_size,
                            buffer_size,
                            cancel,
                        });

                        let now = Instant::now();
                        let duration = now.duration_since(start);

                        tx.send(Task::Done(result, duration, now))
                            .expect("send done failed");
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
                                bases.append(&mut a);
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
            Done(anyhow::Result<(Vec<Base>, Values)>, Duration, Instant),
            Tick(usize, u64, Duration, Instant),
        }

        struct Work<'a, 'filter> {
            handle: &'a ProcessHandle,
            tx: &'a mpsc::Sender<Task>,
            queue: &'a SegQueue<(Address, usize)>,
            filter: &'filter TypedFilterExpr<'a>,
            value_type: Type,
            special: Option<&'a Special>,
            alignment: Option<usize>,
            step_size: usize,
            buffer_size: usize,
            cancel: &'a Token,
        }

        #[inline(always)]
        fn work(work: Work<'_, '_>) -> anyhow::Result<(Vec<Base>, Values)> {
            let Work {
                handle,
                tx,
                queue,
                filter,
                value_type,
                special,
                alignment,
                step_size,
                buffer_size,
                cancel,
            } = work;

            let mut bases = Vec::new();
            let mut values = Values::new(value_type, &handle.process);

            let mut buffer = vec![0u8; buffer_size];

            while let Ok((address, len)) = queue.pop() {
                if cancel.test() {
                    return Ok((bases, values));
                }

                let len = usize::min(buffer.len(), len);
                let data = &mut buffer[..len];

                let start = Instant::now();
                let mut hits = 0;

                let len = handle.process.read_process_memory(address, data)?;

                if len == 0 {
                    continue;
                }

                let data = &data[..len];

                process_one(ProcessOne {
                    handle,
                    filter,
                    value_type,
                    bases: &mut bases,
                    values: &mut values,
                    hits: &mut hits,
                    base: address,
                    data,
                    step_size,
                    alignment,
                    special,
                    cancel,
                })?;

                let duration = Instant::now().duration_since(start);
                tx.send(Task::Tick(data.len(), hits, duration, Instant::now()))
                    .expect("send tick failed");
            }

            Ok((bases, values))
        };

        struct ProcessOne<'a, 'filter> {
            handle: &'a ProcessHandle,
            filter: &'filter TypedFilterExpr<'a>,
            value_type: Type,
            bases: &'a mut Vec<Base>,
            values: &'a mut Values,
            hits: &'a mut u64,
            base: Address,
            data: &'a [u8],
            step_size: usize,
            alignment: Option<usize>,
            special: Option<&'a Special>,
            cancel: &'a Token,
        }

        #[inline(always)]
        fn process_one(process_one: ProcessOne<'_, '_>) -> anyhow::Result<()> {
            let ProcessOne {
                handle,
                filter,
                value_type,
                bases,
                values,
                hits,
                base,
                data,
                step_size,
                alignment,
                special,
                cancel,
                ..
            } = process_one;

            let mut offset = match special {
                Some(special) => match special.test(data) {
                    Some(offset) => offset,
                    None => return Ok(()),
                },
                None => 0usize,
            };

            if let Some(size) = alignment {
                align(&mut offset, size);
            }

            let mut last_address = None;

            while offset < data.len() && !cancel.test() {
                let address = match base.checked_add(offset.try_into()?) {
                    Some(address) => address,
                    None => bail!("base `{}` + offset `{}` out of range", base, offset),
                };

                let pointer = Pointer::from(address);
                let mut proxy = handle.address_proxy(&pointer);

                // sanity check
                if let Some(last_address) = last_address {
                    if last_address >= address {
                        bail!(
                            "BUG: address did not increase during scan: {} -> {}",
                            last_address,
                            address
                        )
                    }
                }

                last_address = Some(address);

                if let Test::True = filter.test(ValueRef::None, ValueRef::None, &mut proxy)? {
                    *hits += 1;
                    let (value, advance) = proxy.eval(value_type)?;

                    let base = handle.address_to_base(address);

                    bases.push(base);
                    values.push(value);

                    if let Some(advance) = advance {
                        if advance == 0 {
                            bail!("BUG: attempt to advance by 0 bytes");
                        }

                        offset += advance;
                    } else {
                        offset += step_size;
                    }
                } else {
                    offset += step_size;
                }

                if offset >= data.len() {
                    break;
                }

                if let Some(special) = special {
                    offset += match special.test(&data[offset..]) {
                        Some(o) => o,
                        None => return Ok(()),
                    };

                    if let Some(size) = alignment {
                        align(&mut offset, size);
                    }
                }
            }

            Ok(())
        }

        fn align(to_align: &mut usize, alignment: usize) {
            let rem = *to_align % alignment;

            if rem > 0 {
                *to_align += alignment - rem;
            }
        }
    }
}

// NB: forward impl to inner `Process`.
impl ProcessInfo for ProcessHandle {
    type Process = Process;
    type ByteOrder = byteorder::NativeEndian;

    fn process(&self) -> &Self::Process {
        &self.process
    }

    fn pointer_width(&self) -> PointerWidth {
        ProcessInfo::pointer_width(&self.process)
    }

    fn virtual_query(&self, address: Address) -> anyhow::Result<Option<MemoryInformation>> {
        self.process.virtual_query(address)
    }
}

#[derive(Debug, Clone)]
pub struct AddressProxy<'a, P>
where
    P: FollowablePointer,
{
    pub pointer: &'a P,
    pub(crate) handle: &'a ProcessHandle,
    memory_cache: Option<&'a MemoryCache>,
    /// cached, followed address.
    pub followed: Cached<Option<Address>>,
    /// Cached value of different types.
    cache: HashMap<Type, (Value, Option<usize>)>,
}

impl<P> AddressProxy<'_, P>
where
    P: FollowablePointer,
{
    pub fn address(&mut self) -> anyhow::Result<Option<Address>> {
        Ok(if let Some(memory_cache) = self.memory_cache {
            match self.followed {
                Cached::Some(address) => address,
                Cached::None => {
                    let address = self.pointer.follow(self.handle, |a, buf| {
                        memory_cache.read_process_memory(&self.handle.process, a, buf)
                    })?;

                    self.followed = Cached::Some(address);
                    address
                }
            }
        } else {
            match self.followed {
                Cached::Some(address) => address,
                Cached::None => {
                    let address = self.pointer.follow_default(self.handle)?;
                    self.followed = Cached::Some(address);
                    address
                }
            }
        })
    }

    /// Evaluate the pointer of the proxy.
    pub fn eval(&mut self, ty: Type) -> anyhow::Result<(Value, Option<usize>)> {
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

        let result = if let Some(memory_cache) = self.memory_cache {
            match ty.decode(&(memory_cache, &self.handle.process), address) {
                Ok(value) => value,
                Err(..) => (Value::None, None),
            }
        } else {
            match ty.decode(&self.handle.process, address) {
                Ok(value) => value,
                Err(..) => (Value::None, None),
            }
        };

        // self.cache.insert(ty, result.clone());
        Ok(result)
    }
}

#[derive(Debug)]
pub struct MemoryCache {
    /// Underlying cache.
    cache: RwLock<lru::LruCache<AddressRange, Vec<u8>>>,
    /// Valid memory regions.
    regions: Regions,
}

impl MemoryCache {
    /// Construct a new cache out of the given process.
    pub fn new(regions: Regions) -> Self {
        Self {
            cache: RwLock::new(lru::LruCache::new(0x1000)),
            regions,
        }
    }

    /// Read process memory.
    pub fn read_process_memory<'a>(
        &self,
        process: &Process,
        address: Address,
        buf: &'a mut [u8],
    ) -> anyhow::Result<usize> {
        let region = match self.regions.find_region_for(address) {
            Some(region) => region,
            None => return Ok(0),
        };

        let s = address.as_usize() - region.base.as_usize();
        let e = s.saturating_add(buf.len());

        {
            let cache = self.cache.read();

            if let Some(data) = cache.peek(region) {
                if e >= data.len() {
                    return Ok(0);
                }

                buf.clone_from_slice(&data[s..e]);
                return Ok(buf.len());
            }
        }

        let mut cache = self.cache.write();
        let len = region.size.as_usize();
        let mut data = vec![0u8; len];

        let read = process.read_process_memory(region.base, &mut data)?;

        if read != data.len() {
            return Ok(0);
        }

        let len = if e <= data.len() {
            // full read
            buf.clone_from_slice(&data[s..e]);
            buf.len()
        } else {
            // partial read
            let len = buf.len() - (e - data.len());
            buf[..len].clone_from_slice(&data[s..]);
            len
        };

        cache.put(region.clone(), data);
        Ok(len)
    }
}

impl<'m, 'p> MemoryReader for (&'m MemoryCache, &'p Process) {
    type Process = Process;

    fn read_memory(&self, address: Address, buf: &mut [u8]) -> anyhow::Result<usize> {
        Ok(self.0.read_process_memory(self.1, address, buf)?)
    }

    fn process(&self) -> &Self::Process {
        self.1
    }
}

#[derive(Debug)]
pub struct Regions {
    regions: Vec<MemoryInformation>,
}

impl Regions {
    /// Find the region for the given address.
    pub fn find_region_for(&self, address: Address) -> Option<&AddressRange> {
        AddressRange::find_in_range(&self.regions, |r| &r.range, address).map(|r| &r.range)
    }
}

impl iter::FromIterator<MemoryInformation> for Regions {
    fn from_iter<I: IntoIterator<Item = MemoryInformation>>(iter: I) -> Self {
        let regions = iter.into_iter().collect();
        Self { regions }
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

pub trait Decode: Sized {
    fn decode(buf: &[u8]) -> Result<Self, anyhow::Error>;
}

impl Decode for u64 {
    fn decode(buf: &[u8]) -> Result<Self, anyhow::Error> {
        use byteorder::{ByteOrder, LittleEndian};
        Ok(LittleEndian::read_u64(buf))
    }
}

impl Decode for u32 {
    fn decode(buf: &[u8]) -> Result<Self, anyhow::Error> {
        use byteorder::{ByteOrder, LittleEndian};
        Ok(LittleEndian::read_u32(buf))
    }
}

pub struct Session<'a> {
    handle: &'a ProcessHandle,
    memory_cache: MemoryCache,
}

impl<'a> Session<'a> {
    /// Construct an address proxy for the given address.
    pub fn address_proxy<P>(&'a self, pointer: &'a P) -> AddressProxy<'a, P>
    where
        P: FollowablePointer,
    {
        AddressProxy {
            pointer,
            handle: self.handle,
            memory_cache: Some(&self.memory_cache),
            followed: Cached::None,
            cache: Default::default(),
        }
    }
}
