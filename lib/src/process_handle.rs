//! High-level interface to processes.

use crate::{
    error::Error,
    filter_expr::FilterExpr,
    pointer,
    process::{MemoryInformation, MemoryReader},
    progress_reporter::ProgressReporter,
    scan::ScanProgress,
    system,
    thread::Thread,
    Address, AddressRange, Cached, Pointer, PointerBase, Process, ProcessId, ProcessInfo as _,
    ScanResult, Size, Test, ThreadId, Token, Type, TypeHint, Value, ValueExpr,
};
use anyhow::{bail, Context as _};
use hashbrown::HashMap;
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::{convert::TryFrom, ffi::OsString, fmt, iter};
use winapi::shared::winerror;

/// A handle for a process.
#[derive(Debug)]
pub struct ProcessHandle {
    /// Name of the process (if present).
    pub name: String,
    /// The file name of the process.
    pub file_name: Option<OsString>,
    /// Handle to the process.
    pub process: Process,
    /// Information about all loaded modules.
    pub modules: Vec<ModuleInfo>,
    /// Module address by name.
    pub modules_address: HashMap<String, Address>,
    /// The range at which we have found kernel32.dll (if present).
    kernel32: Option<AddressRange>,
    /// Threads.
    pub threads: Vec<ProcessThread>,
}

pub struct ModulesState {
    pub process_name: Option<String>,
    pub process_file_name: Option<OsString>,
    pub modules: Vec<ModuleInfo>,
    pub modules_address: HashMap<String, Address>,
    pub kernel32: Option<AddressRange>,
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

        let name = process.module_base_name()?.to_string_lossy().to_string();

        Ok(Some(ProcessHandle {
            name,
            file_name: None,
            process,
            modules: Vec::new(),
            modules_address: HashMap::new(),
            kernel32: None,
            threads: Vec::new(),
        }))
    }

    /// Find the first process matching the given name.
    pub fn open_by_name(name: &str) -> anyhow::Result<Option<ProcessHandle>> {
        for pid in system::processes()? {
            let handle = match ProcessHandle::open(pid).with_context(|| Error::OpenProcess(pid))? {
                Some(handle) => handle,
                // process cannot be opened.
                None => continue,
            };

            if handle.name.to_lowercase() != name.to_lowercase() {
                continue;
            }

            return Ok(Some(handle));
        }

        Ok(None)
    }

    /// Name of the process.
    pub fn name(&self) -> ProcessName {
        ProcessName {
            id: self.process.process_id(),
            name: self.name.clone(),
        }
    }

    /// Get the current module state for the process.
    pub fn get_modules(&self) -> anyhow::Result<ModulesState> {
        use std::ffi::OsStr;

        let mut process_name = None;
        let mut process_file_name = None;
        let mut modules = Vec::with_capacity(self.modules.len());
        let mut modules_address = HashMap::new();
        let mut kernel32 = None;

        for module in self.process.modules()? {
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
            modules.push(ModuleInfo {
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
            kernel32,
        })
    }

    /// Update modules with the result from a get_modules call.
    pub fn update_modules(&mut self, modules: ModulesState) {
        if let Some(name) = modules.process_name {
            self.name = name;
        }

        self.file_name = modules.process_file_name;

        self.modules = modules.modules;
        self.modules_address = modules.modules_address;
        self.kernel32 = modules.kernel32;
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

    /// Get information on threads.
    pub fn get_threads(&self) -> anyhow::Result<Vec<ProcessThread>> {
        let mut threads = Vec::new();
        Self::load_threads_into(&self.process, &mut threads, self.kernel32.as_ref())?;
        Ok(threads)
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
        Self::load_threads_into(&self.process, &mut self.threads, self.kernel32.as_ref())
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
    pub fn address_to_pointer_base(&self, address: Address) -> anyhow::Result<PointerBase> {
        Ok(match self.find_location(address) {
            Location::Module(module) => {
                let offset = address.offset_of(module.range.base);
                PointerBase::Module {
                    name: module.name.to_string(),
                    offset,
                }
            }
            _ => PointerBase::Address { address },
        })
    }

    /// Find if address is contained in a module.
    pub fn find_module(&self, address: Address) -> Option<&ModuleInfo> {
        AddressRange::find_in_range(&self.modules, |m| &m.range, address)
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
        use byteorder::{ByteOrder, LittleEndian};

        let ptr_width = Size::new(process.pointer_width as u64);
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
            let ptr = Address::try_from(LittleEndian::read_u64(w))?;

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

    pub fn null_address_proxy(&self) -> AddressProxy<'_> {
        AddressProxy {
            ty: Type::None,
            pointer: &pointer::NULL_POINTER,
            handle: self,
            memory_cache: None,
            followed: Cached::None,
            evaled: Cached::None,
        }
    }

    /// Construct an address proxy for the given address.
    pub fn address_proxy<'a>(&'a self, pointer: &'a Pointer, ty: Type) -> AddressProxy<'a> {
        AddressProxy {
            ty,
            pointer,
            handle: self,
            memory_cache: None,
            followed: Cached::None,
            evaled: Cached::None,
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
        results: &mut Vec<Box<ScanResult>>,
        new_type: Option<Type>,
        cancel: Option<&Token>,
        progress: (impl ScanProgress + Send),
        filter: &FilterExpr,
    ) -> anyhow::Result<()> {
        use std::sync::mpsc;

        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        if results.is_empty() {
            return Ok(());
        }

        let mut last_error = None;
        let mut reporter = ProgressReporter::new(progress, results.len(), cancel, &mut last_error);

        // how many tasks to run in parallel.
        let mut tasks = thread_pool.current_num_threads();

        let mut to_remove = Vec::new();

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel::<anyhow::Result<Task>>();
                let (p_tx, p_rx) =
                    crossbeam_channel::unbounded::<Option<(usize, &mut ScanResult)>>();

                for _ in 0..tasks {
                    let tx = tx.clone();
                    let p_rx = p_rx.clone();

                    s.spawn(move |_| {
                        while !cancel.test() {
                            let (index, result) = match p_rx.recv().expect("failed to receive") {
                                Some(task) => task,
                                None => break,
                            };

                            let mut work = || {
                                let value_type = new_type.unwrap_or_else(|| result.last_type());
                                let mut proxy = self.address_proxy(&result.pointer, value_type);

                                if let Test::True = filter.test(
                                    result.initial_info(),
                                    result.last_info(),
                                    &mut proxy,
                                )? {
                                    let (value, _) = proxy.eval()?;
                                    result.last = value;
                                    *result.pointer.last_address_mut() = proxy.follow_default()?;
                                    return Ok(Task::Accepted);
                                }

                                Ok(Task::Rejected(index))
                            };

                            tx.send(work()).expect("failed to send");
                        }

                        tx.send(Ok(Task::Done)).expect("failed to send");
                    });
                }

                let mut it = results.iter_mut().enumerate();

                for (index, result) in (&mut it).take(tasks) {
                    p_tx.send(Some((index, &mut *result)))
                        .expect("failed to send");
                }

                let mut count = 0u64;

                while tasks > 0 {
                    let result = rx.recv().expect("closed");

                    if let Some((index, result)) = it.next() {
                        p_tx.send(Some((index, &mut **result)))
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
            results.swap_remove(index);
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
        results: &mut [Box<ScanResult>],
        cancel: Option<&Token>,
        new_type: Option<Type>,
        progress: (impl ScanProgress + Send),
        expr: &ValueExpr,
    ) -> anyhow::Result<()> {
        use std::sync::mpsc;

        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        if results.is_empty() {
            return Ok(());
        }

        let mut last_error = None;
        let mut reporter = ProgressReporter::new(progress, results.len(), cancel, &mut last_error);

        // how many tasks to run in parallel.
        let mut tasks = thread_pool.current_num_threads();
        let cancel = &cancel;

        let new_type = match new_type {
            Some(new_type) => Some(new_type),
            None => expr.value_type_of(TypeHint::NoHint)?.option(),
        };

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel::<anyhow::Result<bool>>();
                let (p_tx, p_rx) = crossbeam_channel::unbounded::<Option<&mut ScanResult>>();

                for _ in 0..tasks {
                    let tx = tx.clone();
                    let p_rx = p_rx.clone();

                    s.spawn(move |_| {
                        while !cancel.test() {
                            let result = match p_rx.recv().expect("closed") {
                                Some(task) => task,
                                None => {
                                    break;
                                }
                            };

                            let value_type = new_type.unwrap_or_else(|| result.last_type());

                            let mut work = move || {
                                let mut proxy = self.address_proxy(&result.pointer, value_type);

                                let initial = result.initial_info();
                                let last = result.last_info();

                                let expr_type = expr
                                    .type_of(
                                        TypeHint::Explicit(initial.ty),
                                        TypeHint::Explicit(last.ty),
                                        TypeHint::Explicit(value_type),
                                        TypeHint::NoHint,
                                    )?
                                    .ok_or_else(|| Error::TypeInference(expr.clone()))?;

                                let value = expr
                                    .type_check(initial.ty, last.ty, value_type, expr_type)?
                                    .eval(initial, last, &mut proxy)?;

                                result.initial = value;
                                result.last = Value::None;
                                *result.pointer.last_address_mut() = proxy.follow_default()?;
                                Ok(false)
                            };

                            tx.send(work()).expect("failed to send");
                        }

                        tx.send(Ok(true)).expect("failed to send");
                    });
                }

                let mut it = results.iter_mut();

                for result in (&mut it).take(tasks) {
                    p_tx.send(Some(&mut **result)).expect("closed");
                }

                let mut count = 0u64;

                while tasks > 0 {
                    let res = rx.recv().expect("failed to receive on channel");

                    if let Some(result) = it.next() {
                        p_tx.send(Some(&mut **result))
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
}

#[derive(Debug, Clone)]
pub struct AddressProxy<'a> {
    pub(crate) ty: Type,
    pointer: &'a Pointer,
    pub(crate) handle: &'a ProcessHandle,
    memory_cache: Option<&'a MemoryCache>,
    /// cached, followed address.
    pub followed: Cached<Option<Address>>,
    evaled: Cached<(Value, Option<usize>)>,
}

impl AddressProxy<'_> {
    /// Evaluate the pointer of the proxy.
    pub fn eval(&mut self) -> anyhow::Result<(Value, Option<usize>)> {
        if let Cached::Some(result) = &self.evaled {
            return Ok(result.clone());
        }

        if let Some(memory_cache) = self.memory_cache {
            let address = match self.followed {
                Cached::Some(address) => address,
                Cached::None => {
                    let address = self.pointer.follow(self.handle, |a, buf| {
                        memory_cache.read_process_memory(&self.handle.process, a, buf)
                    })?;

                    self.followed = Cached::Some(address);
                    address
                }
            };

            let address = match address {
                Some(address) => address,
                None => return Ok((Value::None, None)),
            };

            let result = match self
                .ty
                .decode(&(memory_cache, &self.handle.process), address)
            {
                Ok(value) => value,
                Err(..) => (Value::None, None),
            };

            self.evaled = Cached::Some(result.clone());
            Ok(result)
        } else {
            let address = match self.followed {
                Cached::Some(address) => address,
                Cached::None => {
                    let address = self.pointer.follow_default(self.handle)?;
                    self.followed = Cached::Some(address);
                    address
                }
            };

            let address = match address {
                Some(address) => address,
                None => return Ok((Value::None, None)),
            };

            let result = match self.ty.decode(&self.handle.process, address) {
                Ok(value) => value,
                Err(..) => (Value::None, None),
            };

            self.evaled = Cached::Some(result.clone());
            Ok(result)
        }
    }

    /// Follow the address this proxy refers to.
    pub fn follow_default(&mut self) -> anyhow::Result<Option<Address>> {
        if let Cached::Some(address) = self.followed {
            return Ok(address);
        }

        let address = if let Some(memory_cache) = self.memory_cache {
            self.pointer.follow(self.handle, |a, buf| {
                memory_cache.read_process_memory(&self.handle.process, a, buf)
            })?
        } else {
            self.pointer.follow_default(self.handle)?
        };

        self.followed = Cached::Some(address);
        Ok(address)
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
    pub fn address_proxy(&'a self, pointer: &'a Pointer, ty: Type) -> AddressProxy<'a> {
        AddressProxy {
            ty,
            pointer,
            handle: self.handle,
            memory_cache: Some(&self.memory_cache),
            followed: Cached::None,
            evaled: Cached::None,
        }
    }
}
