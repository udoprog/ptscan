//! High-level interface to processes.

use crate::{
    error::Error,
    filter_expr::{FilterExpr, Special},
    process::MemoryInformation,
    scan::{self, ScanProgress, ScanResult},
    system,
    thread::Thread,
    Address, AddressRange, Pointer, PointerBase, Process, ProcessId, Size, Test, Token, Type,
    Value, ValueExpr,
};
use anyhow::{bail, Context as _};
use hashbrown::HashMap;
use parking_lot::RwLock;
use std::{convert::TryFrom, fmt, iter};
use winapi::shared::winerror;

/// A handle for a process.
#[derive(Debug)]
pub struct ProcessHandle {
    /// Name of the process (if present).
    pub name: String,
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

    /// Refresh all available modules.
    ///
    /// If kernel32 is found, extract its range separately.
    pub fn refresh_modules(&mut self) -> Result<(), Error> {
        use std::ffi::OsStr;

        let mut name = None;
        let mut modules = Vec::with_capacity(self.modules.len());
        let mut modules_address = HashMap::new();

        for module in self.process.modules()? {
            let module_name = module.name()?;

            if name.is_none() {
                name = Some(module_name.to_string_lossy().to_string());
            }

            let info = module.info()?;

            let range = AddressRange {
                base: info.base_of_dll,
                size: info.size_of_image,
            };

            if module_name.as_os_str() == OsStr::new("KERNEL32.DLL") {
                self.kernel32 = Some(range);
            }

            let name = module_name.to_string_lossy().to_string();

            modules_address.insert(name.to_string(), range.base);

            modules.push(ModuleInfo { name, range });
        }

        modules.sort_by_key(|m| m.range.base);

        if let Some(name) = name {
            self.name = name;
        }

        self.modules = modules;
        self.modules_address = modules_address;
        Ok(())
    }

    /// Refresh information about known threads.
    pub fn refresh_threads(&mut self) -> anyhow::Result<()> {
        self.threads.clear();

        for (id, t) in system::threads()?.enumerate() {
            if t.process_id() != self.process.process_id() {
                continue;
            }

            let thread = Thread::builder()
                .all_access()
                .query_information()
                .get_context()
                .build(t.thread_id())
                .with_context(|| Error::BuildThread(t.thread_id()))?;

            let stack = self
                .thread_stack(&thread)
                .with_context(|| Error::ThreadStack(thread.thread_id()))?;

            let stack_exit = self
                .scan_for_exit(stack)
                .with_context(|| Error::ScanForExit)?;

            self.threads.push(ProcessThread {
                id,
                stack,
                thread,
                stack_exit,
            });
        }

        self.threads.sort_by_key(|t| t.stack.base);
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
                let offset = address.offset_of(module.range.base)?;
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

    /// Access the address of the stack for the given thread.
    pub fn thread_stack(&self, thread: &Thread) -> Result<AddressRange, Error> {
        // NB: if ptscan is running in 32-bit mode this might have to be different.
        thread.thread_stack(&self.process)
    }

    /// Scan for the base of the stack.
    ///
    /// TODO: look into using VirtualQueryEx to get the range of the stack.
    /// https://msdn.microsoft.com/en-us/library/windows/desktop/aa366907(v=vs.85).aspx
    ///
    /// Ported from:
    /// https://github.com/cheat-engine/cheat-engine/blob/0d9d35183d0dcd9eeed4b4d0004fa3a1981b018a/Cheat%20Engine/CEFuncProc.pas
    pub fn scan_for_exit(&self, stack: AddressRange) -> anyhow::Result<Option<Address>> {
        use byteorder::{ByteOrder, LittleEndian};

        let ptr_width = Size::new(self.process.pointer_width as u64);
        let mut buf = vec![0u8; stack.size.as_usize()];
        self.process.read_process_memory(stack.base, &mut buf)?;

        if !stack.base.is_aligned(ptr_width)? {
            bail!(
                "for some reason, the stack {:?} is not aligned with {}",
                stack,
                ptr_width
            );
        }

        let kernel32 = match self.kernel32.as_ref() {
            Some(kernel32) => kernel32,
            None => return Ok(None),
        };

        for (n, w) in buf.chunks(ptr_width.as_usize()).enumerate().rev() {
            // TODO: make independent of host architecture (use u64).
            let ptr = Address::try_from(LittleEndian::read_u64(w))?;

            if kernel32.contains(ptr) {
                let address = Size::try_from(n * ptr_width.as_usize())?;
                return Ok(Some(stack.base.add(address)?));
            }
        }

        Ok(None)
    }

    /// Construct an address proxy for the given address.
    pub fn address_proxy<'a>(&'a self, pointer: &'a Pointer) -> AddressProxy<'a> {
        AddressProxy {
            pointer,
            handle: self,
            memory_cache: None,
            followed: Cached::None,
        }
    }

    /// Create a new cached session.
    pub fn session(&self) -> Result<Session<'_>, Error> {
        use crate::utils::IteratorExtension as _;

        let regions = self
            .process
            .virtual_memory_regions()
            .only_relevant()
            .collect::<Result<_, Error>>()?;

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
        let mut reporter = scan::Reporter::new(progress, results.len(), cancel, &mut last_error);

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
                                let mut proxy = self.address_proxy(&result.pointer);
                                let ty = new_type.unwrap_or_else(|| result.last().ty());

                                if let Test::True =
                                    filter.test(result.initial(), result.last(), ty, &mut proxy)?
                                {
                                    let value = proxy.eval(ty)?;
                                    result.last = Some(value);
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
        let mut reporter = scan::Reporter::new(progress, results.len(), cancel, &mut last_error);

        // how many tasks to run in parallel.
        let mut tasks = thread_pool.current_num_threads();
        let cancel = &cancel;

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

                            let value_type = new_type.unwrap_or_else(|| result.last().ty());

                            let mut work = move || {
                                let mut proxy = self.address_proxy(&result.pointer);

                                let initial = result.initial();
                                let last = result.last();

                                let expr_type = expr
                                    .type_of(Some(initial.ty()), Some(last.ty()), Some(value_type))?
                                    .ok_or_else(|| Error::TypeInference(expr.clone()))?;

                                let value =
                                    expr.eval(initial, last, value_type, expr_type, &mut proxy)?;

                                result.initial = value;
                                result.last = None;
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

#[derive(Debug, Clone, Copy)]
pub enum Cached<T> {
    Some(T),
    None,
}

#[derive(Debug, Clone)]
pub struct AddressProxy<'a> {
    pointer: &'a Pointer,
    pub(crate) handle: &'a ProcessHandle,
    memory_cache: Option<&'a MemoryCache>,
    /// cached, followed address.
    followed: Cached<Option<Address>>,
}

impl AddressProxy<'_> {
    /// Evaluate the pointer of the proxy.
    pub fn eval(&mut self, ty: Type) -> anyhow::Result<Value> {
        let mut buf = vec![0u8; ty.size(&self.handle.process)];

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
                None => return Ok(Value::None(ty)),
            };

            let value =
                if memory_cache.read_process_memory(&self.handle.process, address, &mut buf)? {
                    match ty.decode(&self.handle.process, &buf) {
                        Ok(value) => value,
                        Err(..) => Value::None(ty),
                    }
                } else {
                    Value::None(ty)
                };

            Ok(value)
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
                None => return Ok(Value::None(ty)),
            };

            let value = if self.handle.process.read_process_memory(address, &mut buf)? {
                match ty.decode(&self.handle.process, &buf) {
                    Ok(value) => value,
                    Err(..) => Value::None(ty),
                }
            } else {
                Value::None(ty)
            };

            Ok(value)
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
    pub fn read_process_memory(
        &self,
        process: &Process,
        address: Address,
        buf: &mut [u8],
    ) -> anyhow::Result<bool> {
        let region = match self.regions.find_region_for(address) {
            Some(region) => region,
            None => return Ok(false),
        };

        let s = address.as_usize() - region.base.as_usize();
        let e = s.saturating_add(buf.len());

        {
            let cache = self.cache.read();

            if let Some(data) = cache.peek(region) {
                if e >= data.len() {
                    return Ok(false);
                }

                buf.clone_from_slice(&data[s..e]);
                return Ok(true);
            }
        }

        let mut cache = self.cache.write();
        let mut data = vec![0u8; region.size.as_usize()];

        let start = std::time::Instant::now();

        println!("reading under lock...");
        if !process.read_process_memory(region.base, &mut data)? {
            return Ok(false);
        }
        println!(
            "done reading under lock: {:?}",
            std::time::Instant::now().duration_since(start)
        );

        let result = if e < data.len() {
            buf.clone_from_slice(&data[s..e]);
            true
        } else {
            false
        };

        cache.put(region.clone(), data);
        Ok(result)
    }

    pub fn with_region<T, U>(
        &self,
        process: &Process,
        region: &AddressRange,
        handle: T,
    ) -> anyhow::Result<Option<U>>
    where
        T: FnOnce(&[u8]) -> anyhow::Result<Option<U>>,
    {
        {
            let cache = self.cache.read();

            if let Some(data) = cache.peek(region) {
                return handle(data);
            }
        }

        let mut cache = self.cache.write();
        let mut data = vec![0u8; region.size.as_usize()];

        if !process.read_process_memory(region.base, &mut data)? {
            return Ok(None);
        }

        let result = handle(&data);
        cache.put(region.clone(), data);
        Ok(result?)
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

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub name: String,
    pub range: AddressRange,
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

#[derive(Debug, Clone)]
pub struct ProcessThread {
    pub id: usize,
    pub stack: AddressRange,
    pub thread: Thread,
    pub stack_exit: Option<Address>,
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
    pub fn address_proxy(&'a self, pointer: &'a Pointer) -> AddressProxy<'a> {
        AddressProxy {
            pointer,
            handle: self.handle,
            memory_cache: Some(&self.memory_cache),
            followed: Cached::None,
        }
    }

    /// Get the next address matching.
    pub fn next_address(
        &self,
        region: &AddressRange,
        address: Address,
        special: &Special,
    ) -> anyhow::Result<Option<Address>> {
        self.memory_cache
            .with_region(&self.handle.process, region, |data| {
                let offset = address.sub_address(region.base)?.as_usize();
                let data = &data[offset..];

                Ok(match special.test(data) {
                    Some(offset) => Some(address.saturating_add(Size::try_from(offset)?)),
                    None => None,
                })
            })
    }
}
