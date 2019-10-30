use std::{convert::TryFrom, ffi, fmt, iter, ptr, slice, sync::Arc};

use crate::{
    error::Error, filter, module, scan, scan::ScanResult, system, utils, Address, AddressRange,
    ProcessId, Size, Token, Type, Value,
};

use ntapi::ntpsapi;
use parking_lot::RwLock;
use winapi::{
    shared::{
        basetsd::SIZE_T,
        minwindef::{BOOL, DWORD, FALSE, LPCVOID, LPVOID, PBOOL, TRUE},
        winerror,
    },
    um::{memoryapi, processthreadsapi, psapi, winnt, wow64apiset},
};

const ZERO: [u8; 16] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

pub struct Session<'a> {
    process: &'a Process,
    memory_cache: MemoryCache,
}

impl<'a> Session<'a> {
    /// Construct an address proxy for the given address.
    pub fn address_proxy(&'a self, address: Address) -> AddressProxy<'a> {
        AddressProxy {
            address,
            process: self.process,
            memory_cache: Some(&self.memory_cache),
        }
    }

    /// Get the next address matching.
    pub fn next_address(
        &self,
        region: &AddressRange,
        address: Address,
        special: &filter::Special,
    ) -> anyhow::Result<Option<Address>> {
        match special {
            filter::Special::Bytes(bytes) => {
                if bytes.is_empty() {
                    return Ok(None);
                }

                let offset = address.sub_address(region.base)?.as_usize();

                return self.memory_cache.with_region(self.process, region, |data| {
                    let first = bytes[0];

                    let index = match memchr::memchr(first, &data[offset..]) {
                        Some(index) => index,
                        None => return Ok(None),
                    };

                    Ok(Some(address.saturating_add(Size::try_from(index)?)))
                });
            }
            filter::Special::NonZero => {
                let offset = address.sub_address(region.base)?.as_usize();

                return self.memory_cache.with_region(self.process, region, |data| {
                    let mut data = &data[offset..];

                    let mut local = 0;

                    while !data.is_empty() {
                        let len = usize::min(data.len(), ZERO.len());

                        if &data[..len] != &ZERO[..len] {
                            break;
                        }

                        data = &data[len..];
                        local += len;
                    }

                    let index = match data.iter().position(|c| *c != 0) {
                        Some(index) => index,
                        None => return Ok(Some(address.saturating_add(Size::try_from(local)?))),
                    };

                    return Ok(Some(address.saturating_add(Size::try_from(local + index)?)));
                });
            }
        }
    }
}

/// A process handle.
#[derive(Clone)]
pub struct Process {
    process_id: ProcessId,
    pub(crate) handle: Arc<utils::Handle>,
}

impl Process {
    pub fn builder() -> OpenProcessBuilder {
        OpenProcessBuilder {
            desired_access: Default::default(),
            inherit_handles: FALSE,
        }
    }

    /// Construct an address proxy for the given address.
    pub fn address_proxy(&self, address: Address) -> AddressProxy<'_> {
        AddressProxy {
            address,
            process: self,
            memory_cache: None,
        }
    }

    /// Create a new cached session.
    pub fn session<'a>(&'a self) -> Result<Session<'a>, Error> {
        use crate::utils::IteratorExtension as _;

        let regions = self
            .virtual_memory_regions()
            .only_relevant()
            .collect::<Result<_, Error>>()?;

        Ok(Session {
            process: self,
            memory_cache: MemoryCache::new(regions),
        })
    }

    /// Get the name of the process.
    pub fn module_base_name(&self) -> Result<ffi::OsString, Error> {
        crate::utils::string(|buf, len| unsafe {
            psapi::GetModuleBaseNameW(**self.handle, ptr::null_mut(), buf, len)
        })
    }

    /// Get the process ID.
    pub fn process_id(&self) -> ProcessId {
        self.process_id
    }

    /// Enumerate all modules.
    pub fn modules<'a>(&'a self) -> Result<Vec<module::Module<'a>>, Error> {
        let modules = utils::array(0x400, |buf, size, needed| unsafe {
            psapi::EnumProcessModules(**self.handle, buf, size, needed)
        })?;

        let mut out = Vec::new();

        for module in modules {
            out.push(module::Module::new(self, module))
        }

        Ok(out)
    }

    /// Read the given structure from the given address.
    ///
    /// This is unsafe since it reinteprets the given memory region as a type, which might not be correct.
    pub unsafe fn read<T>(&self, address: Address) -> Result<T, Error> {
        use std::mem;

        let mut out: T = mem::zeroed();
        let buf = slice::from_raw_parts_mut(&mut out as *mut T as *mut u8, mem::size_of::<T>());

        if !self.read_process_memory(address, buf)? {
            return Err(Error::ReadUnderflow);
        }

        Ok(out)
    }

    /// Write to process memory at the specified address.
    pub fn write_process_memory<'a>(
        &self,
        address: Address,
        buffer: &'a [u8],
    ) -> Result<bool, Error> {
        let mut bytes_written: SIZE_T = 0;

        let result = checked!(memoryapi::WriteProcessMemory(
            **self.handle,
            address.convert::<LPVOID>()?,
            buffer.as_ptr() as LPCVOID,
            buffer.len() as SIZE_T,
            &mut bytes_written as *mut SIZE_T,
        ));

        match result {
            Ok(()) => Ok(bytes_written == buffer.len()),
            Err(ref e) if e.raw_os_error() == Some(winerror::ERROR_PARTIAL_COPY as i32) => {
                Ok(false)
            }
            Err(e) => Err(e),
        }
    }

    /// Read process memory at the specified address.
    ///
    /// Returns `true` if the region was successfully read.
    /// Returns `false` if there was not enough space in the region that we attempted to read.
    pub fn read_process_memory<'a>(
        &self,
        address: Address,
        buffer: &'a mut [u8],
    ) -> Result<bool, Error> {
        let mut bytes_read: SIZE_T = 0;

        let result = checked!(memoryapi::ReadProcessMemory(
            **self.handle,
            address.convert::<LPCVOID>()?,
            buffer.as_mut_ptr() as LPVOID,
            buffer.len() as SIZE_T,
            &mut bytes_read as *mut SIZE_T,
        ));

        match result {
            Ok(()) => Ok(bytes_read == buffer.len()),
            Err(ref e) if e.raw_os_error() == Some(winerror::ERROR_PARTIAL_COPY as i32) => {
                Ok(false)
            }
            Err(e) => Err(e),
        }
    }

    /// Read process memory with the given type.
    pub fn read_memory_of_type(
        &self,
        address: Address,
        ty: Type,
        buf: &mut [u8],
    ) -> Result<Value, Error> {
        if !self.read_process_memory(address, buf)? {
            return Err(Error::ReadUnderflow);
        }

        Ok(ty.decode(buf)?)
    }

    /// Read a 64-bit memory region as a little endian unsigned integer.
    pub fn read_u64(&self, address: Address) -> Result<u64, Error> {
        use byteorder::{ByteOrder, LittleEndian};
        let mut out = [0u8; std::mem::size_of::<u64>()];

        if !self.read_process_memory(address, &mut out)? {
            return Err(Error::ReadUnderflow);
        }

        Ok(LittleEndian::read_u64(&out))
    }

    /// Test if the process is a 32-bit process running under WOW64 compatibility.
    pub fn is_wow64(&self) -> Result<bool, Error> {
        let mut out: BOOL = FALSE;
        checked!(wow64apiset::IsWow64Process(
            **self.handle,
            &mut out as PBOOL
        ))?;
        Ok(out == TRUE)
    }

    /// Test if this is a 64-bit process.
    pub fn is_64bit(&self) -> Result<bool, Error> {
        if self.is_wow64()? {
            return Ok(false);
        }

        Ok(system::info()?.arch.is_64bit())
    }

    /// Retrieve information about the memory page that corresponds to the given virtual `address`.
    pub fn virtual_query(&self, address: Address) -> Result<Option<MemoryInformation>, Error> {
        const LENGTH: SIZE_T = mem::size_of::<winnt::MEMORY_BASIC_INFORMATION>() as SIZE_T;

        use std::mem;
        let mut mbi: winnt::MEMORY_BASIC_INFORMATION = unsafe { mem::zeroed() };

        let b = unsafe {
            memoryapi::VirtualQueryEx(
                **self.handle,
                address.convert()?,
                &mut mbi as *mut _ as winnt::PMEMORY_BASIC_INFORMATION,
                LENGTH as SIZE_T,
            )
        };

        if b == 0 {
            let e = Error::last_system_error();

            if e.raw_os_error() == Some(winerror::ERROR_INVALID_PARAMETER as i32) {
                return Ok(None);
            }

            return Err(e);
        }

        let state = match mbi.State {
            winnt::MEM_COMMIT => MemoryState::Commit,
            winnt::MEM_FREE => MemoryState::Free,
            winnt::MEM_RESERVE => MemoryState::Reserve,
            state => {
                return Err(Error::BadRegionState(state));
            }
        };

        let ty = match (state, mbi.Type) {
            (MemoryState::Free, _) => MemoryType::None,
            (_, winnt::MEM_IMAGE) => MemoryType::Image,
            (_, winnt::MEM_MAPPED) => MemoryType::Mapped,
            (_, winnt::MEM_PRIVATE) => MemoryType::Private,
            (_, ty) => {
                return Err(Error::BadRegionType(ty));
            }
        };

        let mut protect = fixed_map::Set::new();

        if state != MemoryState::Free {
            MemoryProtect::decode(mbi.Protect, &mut protect);
        }

        Ok(Some(MemoryInformation {
            range: AddressRange {
                base: Address::try_from(mbi.BaseAddress)?,
                size: Size::try_from(mbi.RegionSize)?,
            },
            state,
            ty,
            protect,
        }))
    }

    /// Iterate over all virtual memory segments with a given chunk size.
    pub fn virtual_memory_regions<'a>(&'a self) -> VirtualMemoryRegions<'a> {
        VirtualMemoryRegions {
            process: self,
            current: Address::new(0),
        }
    }

    /// Suspend the process.
    pub fn suspend(&self) -> Result<(), Error> {
        let status = unsafe { ntpsapi::NtSuspendProcess(**self.handle) };

        if status != 0 {
            return Err(Error::last_system_error());
        }

        Ok(())
    }

    /// Resume the process.
    pub fn resume(&self) -> Result<(), Error> {
        let status = unsafe { ntpsapi::NtResumeProcess(**self.handle) };

        if status != 0 {
            return Err(Error::last_system_error());
        }

        Ok(())
    }

    /// Read the value of many memory locations.
    pub fn rescan_values(
        &self,
        thread_pool: &rayon::ThreadPool,
        results: &[ScanResult],
        results_output: &mut Vec<ScanResult>,
        filter: &filter::Filter,
        cancel: Option<&Token>,
        progress: (impl scan::Progress + Send),
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
        let tasks = 0x100usize;

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel::<anyhow::Result<Task>>();
                let (p_tx, p_rx) = crossbeam_channel::unbounded::<Option<&ScanResult>>();

                for _ in 0..thread_pool.current_num_threads() {
                    let tx = tx.clone();
                    let p_rx = p_rx.clone();

                    s.spawn(move |_| {
                        let mut results = Vec::new();

                        loop {
                            let result = match p_rx.recv().expect("closed") {
                                Some(task) => task,
                                None => {
                                    tx.send(Ok(Task::Result(results))).expect("closed");
                                    return;
                                }
                            };

                            let mut work = || {
                                let proxy = self.address_proxy(result.address);

                                if filter.test(&result.value, proxy)? {
                                    let value = proxy.eval(filter.ty)?;

                                    results.push(ScanResult {
                                        address: result.address,
                                        value,
                                    });

                                    return Ok(Task::Accepted);
                                }

                                Ok(Task::Rejected)
                            };

                            tx.send(work()).expect("closed");
                        }
                    });
                }

                let mut it = results.iter();

                for result in (&mut it).take(tasks) {
                    p_tx.send(Some(result)).expect("closed");
                }

                let mut count = 0u64;
                let mut expected = thread_pool.current_num_threads();

                while !reporter.is_done() {
                    if cancel.test() {
                        break;
                    }

                    let result = rx.recv().expect("closed");

                    if let Some(result) = it.next() {
                        p_tx.send(Some(result)).expect("closed");
                    }

                    count += match reporter.eval(result).unwrap_or(Task::Rejected) {
                        Task::Accepted => 1,
                        Task::Rejected => 0,
                        Task::Result(mut results) => {
                            results_output.append(&mut results);
                            expected -= 1;
                            continue;
                        }
                    };

                    reporter.tick(count);
                }

                // send indicator for threads to go.
                for _ in 0..thread_pool.current_num_threads() {
                    p_tx.send(None).expect("closed");
                }

                // reap threads.
                while expected > 0 {
                    let result = rx.recv().expect("closed");

                    count += match reporter.eval(result).unwrap_or(Task::Rejected) {
                        Task::Accepted => 1,
                        Task::Rejected => 0,
                        Task::Result(mut results) => {
                            results_output.append(&mut results);
                            expected -= 1;
                            continue;
                        }
                    };

                    reporter.tick(count);
                }

                Ok::<_, anyhow::Error>(())
            })?;

            Ok::<_, anyhow::Error>(())
        })?;

        if let Some(e) = last_error {
            return Err(e);
        }

        return Ok(());

        enum Task {
            /// Address was accepted.
            Accepted,
            /// Address was rejected.
            Rejected,
            /// The result of a computation.
            Result(Vec<ScanResult>),
        }
    }

    /// Refresh a given set of existing values without a filter.
    pub fn refresh_values(
        &self,
        thread_pool: &rayon::ThreadPool,
        results: &mut [ScanResult],
        cancel: Option<&Token>,
        progress: (impl scan::Progress + Send),
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
        let tasks = 0x100usize;

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel();
                let (p_tx, p_rx) = crossbeam_channel::unbounded::<Option<&mut ScanResult>>();

                for _ in 0..thread_pool.current_num_threads() {
                    let tx = tx.clone();
                    let p_rx = p_rx.clone();

                    let mut buf = vec![0u8; 0x10];

                    s.spawn(move |_| loop {
                        let result = match p_rx.recv().expect("closed") {
                            Some(task) => task,
                            None => {
                                tx.send(None).expect("closed");
                                return;
                            }
                        };

                        let needed = result.value.size();

                        if buf.len() < needed {
                            buf.extend(
                                std::iter::repeat(0u8).take(needed.saturating_sub(buf.len())),
                            );
                        }

                        let buf = &mut buf[..needed];

                        let mut work = move || {
                            if !self.read_process_memory(result.address, buf)? {
                                return Ok(0u64);
                            }

                            result.value = match result.value.ty().decode(buf) {
                                Ok(value) => value,
                                Err(_) => Value::None,
                            };

                            Ok(1)
                        };

                        tx.send(Some(work())).expect("closed");
                    });
                }

                let mut it = results.iter_mut();

                for result in (&mut it).take(tasks) {
                    p_tx.send(Some(result)).expect("closed");
                }

                let mut count = 0u64;
                let mut expected = thread_pool.current_num_threads();

                while !reporter.is_done() {
                    if cancel.test() {
                        break;
                    }

                    let res = rx.recv().expect("closed").expect("sporadic none option");

                    if let Some(result) = it.next() {
                        p_tx.send(Some(result)).expect("closed");
                    }

                    count += reporter.eval(res).unwrap_or_default();
                    reporter.tick(count);
                }

                // send indicator for threads to go.
                for _ in 0..thread_pool.current_num_threads() {
                    p_tx.send(None).expect("closed");
                }

                // reap threads.
                while expected > 0 {
                    match rx.recv().expect("closed") {
                        Some(result) => {
                            count += reporter.eval(result).unwrap_or_default();
                            reporter.tick(count);
                        }
                        None => {
                            expected -= 1;
                        }
                    }
                }
            });
        });

        if let Some(e) = last_error {
            return Err(e);
        }

        Ok(())
    }
}

pub struct FindRegionsFor<'a> {
    iter: slice::Iter<'a, MemoryInformation>,
    end: Address,
}

impl<'a> Iterator for FindRegionsFor<'a> {
    type Item = &'a MemoryInformation;

    fn next(&mut self) -> Option<Self::Item> {
        let region = match self.iter.next() {
            Some(region) => region,
            None => return None,
        };

        if self.end < region.range.base {
            return None;
        }

        Some(region)
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

        if !process.read_process_memory(region.base, &mut data)? {
            return Ok(false);
        }

        buf.clone_from_slice(&data[s..e]);
        cache.put(region.clone(), data);
        Ok(true)
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

#[derive(Debug, Clone, Copy)]
pub struct AddressProxy<'a> {
    address: Address,
    process: &'a Process,
    memory_cache: Option<&'a MemoryCache>,
}

impl AddressProxy<'_> {
    /// Evaluate the location of the proxy.
    pub fn eval(&self, ty: Type) -> anyhow::Result<Value> {
        let mut buf = vec![0u8; ty.size()];

        if let Some(memory_cache) = self.memory_cache {
            if memory_cache.read_process_memory(self.process, self.address, &mut buf)? {
                return Ok(match ty.decode(&buf) {
                    Ok(value) => value,
                    Err(..) => Value::None,
                });
            }
        } else {
            if self.process.read_process_memory(self.address, &mut buf)? {
                return Ok(match ty.decode(&buf) {
                    Ok(value) => value,
                    Err(..) => Value::None,
                });
            }
        }

        Ok(Value::None)
    }
}

impl fmt::Debug for Process {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Process")
            .field("process_id", &self.process_id)
            .finish()
    }
}

/// Builder to open a process.
#[derive(Default)]
pub struct OpenProcessBuilder {
    desired_access: DWORD,
    inherit_handles: BOOL,
}

impl OpenProcessBuilder {
    /// Open process with all access.
    pub fn all_access(self) -> Self {
        OpenProcessBuilder {
            desired_access: self.desired_access | winnt::PROCESS_ALL_ACCESS,
            ..self
        }
    }

    /// Set the query information flag.
    pub fn query_information(self) -> Self {
        OpenProcessBuilder {
            desired_access: self.desired_access | winnt::PROCESS_QUERY_INFORMATION,
            ..self
        }
    }

    /// Set the VM Read flag.
    pub fn vm_read(self) -> Self {
        OpenProcessBuilder {
            desired_access: self.desired_access | winnt::PROCESS_VM_READ,
            ..self
        }
    }

    /// Indicate that the process should inherit handles.
    pub fn inherit_handles(self) -> Self {
        OpenProcessBuilder {
            inherit_handles: TRUE,
            ..self
        }
    }

    /// Build the process handle.
    pub fn build(self, process_id: ProcessId) -> Result<Process, Error> {
        let handle = unsafe {
            processthreadsapi::OpenProcess(self.desired_access, self.inherit_handles, process_id)
        };

        if handle.is_null() {
            return Err(Error::last_system_error());
        }

        let handle = Arc::new(utils::Handle::new(handle));

        Ok(Process { process_id, handle })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MemoryState {
    Commit,
    Free,
    Reserve,
}

impl MemoryState {
    /// Test if the memory state is free (i.e. unused).
    pub fn is_free(&self) -> bool {
        match *self {
            MemoryState::Free => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MemoryType {
    None,
    Image,
    Mapped,
    Private,
}

#[derive(fixed_map::Key, Debug, Clone, Copy)]
pub enum MemoryProtect {
    Execute,
    ExecuteRead,
    ExecuteReadWrite,
    ExecuteWriteCopy,
    NoAccess,
    ReadOnly,
    ReadWrite,
    WriteCopy,
    TargetsInvalid,
    TargetsNoUpdate,
    Guard,
    NoCache,
    WriteCombine,
}

macro_rules! protect_bits {
    ($flag:expr, $set:expr, {$($flag_from:ident => $flag_to:ident,)*}) => {
        $(if $flag & winnt::$flag_from != 0 {
            $set.insert(MemoryProtect::$flag_to);
        })*
    }
}

impl MemoryProtect {
    fn decode(protect: DWORD, set: &mut fixed_map::Set<MemoryProtect>) {
        protect_bits!(protect, set, {
            PAGE_EXECUTE => Execute,
            PAGE_EXECUTE_READ => ExecuteRead,
            PAGE_EXECUTE_READWRITE => ExecuteReadWrite,
            PAGE_EXECUTE_WRITECOPY => ExecuteWriteCopy,
            PAGE_NOACCESS => NoAccess,
            PAGE_READONLY => ReadOnly,
            PAGE_READWRITE => WriteCopy,
            PAGE_WRITECOPY => NoAccess,
            PAGE_TARGETS_INVALID => TargetsInvalid,
            PAGE_TARGETS_NO_UPDATE => TargetsNoUpdate,
            PAGE_GUARD => Guard,
            PAGE_NOCACHE => NoCache,
            PAGE_WRITECOMBINE => WriteCombine,
        });
    }
}

#[derive(Debug, Clone)]
pub struct MemoryInformation {
    pub range: AddressRange,
    pub state: MemoryState,
    pub ty: MemoryType,
    pub protect: fixed_map::Set<MemoryProtect>,
}

/// Iterator over virtual memory regions.
pub struct VirtualMemoryRegions<'a> {
    process: &'a Process,
    current: Address,
}

impl<'a> Iterator for VirtualMemoryRegions<'a> {
    type Item = Result<MemoryInformation, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let m = try_iter!(self.process.virtual_query(self.current))?;
        self.current = try_iter!(try_iter!(self.current.add(m.range.size)).add(Size::new(1)));
        return Some(Ok(m));
    }
}
