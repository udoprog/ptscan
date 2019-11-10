use std::{convert::TryFrom, ffi, fmt, ptr, slice, sync::Arc};

use crate::{error::Error, module, utils, Address, AddressRange, Endianness, ProcessId, Size};

use ntapi::ntpsapi;
use winapi::{
    shared::{
        basetsd::SIZE_T,
        minwindef::{BOOL, DWORD, FALSE, LPCVOID, LPVOID, TRUE},
        winerror,
    },
    um::{memoryapi, processthreadsapi, psapi, winnt},
};

pub trait MemoryReader<'a>: Copy {
    fn read_memory<'m>(self, address: Address, buf: &'m mut [u8]) -> anyhow::Result<usize>;

    fn process(self) -> &'a Process;
}

impl<'a> MemoryReader<'a> for &'a Process {
    fn read_memory<'m>(self, address: Address, buf: &'m mut [u8]) -> anyhow::Result<usize> {
        Ok(self.read_process_memory(address, buf)?)
    }

    fn process(self) -> &'a Process {
        self
    }
}

/// A process handle.
#[derive(Clone)]
pub struct Process {
    pub process_id: ProcessId,
    pub is_64bit: bool,
    pub pointer_width: usize,
    pub endianness: Endianness,
    pub(crate) handle: Arc<utils::Handle>,
}

impl Process {
    pub fn builder() -> OpenProcessBuilder {
        OpenProcessBuilder {
            desired_access: Default::default(),
            inherit_handles: FALSE,
        }
    }

    pub fn read_u16(&self, buf: &[u8]) -> u16 {
        use byteorder::ByteOrder as _;
        // FUTURE TODO: take endianness into account for remote scans.
        byteorder::NativeEndian::read_u16(buf)
    }

    pub fn read_u32(&self, buf: &[u8]) -> u32 {
        use byteorder::ByteOrder as _;
        // FUTURE TODO: take endianness into account for remote scans.
        byteorder::NativeEndian::read_u32(buf)
    }

    pub fn read_u64(&self, buf: &[u8]) -> u64 {
        use byteorder::ByteOrder as _;
        // FUTURE TODO: take endianness into account for remote scans.
        byteorder::NativeEndian::read_u64(buf)
    }

    pub fn read_u128(&self, buf: &[u8]) -> u128 {
        use byteorder::ByteOrder as _;
        // FUTURE TODO: take endianness into account for remote scans.
        byteorder::NativeEndian::read_u128(buf)
    }

    pub fn read_f32(&self, buf: &[u8]) -> f32 {
        use byteorder::ByteOrder as _;
        // FUTURE TODO: take endianness into account for remote scans.
        byteorder::NativeEndian::read_f32(buf)
    }

    pub fn read_f64(&self, buf: &[u8]) -> f64 {
        use byteorder::ByteOrder as _;
        // FUTURE TODO: take endianness into account for remote scans.
        byteorder::NativeEndian::read_f64(buf)
    }

    pub fn encode_u16(&self, buf: &mut [u8], value: u16) {
        use byteorder::ByteOrder as _;
        byteorder::NativeEndian::write_u16(buf, value)
    }

    pub fn encode_u32(&self, buf: &mut [u8], value: u32) {
        use byteorder::ByteOrder as _;
        byteorder::NativeEndian::write_u32(buf, value)
    }

    pub fn encode_u64(&self, buf: &mut [u8], value: u64) {
        use byteorder::ByteOrder as _;
        byteorder::NativeEndian::write_u64(buf, value)
    }

    pub fn encode_u128(&self, buf: &mut [u8], value: u128) {
        use byteorder::ByteOrder as _;
        byteorder::NativeEndian::write_u128(buf, value)
    }

    pub fn encode_f32(&self, buf: &mut [u8], value: f32) {
        use byteorder::ByteOrder as _;
        byteorder::NativeEndian::write_f32(buf, value)
    }

    pub fn encode_f64(&self, buf: &mut [u8], value: f64) {
        use byteorder::ByteOrder as _;
        byteorder::NativeEndian::write_f64(buf, value)
    }

    /// Encode a pointer into the specified buffer.
    pub fn encode_pointer(&self, buf: &mut [u8], value: u64) -> anyhow::Result<(), Error> {
        assert!(buf.len() == self.pointer_width);

        match self.pointer_width {
            8 => self.encode_u64(buf, value),
            4 => {
                let value = u32::try_from(value).map_err(|_| Error::PointerConversionError)?;
                self.encode_u32(buf, value);
            }
            n => return Err(Error::UnsupportedPointerWidth(n)),
        }

        Ok(())
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
    ///
    /// # Safety
    ///
    /// The type `T` must be zeroable and support being punted from process memory.
    pub unsafe fn read<T>(&self, address: Address) -> Result<T, Error> {
        use std::mem;

        let mut out: T = mem::zeroed();
        let buf = slice::from_raw_parts_mut(&mut out as *mut T as *mut u8, mem::size_of::<T>());

        let len = self.read_process_memory(address, buf)?;

        if len != buf.len() {
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
    ) -> Result<usize, Error> {
        let mut bytes_read: SIZE_T = 0;

        let result = checked!(memoryapi::ReadProcessMemory(
            **self.handle,
            address.convert::<LPCVOID>()?,
            buffer.as_mut_ptr() as LPVOID,
            buffer.len() as SIZE_T,
            &mut bytes_read as *mut SIZE_T,
        ));

        match result {
            Ok(()) => Ok(bytes_read),
            Err(ref e) if e.raw_os_error() == Some(winerror::ERROR_PARTIAL_COPY as i32) => Ok(0),
            Err(ref e) if e.raw_os_error() == Some(winerror::ERROR_NOACCESS as i32) => Ok(0),
            Err(e) => Err(e),
        }
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
    pub fn virtual_memory_regions(&self) -> VirtualMemoryRegions<'_> {
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

        let handle = utils::Handle::new(handle);
        let is_64bit = handle.is_64bit()?;
        let handle = Arc::new(handle);

        let pointer_width = if is_64bit { 8 } else { 4 };
        let endianness = Endianness::LittleEndian;

        Ok(Process {
            process_id,
            is_64bit,
            pointer_width,
            endianness,
            handle,
        })
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
    pub fn is_free(self) -> bool {
        match self {
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

impl MemoryInformation {
    /// Test if region is writable.
    pub fn is_writable(&self) -> bool {
        use self::MemoryProtect::*;
        self.is_protect_any(&[ReadWrite, WriteCopy, ExecuteReadWrite, ExecuteWriteCopy])
    }

    /// Test if region is executable.
    pub fn is_executable(&self) -> bool {
        use self::MemoryProtect::*;
        self.is_protect_any(&[Execute, ExecuteRead, ExecuteReadWrite, ExecuteWriteCopy])
    }

    /// Test if region is executable.
    pub fn is_copy_on_write(&self) -> bool {
        use self::MemoryProtect::*;
        self.is_protect_any(&[WriteCopy, ExecuteWriteCopy])
    }

    /// Test if protection contains any of the specified elements.
    fn is_protect_any<'a>(&self, it: impl IntoIterator<Item = &'a MemoryProtect>) -> bool {
        it.into_iter().any(|p| self.protect.contains(*p))
    }
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
        Some(Ok(m))
    }
}
