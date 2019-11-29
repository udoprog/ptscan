use std::{convert::TryFrom, ffi::OsString, fmt, io, ptr, slice, sync::Arc};

use crate::{error::Error, module, utils, Address, AddressRange, Endianness, ProcessId, Size};

use ntapi::ntpsapi;
use serde::{Deserialize, Serialize};
use winapi::{
    shared::{
        basetsd::SIZE_T,
        minwindef::{BOOL, DWORD, FALSE, LPCVOID, LPVOID, TRUE},
        winerror,
    },
    um::{memoryapi, processthreadsapi, psapi, winnt},
};

#[derive(Debug, Clone, Copy)]
pub enum PointerWidth {
    Pointer32,
    Pointer64,
}

impl PointerWidth {
    /// The size of the pointer in bytes.
    pub fn size(self) -> usize {
        match self {
            Self::Pointer32 => 4,
            Self::Pointer64 => 8,
        }
    }
}

pub trait MemoryReader {
    type Process: ProcessInfo;

    fn read_memory(&self, address: Address, buf: &mut [u8]) -> anyhow::Result<usize>;

    fn process(&self) -> &Self::Process;
}

impl MemoryReader for Process {
    type Process = Process;

    fn read_memory(&self, address: Address, buf: &mut [u8]) -> anyhow::Result<usize> {
        Ok(self.read_process_memory(address, buf)?)
    }

    fn process(&self) -> &Process {
        self
    }
}

pub trait ProcessInfo {
    type Process: ProcessInfo;
    type ByteOrder: byteorder::ByteOrder;

    fn process(&self) -> &Self::Process;

    /// The pointer width of the process.
    fn pointer_width(&self) -> PointerWidth;

    /// Access information on virtual memory for the given address.
    fn virtual_query(&self, address: Address) -> anyhow::Result<Option<MemoryInformation>>;

    /// Access virtual memory regions of the process.
    fn virtual_memory_regions(&self) -> VirtualMemoryRegions<'_, Self::Process> {
        VirtualMemoryRegions {
            process: self.process(),
            current: Address::null(),
        }
    }

    /// Decode the given buffer as a pointer.
    fn decode_pointer(&self, buf: &[u8]) -> Address {
        assert!(
            buf.len() == self.pointer_width().size(),
            "{} (buffer length) != {} (pointer width)",
            buf.len(),
            self.pointer_width().size()
        );

        match self.pointer_width() {
            PointerWidth::Pointer64 => {
                let address = <Self::ByteOrder as byteorder::ByteOrder>::read_u64(buf);
                Address(address)
            }
            PointerWidth::Pointer32 => {
                let address = <Self::ByteOrder as byteorder::ByteOrder>::read_u32(buf);
                Address(address as u64)
            }
        }
    }

    /// Encode a pointer into the specified buffer.
    ///
    /// Returns a boolean indicating if the pointer could be encoded.
    fn encode_pointer(&self, buf: &mut [u8], value: u64) -> bool {
        match self.pointer_width() {
            PointerWidth::Pointer64 => {
                <Self::ByteOrder as byteorder::ByteOrder>::write_u64(buf, value)
            }
            PointerWidth::Pointer32 => {
                let value = match u32::try_from(value) {
                    Ok(value) => value,
                    Err(..) => return false,
                };

                <Self::ByteOrder as byteorder::ByteOrder>::write_u32(buf, value);
            }
        }

        true
    }
}

/// A process handle.
#[derive(Clone)]
pub struct Process {
    pub process_id: ProcessId,
    pub is_64bit: bool,
    pub pointer_width: PointerWidth,
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

    /// Get the name of the process.
    pub fn module_base_name(&self) -> io::Result<OsString> {
        crate::utils::fixed_string(|buf, len| unsafe {
            psapi::GetModuleBaseNameW(**self.handle, ptr::null_mut(), buf, len)
        })
    }

    /// Get the process ID.
    pub fn process_id(&self) -> ProcessId {
        self.process_id
    }

    /// Enumerate all modules.
    pub fn modules<'a>(&'a self) -> Result<Vec<module::Module>, Error> {
        let modules = utils::array(0x400, |buf, size, needed| unsafe {
            psapi::EnumProcessModules(**self.handle, buf, size, needed)
        })?;

        let mut out = Vec::new();

        for module in modules {
            out.push(module::Module::new(module))
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
    pub fn read_process_memory(&self, address: Address, buffer: &mut [u8]) -> Result<usize, Error> {
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
    pub fn virtual_query(&self, address: Address) -> anyhow::Result<Option<MemoryInformation>> {
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

            return Err(e.into());
        }

        let state = match mbi.State {
            winnt::MEM_COMMIT => MemoryState::Commit,
            winnt::MEM_FREE => MemoryState::Free,
            winnt::MEM_RESERVE => MemoryState::Reserve,
            state => {
                return Err(Error::BadRegionState(state).into());
            }
        };

        let ty = match (state, mbi.Type) {
            (MemoryState::Free, _) => None,
            (_, winnt::MEM_IMAGE) => Some(MemoryType::Image),
            (_, winnt::MEM_MAPPED) => Some(MemoryType::Mapped),
            (_, winnt::MEM_PRIVATE) => Some(MemoryType::Private),
            (_, ty) => {
                return Err(Error::BadRegionType(ty).into());
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

impl ProcessInfo for Process {
    type Process = Process;
    type ByteOrder = byteorder::NativeEndian;

    fn process(&self) -> &Self::Process {
        self
    }

    fn pointer_width(&self) -> PointerWidth {
        self.pointer_width
    }

    fn virtual_query(&self, address: Address) -> anyhow::Result<Option<MemoryInformation>> {
        Process::virtual_query(self, address)
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

        let pointer_width = if is_64bit {
            PointerWidth::Pointer64
        } else {
            PointerWidth::Pointer32
        };
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
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

    pub fn as_str(self) -> &'static str {
        match self {
            Self::Commit => "MEM_COMMIT",
            Self::Free => "MEM_FREE",
            Self::Reserve => "MEM_RESERVE",
        }
    }
}

impl fmt::Display for MemoryState {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(fmt)
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MemoryType {
    Image,
    Mapped,
    Private,
}

impl MemoryType {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Image => "MEM_IMAGE",
            Self::Mapped => "MEM_MAPPED",
            Self::Private => "MEM_PRIVATE",
        }
    }
}

impl fmt::Display for MemoryType {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(fmt)
    }
}

#[derive(fixed_map::Key, Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
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

macro_rules! as_str {
    ($slf:expr, {$($flag_from:ident => $flag_to:ident,)*}) => {
        match $slf {
            $(Self::$flag_from => stringify!($flag_to),)*
        }
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
            PAGE_READWRITE => ReadWrite,
            PAGE_WRITECOPY => WriteCopy,
            PAGE_TARGETS_INVALID => TargetsInvalid,
            PAGE_TARGETS_NO_UPDATE => TargetsNoUpdate,
            PAGE_GUARD => Guard,
            PAGE_NOCACHE => NoCache,
            PAGE_WRITECOMBINE => WriteCombine,
        });
    }

    /// Get the string representation of this proection mode.
    pub fn as_str(self) -> &'static str {
        as_str!(self, {
            Execute => PAGE_EXECUTE,
            ExecuteRead => PAGE_EXECUTE_READ,
            ExecuteReadWrite => PAGE_EXECUTE_READWRITE,
            ExecuteWriteCopy => PAGE_EXECUTE_WRITECOPY,
            NoAccess => PAGE_NOACCESS,
            ReadOnly => PAGE_READONLY,
            ReadWrite => PAGE_READWRITE,
            WriteCopy => PAGE_WRITECOPY,
            TargetsInvalid => PAGE_TARGETS_INVALID,
            TargetsNoUpdate => PAGE_TARGETS_NO_UPDATE,
            Guard => PAGE_GUARD,
            NoCache => PAGE_NOCACHE,
            WriteCombine => PAGE_WRITECOMBINE,
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryInformation {
    pub range: AddressRange,
    pub state: MemoryState,
    #[serde(rename = "type", default, skip_serializing_if = "Option::is_none")]
    pub ty: Option<MemoryType>,
    pub protect: fixed_map::Set<MemoryProtect>,
}

impl MemoryInformation {
    /// Test if region is writable.
    pub fn is_readable(&self) -> bool {
        use self::MemoryProtect::*;

        self.state == MemoryState::Commit
            && !self.is_protect_any(&[NoAccess, TargetsInvalid, TargetsNoUpdate, Guard, NoCache])
    }

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
        self.state == MemoryState::Commit && self.is_protect_any(&[WriteCopy, ExecuteWriteCopy])
    }

    /// Test if protection contains any of the specified elements.
    fn is_protect_any<'a>(&self, it: impl IntoIterator<Item = &'a MemoryProtect>) -> bool {
        it.into_iter().any(|p| self.protect.contains(*p))
    }
}

impl fmt::Display for MemoryInformation {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{} {}", self.range, self.state)?;

        if let Some(ty) = self.ty {
            write!(fmt, " {}", ty)?;
        }

        let protect = self.protect.iter().map(|p| p.as_str()).collect::<Vec<_>>();

        if !protect.is_empty() {
            write!(fmt, "{}", protect.join(","))?;
        }

        Ok(())
    }
}

/// Iterator over virtual memory regions.
pub struct VirtualMemoryRegions<'a, P> {
    process: &'a P,
    current: Address,
}

impl<'a, P> Iterator for VirtualMemoryRegions<'a, P>
where
    P: ProcessInfo,
{
    type Item = anyhow::Result<MemoryInformation>;

    fn next(&mut self) -> Option<Self::Item> {
        let m = try_iter!(self.process.virtual_query(self.current))?;
        self.current = self
            .current
            .checked_size(m.range.size)?
            .checked_size(Size::new(1))?;
        Some(Ok(m))
    }
}
