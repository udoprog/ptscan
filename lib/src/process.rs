use std::{convert::TryFrom, ffi, fmt, io, ptr, sync::Arc};

use crate::{module, system, utils, Address, AddressRange, ProcessId, Size, Type, Value};

use err_derive::Error;
use ntapi::ntpsapi;
use winapi::{
    shared::{
        basetsd::SIZE_T,
        minwindef::{BOOL, DWORD, FALSE, LPCVOID, LPVOID, PBOOL, TRUE},
        winerror,
    },
    um::{memoryapi, processthreadsapi, psapi, winnt, wow64apiset},
};

#[derive(Debug, Error)]
pub enum Error {
    #[error(display = "bad region state: {}", _0)]
    BadRegionState(DWORD),
    #[error(display = "bad region type: {}", _0)]
    BadRegionType(DWORD),
    #[error(
        display = "read underflow, expected `{}` but got `{}`",
        expected,
        actual
    )]
    ReadUnderflow { expected: usize, actual: usize },
    #[error(
        display = "write underflow, expected `{}` but got `{}`",
        expected,
        actual
    )]
    WriteUnderflow { expected: usize, actual: usize },
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

    /// Get the name of the process.
    pub fn module_base_name(&self) -> Result<ffi::OsString, io::Error> {
        crate::utils::string(|buf, len| unsafe {
            psapi::GetModuleBaseNameW(**self.handle, ptr::null_mut(), buf, len)
        })
    }

    /// Get the process ID.
    pub fn process_id(&self) -> ProcessId {
        self.process_id
    }

    /// Enumerate all modules.
    pub fn modules<'a>(&'a self) -> Result<Vec<module::Module<'a>>, io::Error> {
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
    pub fn read<T>(&self, address: Address) -> Result<T, io::Error> {
        use std::{mem, slice};

        let mut out: T = unsafe { mem::zeroed() };
        let expected = mem::size_of::<T>();

        let actual = self.read_process_memory(address, unsafe {
            slice::from_raw_parts_mut(&mut out as *mut T as *mut u8, mem::size_of::<T>())
        })?;

        if expected != actual {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                Error::ReadUnderflow { expected, actual },
            ));
        }

        Ok(out)
    }

    /// Write to process memory at the specified address.
    pub fn write_process_memory<'a>(
        &self,
        address: Address,
        buffer: &'a [u8],
    ) -> Result<(), io::Error> {
        let mut actual: SIZE_T = 0;

        checked! {
            memoryapi::WriteProcessMemory(
                **self.handle,
                address.convert::<LPVOID>()?,
                buffer.as_ptr() as LPCVOID,
                buffer.len() as SIZE_T,
                &mut actual as *mut SIZE_T,
            )
        };

        let actual = actual as usize;

        if actual != buffer.len() {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                Error::WriteUnderflow {
                    expected: buffer.len(),
                    actual,
                },
            ));
        }

        Ok(())
    }

    /// Read process memory at the specified address.
    pub fn read_process_memory<'a>(
        &self,
        address: Address,
        buffer: &'a mut [u8],
    ) -> Result<usize, io::Error> {
        let mut bytes_read: SIZE_T = 0;

        checked! {
            memoryapi::ReadProcessMemory(
                **self.handle,
                address.convert::<LPCVOID>()?,
                buffer.as_mut_ptr() as LPVOID,
                buffer.len() as SIZE_T,
                &mut bytes_read as *mut SIZE_T,
            )
        };

        Ok(bytes_read)
    }

    /// Read process memory with the given type.
    pub fn read_memory_of_type(
        &self,
        address: Address,
        ty: Type,
        buf: &mut [u8],
    ) -> Result<Value, io::Error> {
        let size = ty.size();
        let bytes_read = self.read_process_memory(address, buf)?;

        if bytes_read != size {
            return Err(io::Error::new(io::ErrorKind::Other, "incomplete read"));
        }

        Ok(ty.decode(buf))
    }

    /// Read a 64-bit memory region as a little endian unsigned integer.
    pub fn read_u64(&self, address: Address) -> Result<u64, io::Error> {
        use byteorder::{ByteOrder, LittleEndian};
        let mut out = [0u8; 8];
        self.read_process_memory(address, &mut out)?;
        Ok(LittleEndian::read_u64(&out))
    }

    /// Test if the process is a 32-bit process running under WOW64 compatibility.
    pub fn is_wow64(&self) -> Result<bool, io::Error> {
        let mut out: BOOL = FALSE;
        checked!(wow64apiset::IsWow64Process(
            **self.handle,
            &mut out as PBOOL
        ));
        Ok(out == TRUE)
    }

    /// Test if this is a 64-bit process.
    pub fn is_64bit(&self) -> Result<bool, failure::Error> {
        if self.is_wow64()? {
            return Ok(false);
        }

        Ok(system::info()?.arch.is_64bit())
    }

    /// Retrieve information about the memory page that corresponds to the given virtual `address`.
    pub fn virtual_query(&self, address: Address) -> Result<Option<MemoryInformation>, io::Error> {
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
            let e = io::Error::last_os_error();

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
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    Error::BadRegionState(state),
                ));
            }
        };

        let ty = match (state, mbi.Type) {
            (MemoryState::Free, _) => MemoryType::None,
            (_, winnt::MEM_IMAGE) => MemoryType::Image,
            (_, winnt::MEM_MAPPED) => MemoryType::Mapped,
            (_, winnt::MEM_PRIVATE) => MemoryType::Private,
            (_, ty) => {
                return Err(io::Error::new(
                    io::ErrorKind::Other,
                    Error::BadRegionType(ty),
                ));
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
    pub fn suspend(&self) -> Result<(), io::Error> {
        let status = unsafe { ntpsapi::NtSuspendProcess(**self.handle) };

        if status != 0 {
            return Err(io::Error::last_os_error());
        }

        Ok(())
    }

    /// Resume the process.
    pub fn resume(&self) -> Result<(), io::Error> {
        let status = unsafe { ntpsapi::NtResumeProcess(**self.handle) };

        if status != 0 {
            return Err(io::Error::last_os_error());
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
    pub fn build(self, process_id: ProcessId) -> Result<Process, io::Error> {
        let handle = unsafe {
            processthreadsapi::OpenProcess(self.desired_access, self.inherit_handles, process_id)
        };

        if handle.is_null() {
            return Err(io::Error::last_os_error());
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
    type Item = Result<MemoryInformation, io::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let m = try_iter!(self.process.virtual_query(self.current))?;
        self.current = try_iter!(try_iter!(self.current.add(m.range.size)).add(Size::new(1)));
        return Some(Ok(m));
    }
}
