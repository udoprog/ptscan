use std::{convert::TryFrom, fmt, io};

use failure::Error;

use crate::{
    module, system_info,
    utils::{array, drop_handle},
    Address, ProcessId, Size,
};

use winapi::{
    shared::{
        basetsd::SIZE_T,
        minwindef::{BOOL, DWORD, FALSE, LPCVOID, LPVOID, PBOOL, TRUE},
        winerror,
    },
    um::{memoryapi, processthreadsapi, psapi, winnt, wow64apiset},
};

/// A process handle.
pub struct Process {
    process_id: ProcessId,
    pub(crate) handle: winnt::HANDLE,
}

unsafe impl Sync for Process {}

impl Process {
    pub fn builder() -> OpenProcessBuilder {
        OpenProcessBuilder {
            desired_access: Default::default(),
            inherit_handles: FALSE,
        }
    }

    /// Open the specified process by process id with default options.
    pub fn open(process_id: ProcessId) -> Result<Process, Error> {
        Self::builder()
            .query_information()
            .vm_read()
            .build(process_id)
    }

    /// Get the process ID.
    pub fn process_id(&self) -> ProcessId {
        self.process_id
    }

    /// Enumerate all modules.
    pub fn modules<'a>(&'a self) -> Result<Vec<module::Module<'a>>, Error> {
        let modules = array(0x400, |buf, size, needed| unsafe {
            psapi::EnumProcessModules(self.handle, buf, size, needed)
        })?;

        let mut out = Vec::new();

        for module in modules {
            out.push(module::Module::new(self, module))
        }

        Ok(out)
    }

    /// Read the given structure from the given address.
    pub fn read<T>(&self, address: Address) -> Result<T, Error> {
        use std::{mem, slice};

        let mut out: T = unsafe { mem::zeroed() };

        let read = self.read_process_memory(address, unsafe {
            slice::from_raw_parts_mut(&mut out as *mut T as *mut u8, mem::size_of::<T>())
        })?;

        if read.len() != mem::size_of::<winnt::NT_TIB64>() {
            failure::bail!("failed to read tib");
        }

        Ok(out)
    }

    /// Read process memory at the specified address.
    pub fn read_process_memory<'a>(
        &self,
        address: Address,
        buffer: &'a mut [u8],
    ) -> Result<&'a mut [u8], Error> {
        let mut bytes_read: SIZE_T = 0;

        checked! {
            memoryapi::ReadProcessMemory(
                self.handle,
                address.convert::<LPCVOID>()?,
                buffer.as_mut_ptr() as LPVOID,
                buffer.len() as SIZE_T,
                &mut bytes_read as *mut SIZE_T,
            )
        };

        Ok(&mut buffer[..bytes_read])
    }

    /// Read a 64-bit memory region as a little endian unsigned integer.
    pub fn read_u64(&self, address: Address) -> Result<u64, Error> {
        use byteorder::{ByteOrder, LittleEndian};
        let mut out = [0u8; 8];
        let out = self.read_process_memory(address, &mut out)?;
        Ok(LittleEndian::read_u64(out))
    }

    /// Test if the process is a 32-bit process running under WOW64 compatibility.
    pub fn is_wow64(&self) -> Result<bool, Error> {
        let mut out: BOOL = FALSE;
        checked!(wow64apiset::IsWow64Process(self.handle, &mut out as PBOOL));
        Ok(out == TRUE)
    }

    /// Test if this is a 64-bit process.
    pub fn is_64bit(&self) -> Result<bool, Error> {
        if self.is_wow64()? {
            return Ok(false);
        }

        Ok(system_info::SystemInfo::get()?.arch.is_64bit())
    }

    /// Retrieve information about the memory page that corresponds to the given virtual `address`.
    pub fn virtual_query(&self, address: Address) -> Result<Option<MemoryInformation>, Error> {
        const LENGTH: SIZE_T = mem::size_of::<winnt::MEMORY_BASIC_INFORMATION>() as SIZE_T;

        use std::mem;
        let mut mbi: winnt::MEMORY_BASIC_INFORMATION = unsafe { mem::zeroed() };

        let b = unsafe {
            memoryapi::VirtualQueryEx(
                self.handle,
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

            return Err(Error::from(e));
        }

        let state = match mbi.State {
            winnt::MEM_COMMIT => MemoryState::Commit,
            winnt::MEM_FREE => MemoryState::Free,
            winnt::MEM_RESERVE => MemoryState::Reserve,
            state => failure::bail!("bad region state: {}", state),
        };

        let ty = match (state, mbi.Type) {
            (MemoryState::Free, _) => MemoryType::None,
            (_, winnt::MEM_IMAGE) => MemoryType::Image,
            (_, winnt::MEM_MAPPED) => MemoryType::Mapped,
            (_, winnt::MEM_PRIVATE) => MemoryType::Private,
            (_, ty) => failure::bail!("bad region type: {}", ty),
        };

        Ok(Some(MemoryInformation {
            base_address: Address::try_from(mbi.BaseAddress)?,
            region_size: Size::try_from(mbi.RegionSize)?,
            state,
            ty,
        }))
    }

    /// Iterate over all virtual memory segments with a given chunk size.
    pub fn virtual_memory_regions<'a>(&'a self) -> VirtualMemoryRegions<'a> {
        VirtualMemoryRegions {
            process: self,
            current: Address::new(0),
        }
    }
}

impl fmt::Debug for Process {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Process")
            .field("process_id", &self.process_id)
            .finish()
    }
}

impl Drop for Process {
    fn drop(&mut self) {
        drop_handle(self.handle);
    }
}

/// Builder to open a process.
#[derive(Default)]
pub struct OpenProcessBuilder {
    desired_access: DWORD,
    inherit_handles: BOOL,
}

impl OpenProcessBuilder {
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
            return Err(Error::from(io::Error::last_os_error()));
        }

        Ok(Process { process_id, handle })
    }
}

/// Enumerate all processes, returning their corresponding pids.
pub fn system_processes() -> Result<Vec<ProcessId>, Error> {
    array(0x400, |buf, size, needed| unsafe {
        psapi::EnumProcesses(buf, size, needed)
    })
}

#[derive(Debug, Clone, Copy)]
pub enum MemoryState {
    Commit,
    Free,
    Reserve,
}

#[derive(Debug, Clone, Copy)]
pub enum MemoryType {
    Image,
    Mapped,
    Private,
    None,
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

#[derive(Clone, Copy)]
pub struct MemoryInformation {
    pub base_address: Address,
    pub region_size: Size,
    pub state: MemoryState,
    pub ty: MemoryType,
}

impl fmt::Debug for MemoryInformation {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("AddressRange")
            .field("base_address", &self.base_address)
            .field("region_size", &self.region_size)
            .field("state", &self.state)
            .field("ty", &self.ty)
            .finish()
    }
}
/// Iterator over virtual memory segments.
pub struct VirtualMemoryRegions<'a> {
    process: &'a Process,
    current: Address,
}

impl<'a> Iterator for VirtualMemoryRegions<'a> {
    type Item = Result<MemoryInformation, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let memory_info = match self.process.virtual_query(self.current) {
            Ok(Some(memory_info)) => memory_info,
            Ok(None) => return None,
            Err(e) => return Some(Err(e)),
        };

        self.current = match self
            .current
            .add(memory_info.region_size)
            .and_then(|c| c.add(Size::new(1)))
        {
            Ok(current) => current,
            Err(e) => return Some(Err(e)),
        };

        return Some(Ok(memory_info));
    }
}
