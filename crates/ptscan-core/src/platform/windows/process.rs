use std::{convert::TryFrom, ffi::c_void, ffi::OsString, io, mem, ptr};

use ntapi::ntpsapi;
use windows_sys::{
    core::BOOL,
    Win32::{
        Foundation::{self as winerror, FALSE, TRUE},
        System::{
            Diagnostics::Debug,
            Memory as memoryapi, ProcessStatus as psapi,
            Threading::{self, PROCESS_ACCESS_RIGHTS},
        },
    },
};

use super::{utils, Module};
use crate::{
    error::Error,
    process::{MemoryInformation, MemoryProtect, MemoryState, MemoryType, OpenProcessBuilder},
    Address, AddressRange, ProcessId, Size,
};

/// An open handle to a process.
pub(crate) struct RawProcess {
    pub(super) handle: utils::Handle,
}

impl RawProcess {
    pub(crate) fn open(options: &OpenProcessBuilder, process_id: ProcessId) -> Result<Self, Error> {
        let mut desired_access: PROCESS_ACCESS_RIGHTS = 0;

        if options.all_access {
            desired_access |= Threading::PROCESS_ALL_ACCESS;
        }

        if options.query_information {
            desired_access |= Threading::PROCESS_QUERY_INFORMATION;
        }

        if options.vm_read {
            desired_access |= Threading::PROCESS_VM_READ;
        }

        let inherit_handles: BOOL = if options.inherit_handles { TRUE } else { FALSE };

        let handle = unsafe { Threading::OpenProcess(desired_access, inherit_handles, process_id) };

        if handle.is_null() {
            return Err(Error::last_system_error());
        }

        Ok(Self {
            handle: utils::Handle::new(handle),
        })
    }

    pub(crate) fn is_64bit(&self) -> Result<bool, Error> {
        self.handle.is_64bit()
    }

    pub(crate) fn module_base_name(&self) -> io::Result<OsString> {
        utils::fixed_string(|buf, len| unsafe {
            psapi::GetModuleBaseNameW(*self.handle, ptr::null_mut(), buf, len)
        })
    }

    pub(crate) fn modules(&self) -> Result<Vec<Module>, Error> {
        let modules = utils::array(0x400, |buf, size, needed| unsafe {
            psapi::EnumProcessModules(*self.handle, buf, size, needed)
        })?;

        Ok(modules.into_iter().map(Module::new).collect())
    }

    pub(crate) fn write_memory(&self, address: Address, buffer: &[u8]) -> Result<bool, Error> {
        let mut bytes_written: usize = 0;

        let result = checked!(Debug::WriteProcessMemory(
            *self.handle,
            address.convert::<*mut c_void>()?,
            buffer.as_ptr() as *const c_void,
            buffer.len(),
            &mut bytes_written as *mut usize,
        ));

        match result {
            Ok(()) => Ok(bytes_written == buffer.len()),
            Err(ref e) if e.raw_os_error() == Some(winerror::ERROR_PARTIAL_COPY as i32) => {
                Ok(false)
            }
            Err(e) => Err(e),
        }
    }

    pub(crate) fn read_memory(&self, address: Address, buffer: &mut [u8]) -> Result<usize, Error> {
        let mut bytes_read: usize = 0;

        let result = checked!(Debug::ReadProcessMemory(
            *self.handle,
            address.convert::<*const c_void>()?,
            buffer.as_mut_ptr() as *mut c_void,
            buffer.len(),
            &mut bytes_read as *mut usize,
        ));

        match result {
            Ok(()) => Ok(bytes_read),
            Err(ref e) if e.raw_os_error() == Some(winerror::ERROR_PARTIAL_COPY as i32) => Ok(0),
            Err(ref e) if e.raw_os_error() == Some(winerror::ERROR_NOACCESS as i32) => Ok(0),
            Err(e) => Err(e),
        }
    }

    pub(crate) fn memory_regions(&self) -> anyhow::Result<Option<Vec<MemoryInformation>>> {
        Ok(None)
    }

    pub(crate) fn virtual_query(
        &self,
        address: Address,
    ) -> anyhow::Result<Option<MemoryInformation>> {
        const LENGTH: usize = mem::size_of::<memoryapi::MEMORY_BASIC_INFORMATION>();

        let mut mbi: memoryapi::MEMORY_BASIC_INFORMATION = unsafe { mem::zeroed() };

        let b = unsafe {
            memoryapi::VirtualQueryEx(
                *self.handle,
                address.convert()?,
                &mut mbi as *mut memoryapi::MEMORY_BASIC_INFORMATION,
                LENGTH,
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
            memoryapi::MEM_COMMIT => MemoryState::Commit,
            memoryapi::MEM_FREE => MemoryState::Free,
            memoryapi::MEM_RESERVE => MemoryState::Reserve,
            state => {
                return Err(Error::BadRegionState(state).into());
            }
        };

        let ty = match (state, mbi.Type) {
            (MemoryState::Free, _) => None,
            (_, memoryapi::MEM_IMAGE) => Some(MemoryType::Image),
            (_, memoryapi::MEM_MAPPED) => Some(MemoryType::Mapped),
            (_, memoryapi::MEM_PRIVATE) => Some(MemoryType::Private),
            (_, ty) => {
                return Err(Error::BadRegionType(ty).into());
            }
        };

        let mut protect = fixed_map::Set::new();

        if state != MemoryState::Free {
            decode_protect(mbi.Protect, &mut protect);
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

    pub(crate) fn suspend(&self) -> Result<(), Error> {
        let status = unsafe { ntpsapi::NtSuspendProcess(*self.handle as _) };

        if status != 0 {
            return Err(Error::last_system_error());
        }

        Ok(())
    }

    pub(crate) fn resume(&self) -> Result<(), Error> {
        let status = unsafe { ntpsapi::NtResumeProcess(*self.handle as _) };

        if status != 0 {
            return Err(Error::last_system_error());
        }

        Ok(())
    }
}

/// Test if opening a process failed because it no longer exists or we lack
/// access to it.
pub(crate) fn process_unavailable(error: &Error) -> bool {
    matches!(
        error.raw_os_error().map(|n| n as u32),
        Some(winerror::ERROR_INVALID_PARAMETER) | Some(winerror::ERROR_ACCESS_DENIED)
    )
}

macro_rules! protect_bits {
    ($flag:expr, $set:expr, {$($flag_from:ident => $flag_to:ident,)*}) => {
        $(if $flag & memoryapi::$flag_from != 0 {
            $set.insert(MemoryProtect::$flag_to);
        })*
    }
}

fn decode_protect(
    protect: memoryapi::PAGE_PROTECTION_FLAGS,
    set: &mut fixed_map::Set<MemoryProtect>,
) {
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
