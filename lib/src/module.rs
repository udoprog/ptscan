use std::{convert::TryFrom, ffi::OsString, fmt, io};

use crate::{Address, Process, Size};

use winapi::{
    shared::minwindef::{DWORD, HMODULE},
    um::{libloaderapi, psapi},
};

pub struct Module {
    module: HMODULE,
}

impl fmt::Debug for Module {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Module").finish()
    }
}

impl Module {
    /// Construct a new module.
    ///
    /// This constructor is internal, and is used by `Process`.
    pub(crate) fn new(module: HMODULE) -> Self {
        Self { module }
    }

    /// Get the name of the module.
    pub fn name(&self, process: &Process) -> io::Result<OsString> {
        crate::utils::fixed_string(|buf, len| unsafe {
            psapi::GetModuleBaseNameW(**process.handle, self.module, buf, len)
        })
    }

    /// Get the file name corresponding to the module.
    pub fn file_name(&self) -> io::Result<OsString> {
        crate::utils::growable_string(|buf, len| unsafe {
            libloaderapi::GetModuleFileNameW(self.module, buf, len)
        })
    }

    /// Get the information about the module.
    pub fn info(&self, process: &Process) -> io::Result<ModuleInfo> {
        use std::mem;

        let mut out: psapi::MODULEINFO = unsafe { mem::zeroed() };

        let result = unsafe {
            psapi::GetModuleInformation(
                **process.handle,
                self.module,
                &mut out as psapi::LPMODULEINFO,
                mem::size_of::<psapi::MODULEINFO>() as DWORD,
            )
        };

        if result == 0 {
            return Err(io::Error::last_os_error());
        }

        Ok(ModuleInfo {
            base_of_dll: Address::try_from(out.lpBaseOfDll)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?,
            size_of_image: Size::from(out.SizeOfImage),
            entry_point: Address::try_from(out.EntryPoint)
                .map_err(|e| io::Error::new(io::ErrorKind::Other, e))?,
        })
    }
}

#[derive(Debug)]
pub struct ModuleInfo {
    pub base_of_dll: Address,
    pub size_of_image: Size,
    pub entry_point: Address,
}
