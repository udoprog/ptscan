use std::{convert::TryFrom, ffi::OsString, fmt, io};

use crate::{module::ModuleInfo, Address, Process, Size};

use windows_sys::Win32::{
    Foundation::HMODULE,
    System::{LibraryLoader, ProcessStatus as psapi},
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
        super::utils::fixed_string(|buf, len| unsafe {
            psapi::GetModuleBaseNameW(*process.handle.handle, self.module, buf, len)
        })
    }

    /// Get the file name corresponding to the module.
    pub fn file_name(&self) -> io::Result<OsString> {
        super::utils::growable_string(|buf, len| unsafe {
            LibraryLoader::GetModuleFileNameW(self.module, buf, len)
        })
    }

    /// Get the information about the module.
    pub fn info(&self, process: &Process) -> io::Result<ModuleInfo> {
        use std::mem;

        let mut out: psapi::MODULEINFO = unsafe { mem::zeroed() };

        let result = unsafe {
            psapi::GetModuleInformation(
                *process.handle.handle,
                self.module,
                &mut out as *mut psapi::MODULEINFO,
                mem::size_of::<psapi::MODULEINFO>() as u32,
            )
        };

        if result == 0 {
            return Err(io::Error::last_os_error());
        }

        Ok(ModuleInfo {
            base_of_dll: Address::try_from(out.lpBaseOfDll).map_err(io::Error::other)?,
            size_of_image: Size::from(out.SizeOfImage),
            entry_point: Address::try_from(out.EntryPoint).map_err(io::Error::other)?,
        })
    }
}
