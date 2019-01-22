use std::ffi::OsString;
use std::fmt;

use failure::Error;

use crate::process;

use winapi::{
    shared::minwindef::{DWORD, HMODULE},
    um::psapi,
};

pub struct Module<'a> {
    process: &'a process::Process,
    module: HMODULE,
}

impl fmt::Debug for Module<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Module").finish()
    }
}

impl<'a> Module<'a> {
    /// Construct a new module.
    ///
    /// This constructor is internal, and is used by `Process`.
    pub(crate) fn new(process: &'a process::Process, module: HMODULE) -> Self {
        Self { process, module }
    }

    /// Get the name of the module.
    pub fn name(&self) -> Result<OsString, Error> {
        crate::utils::string(|buf, len| unsafe {
            psapi::GetModuleBaseNameW(self.process.handle, self.module, buf, len)
        })
    }

    /// Get the information about the module.
    pub fn info(&self) -> Result<ModuleInfo, Error> {
        use std::mem;

        let mut out: psapi::MODULEINFO = unsafe { mem::zeroed() };

        checked! {
            psapi::GetModuleInformation(
                self.process.handle,
                self.module,
                &mut out as psapi::LPMODULEINFO,
                mem::size_of::<psapi::MODULEINFO>() as DWORD,
            )
        };

        Ok(ModuleInfo {
            base_of_dll: out.lpBaseOfDll as u64,
            size_of_image: out.SizeOfImage as u64,
            entry_point: out.EntryPoint as usize,
        })
    }
}

#[derive(Debug)]
pub struct ModuleInfo {
    pub base_of_dll: u64,
    pub size_of_image: u64,
    pub entry_point: usize,
}
