use std::{convert::TryFrom, ffi::OsString, fmt};

use crate::{error::Error, process, Address, Size};

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
            psapi::GetModuleBaseNameW(**self.process.handle, self.module, buf, len)
        })
    }

    /// Get the information about the module.
    pub fn info(&self) -> Result<ModuleInfo, Error> {
        use std::mem;

        let mut out: psapi::MODULEINFO = unsafe { mem::zeroed() };

        checked!(psapi::GetModuleInformation(
            **self.process.handle,
            self.module,
            &mut out as psapi::LPMODULEINFO,
            mem::size_of::<psapi::MODULEINFO>() as DWORD,
        ))?;

        Ok(ModuleInfo {
            base_of_dll: Address::try_from(out.lpBaseOfDll)?,
            size_of_image: Size::from(out.SizeOfImage),
            entry_point: Address::try_from(out.EntryPoint)?,
        })
    }
}

#[derive(Debug)]
pub struct ModuleInfo {
    pub base_of_dll: Address,
    pub size_of_image: Size,
    pub entry_point: Address,
}
