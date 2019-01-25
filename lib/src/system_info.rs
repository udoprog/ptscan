//! Tools to get information about the current system.

use winapi::um::{sysinfoapi, winnt};

#[derive(Debug)]
pub enum Arch {
    Amd64,
    Other,
}

impl Arch {
    pub fn is_64bit(&self) -> bool {
        match *self {
            Arch::Amd64 => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct SystemInfo {
    pub arch: Arch,
}

impl SystemInfo {
    pub fn get() -> Result<SystemInfo, failure::Error> {
        use std::mem;
        let mut out: sysinfoapi::SYSTEM_INFO = unsafe { mem::zeroed() };
        unsafe { sysinfoapi::GetNativeSystemInfo(&mut out as sysinfoapi::LPSYSTEM_INFO) };

        let arch = match unsafe { out.u.s() }.wProcessorArchitecture {
            winnt::PROCESSOR_ARCHITECTURE_AMD64 => Arch::Amd64,
            _ => Arch::Other,
        };

        Ok(SystemInfo { arch })
    }
}
