use std::io;

use super::utils;
use crate::{error::Error, system::Thread, ProcessId};

use windows_sys::Win32::{
    Foundation::{FALSE, INVALID_HANDLE_VALUE},
    System::{
        Diagnostics::ToolHelp as tlhelp32, ProcessStatus as psapi, SystemInformation as sysinfoapi,
    },
};

/// Enumerate all processes, returning their corresponding pids.
pub fn processes() -> Result<Vec<ProcessId>, Error> {
    utils::array(0x400, |buf, size, needed| unsafe {
        psapi::EnumProcesses(buf, size, needed)
    })
}

/// Enumerate all threads of the system.
pub fn threads() -> Result<Threads, Error> {
    use tlhelp32::{CreateToolhelp32Snapshot, TH32CS_SNAPTHREAD};

    let handle = unsafe { CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0) };

    if handle == INVALID_HANDLE_VALUE {
        return Err(Error::System(io::Error::last_os_error()));
    }

    let handle = utils::Handle::new(handle);

    Ok(Threads {
        handle,
        entry: None,
    })
}

/// Get general system information.
pub fn info() -> Result<Info, Error> {
    use std::mem;
    let mut out: sysinfoapi::SYSTEM_INFO = unsafe { mem::zeroed() };
    unsafe { sysinfoapi::GetNativeSystemInfo(&mut out as *mut sysinfoapi::SYSTEM_INFO) };

    let arch = match unsafe { out.Anonymous.Anonymous }.wProcessorArchitecture {
        sysinfoapi::PROCESSOR_ARCHITECTURE_AMD64 => Arch::Amd64,
        _ => Arch::Other,
    };

    Ok(Info { arch })
}

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
pub struct Info {
    pub arch: Arch,
}

/// An iterator over all system threads.
pub struct Threads {
    handle: utils::Handle,
    entry: Option<tlhelp32::THREADENTRY32>,
}

impl<'a> Iterator for Threads {
    type Item = Thread;

    fn next(&mut self) -> Option<Self::Item> {
        use std::mem;
        use tlhelp32::THREADENTRY32;

        let e = match self.entry.as_mut() {
            Some(e) => {
                e.dwSize = mem::size_of::<THREADENTRY32>() as u32;

                if unsafe { tlhelp32::Thread32Next(*self.handle, e as *mut THREADENTRY32) } == FALSE
                {
                    return None;
                }

                e
            }
            None => {
                let mut e: tlhelp32::THREADENTRY32 = unsafe { mem::zeroed() };
                e.dwSize = mem::size_of::<THREADENTRY32>() as u32;
                let e = self.entry.get_or_insert(e);

                if unsafe { tlhelp32::Thread32First(*self.handle, e as *mut THREADENTRY32) }
                    == FALSE
                {
                    return None;
                }

                e
            }
        };

        Some(Thread::new(e.th32OwnerProcessID, e.th32ThreadID))
    }
}
