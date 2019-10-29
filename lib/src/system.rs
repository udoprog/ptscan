//! Utilities to query the system for things.

use std::{fmt, io};

use crate::{error::Error, utils, ProcessId, ThreadId};

use winapi::{
    shared::minwindef::{DWORD, FALSE},
    um::{handleapi, psapi, sysinfoapi, tlhelp32, winnt},
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

    if handle == handleapi::INVALID_HANDLE_VALUE {
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
    unsafe { sysinfoapi::GetNativeSystemInfo(&mut out as sysinfoapi::LPSYSTEM_INFO) };

    let arch = match unsafe { out.u.s() }.wProcessorArchitecture {
        winnt::PROCESSOR_ARCHITECTURE_AMD64 => Arch::Amd64,
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

/// A thread in the system with an associated parent process.
pub struct Thread {
    process_id: ProcessId,
    thread_id: ThreadId,
}

impl Thread {
    /// Access the process ID the thread belongs to.
    pub fn process_id(&self) -> ProcessId {
        self.process_id
    }

    /// Access the underlying thread.
    pub fn thread_id(&self) -> ThreadId {
        self.thread_id
    }
}

impl fmt::Debug for Thread {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Thread")
            .field("process_id", &self.process_id)
            .field("thread_id", &self.thread_id)
            .finish()
    }
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
        use tlhelp32::{LPTHREADENTRY32, THREADENTRY32};

        let e = match self.entry.as_mut() {
            Some(e) => {
                e.dwSize = mem::size_of::<THREADENTRY32>() as DWORD;

                if unsafe { tlhelp32::Thread32Next(*self.handle, e as LPTHREADENTRY32) } == FALSE {
                    return None;
                }

                e
            }
            None => {
                let mut e: tlhelp32::THREADENTRY32 = unsafe { mem::zeroed() };
                e.dwSize = mem::size_of::<THREADENTRY32>() as DWORD;
                let e = self.entry.get_or_insert(e);

                if unsafe { tlhelp32::Thread32First(*self.handle, e as LPTHREADENTRY32) } == FALSE {
                    return None;
                }

                e
            }
        };

        return Some(Thread {
            process_id: e.th32OwnerProcessID,
            thread_id: e.th32ThreadID,
        });
    }
}
