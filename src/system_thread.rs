use std::{fmt, io};

use failure::Error;

use crate::{utils::drop_handle, ProcessId, ThreadId};

use winapi::{
    shared::minwindef::{DWORD, FALSE},
    um::{handleapi, tlhelp32, winnt},
};

/// A thread in the system with an associated parent process.
pub struct SystemThread {
    process_id: ProcessId,
    thread_id: ThreadId,
}

impl SystemThread {
    /// Access the process ID the thread belongs to.
    pub fn process_id(&self) -> ProcessId {
        self.process_id
    }

    /// Access the underlying thread.
    pub fn thread_id(&self) -> ThreadId {
        self.thread_id
    }
}

impl fmt::Debug for SystemThread {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Thread")
            .field("process_id", &self.process_id)
            .field("thread_id", &self.thread_id)
            .finish()
    }
}

/// An iterator over all system threads.
pub struct SystemThreads {
    handle: winnt::HANDLE,
    entry: Option<tlhelp32::THREADENTRY32>,
}

impl<'a> Iterator for SystemThreads {
    type Item = SystemThread;

    fn next(&mut self) -> Option<Self::Item> {
        use std::mem;
        use tlhelp32::{LPTHREADENTRY32, THREADENTRY32};

        let e = match self.entry.as_mut() {
            Some(e) => {
                e.dwSize = mem::size_of::<THREADENTRY32>() as DWORD;

                if unsafe { tlhelp32::Thread32Next(self.handle, e as LPTHREADENTRY32) } == FALSE {
                    return None;
                }

                e
            }
            None => {
                let mut e: tlhelp32::THREADENTRY32 = unsafe { mem::zeroed() };
                e.dwSize = mem::size_of::<THREADENTRY32>() as DWORD;
                let e = self.entry.get_or_insert(e);

                if unsafe { tlhelp32::Thread32First(self.handle, e as LPTHREADENTRY32) } == FALSE {
                    return None;
                }

                e
            }
        };

        return Some(SystemThread {
            process_id: e.th32OwnerProcessID,
            thread_id: e.th32ThreadID,
        });
    }
}

impl Drop for SystemThreads {
    fn drop(&mut self) {
        drop_handle(self.handle);
    }
}

/// Enumerate all threads of the system.
pub fn system_threads() -> Result<SystemThreads, Error> {
    use tlhelp32::{CreateToolhelp32Snapshot, TH32CS_SNAPTHREAD};

    let handle = unsafe { CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0) };

    if handle == handleapi::INVALID_HANDLE_VALUE {
        return Err(Error::from(io::Error::last_os_error()));
    }

    Ok(SystemThreads {
        handle,
        entry: None,
    })
}
