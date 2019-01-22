use std::{fmt, io};

use failure::Error;

use crate::{process, utils::drop_handle, VirtualAddress};

use winapi::{
    shared::{
        minwindef::{BOOL, DWORD, FALSE, TRUE},
        ntdef,
    },
    um::{processthreadsapi, winbase, winnt},
};

pub type ThreadId = DWORD;

/// A thread being iterated over.
pub struct Thread {
    thread_id: ThreadId,
    handle: winnt::HANDLE,
}

impl Thread {
    /// Set up an open thread builder.
    pub fn builder() -> OpenThreadBuilder {
        OpenThreadBuilder {
            desired_access: Default::default(),
            inherit_handles: FALSE,
        }
    }

    /// Open the specified thread by thread id with default options.
    pub fn open(thread_id: ThreadId) -> Result<Thread, Error> {
        Self::builder().query_information().build(thread_id)
    }

    /// Access the thread ID for the thread.
    pub fn thread_id(&self) -> ThreadId {
        self.thread_id
    }

    /// Access the private address range of a thread to decode its stack.
    pub fn thread_stack(&self, process: &process::Process) -> Result<VirtualAddress, Error> {
        use ntapi::ntpsapi::{
            NtQueryInformationThread, ThreadBasicInformation, THREAD_BASIC_INFORMATION,
        };
        use std::mem;

        let mut thread_info: THREAD_BASIC_INFORMATION = unsafe { mem::zeroed() };
        let mut length: DWORD = 0;

        let status = unsafe {
            NtQueryInformationThread(
                self.handle,
                ThreadBasicInformation,
                &mut thread_info as *mut _ as ntdef::PVOID,
                mem::size_of::<THREAD_BASIC_INFORMATION>() as DWORD,
                &mut length as *mut _,
            )
        };

        if status != 0 {
            failure::bail!("{}: failed to resolve address", status);
        }

        Ok(
            process.read_u64(thread_info.TebBaseAddress as VirtualAddress + 0x08)?
                as VirtualAddress,
        )
    }

    /// Get the context for a thread.
    pub fn get_context<'a>(&'a self) -> Result<ThreadContext<'a>, Error> {
        use std::mem;

        let mut context: winnt::CONTEXT = unsafe { mem::zeroed() };
        context.ContextFlags = winnt::CONTEXT_SEGMENTS;

        checked! {
            processthreadsapi::GetThreadContext(self.handle, &mut context as winnt::PCONTEXT)
        };

        Ok(ThreadContext {
            thread: self,
            context,
        })
    }
}

impl fmt::Debug for Thread {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Thread")
            .field("thread_id", &self.thread_id)
            .finish()
    }
}

impl Drop for Thread {
    fn drop(&mut self) {
        drop_handle(self.handle);
    }
}

pub struct ThreadContext<'a> {
    thread: &'a Thread,
    context: winnt::CONTEXT,
}

impl ThreadContext<'_> {
    /// Find where the thread stack is located.
    pub fn thread_stack(&self, process: &process::Process) -> Result<VirtualAddress, Error> {
        use byteorder::{ByteOrder, LittleEndian};

        use std::mem;
        let mut entry: winnt::LDT_ENTRY = unsafe { mem::zeroed() };

        let seg_fs = self.context.SegFs as DWORD;

        checked! {
            winbase::GetThreadSelectorEntry(
                self.thread.handle,
                seg_fs,
                &mut entry as *mut _ as winbase::LPLDT_ENTRY,
            )
        };

        let base = unsafe {
            let bytes = entry.HighWord.Bytes();

            entry.BaseLow as VirtualAddress
                + ((bytes.BaseMid as VirtualAddress) << 0x10)
                + ((bytes.BaseHi as VirtualAddress) << 0x18)
                + 0x4
        };

        let mut out = [0u8; 4];
        process.read_process_memory(base, &mut out)?;

        Ok(LittleEndian::read_u64(&out) as VirtualAddress)
    }
}

impl fmt::Debug for ThreadContext<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("ThreadContext").finish()
    }
}

/// Builder to open a process.
#[derive(Default)]
pub struct OpenThreadBuilder {
    desired_access: DWORD,
    inherit_handles: BOOL,
}

impl OpenThreadBuilder {
    /// Access everything.
    pub fn all_access(self) -> Self {
        OpenThreadBuilder {
            desired_access: self.desired_access | winnt::THREAD_ALL_ACCESS,
            ..self
        }
    }

    /// Set the THREAD_GET_CONTEXT desired access flag.
    pub fn get_context(self) -> Self {
        OpenThreadBuilder {
            desired_access: self.desired_access | winnt::THREAD_GET_CONTEXT,
            ..self
        }
    }

    /// Set the query information flag.
    pub fn query_information(self) -> Self {
        OpenThreadBuilder {
            desired_access: self.desired_access | winnt::THREAD_QUERY_INFORMATION,
            ..self
        }
    }

    /// Indicate that the process should inherit handles.
    pub fn inherit_handles(self) -> Self {
        OpenThreadBuilder {
            inherit_handles: TRUE,
            ..self
        }
    }

    /// Build the process handle.
    pub fn build(self, thread_id: ThreadId) -> Result<Thread, Error> {
        let handle = unsafe {
            processthreadsapi::OpenThread(self.desired_access, self.inherit_handles, thread_id)
        };

        if handle.is_null() {
            return Err(Error::from(io::Error::last_os_error()));
        }

        Ok(Thread { thread_id, handle })
    }
}
