use std::{convert::TryFrom, fmt, io, mem, sync::Arc};

use crate::{error::Error, process, utils, Address, AddressRange, ThreadId};

use anyhow::bail;
use winapi::{
    shared::{
        minwindef::{BOOL, DWORD, FALSE, TRUE},
        ntdef,
    },
    um::{processthreadsapi, winbase, winnt},
};

/// A thread in the system.
#[derive(Clone)]
pub struct Thread {
    thread_id: ThreadId,
    handle: Arc<utils::Handle>,
}

impl Thread {
    /// Set up an open thread builder.
    pub fn builder() -> OpenThreadBuilder {
        OpenThreadBuilder {
            desired_access: Default::default(),
            inherit_handles: FALSE,
        }
    }

    /// Get the specified thread selector entry.
    pub fn thread_selector_entry(&self, selector: DWORD) -> Result<winnt::LDT_ENTRY, Error> {
        let mut entry: winnt::LDT_ENTRY = unsafe { mem::zeroed() };

        checked!(winbase::GetThreadSelectorEntry(
            **self.handle,
            selector,
            &mut entry as *mut _ as winbase::LPLDT_ENTRY,
        ))?;

        Ok(entry)
    }

    /// Open the specified thread by thread id with default options.
    pub fn open(thread_id: ThreadId) -> Result<Thread, Error> {
        Self::builder().query_information().build(thread_id)
    }

    /// Access the thread ID for the thread.
    pub fn thread_id(&self) -> ThreadId {
        self.thread_id
    }

    /// Access the thread stack from a 64-bit process.
    pub fn thread_stack(&self, process: &process::Process) -> anyhow::Result<AddressRange> {
        let tib = self.thread_teb::<winnt::NT_TIB64>(process)?;

        let stack_base = Address::from(tib.StackBase);
        let stack_limit = Address::from(tib.StackLimit);

        let size = match stack_base.size_from(stack_limit) {
            Some(size) => size,
            None => bail!(
                "stack base `{}` is smaller than stack limit `{}`",
                stack_base,
                stack_limit
            ),
        };

        Ok(AddressRange {
            base: stack_limit,
            size,
        })
    }

    /// Access the private address range of a thread to decode its stack.
    fn thread_teb<T>(&self, process: &process::Process) -> Result<T, Error> {
        use ntapi::ntpsapi::{
            NtQueryInformationThread, ThreadBasicInformation, THREAD_BASIC_INFORMATION,
        };

        let mut thread_info: THREAD_BASIC_INFORMATION = unsafe { mem::zeroed() };
        let mut length: DWORD = 0;

        let status = unsafe {
            NtQueryInformationThread(
                **self.handle,
                ThreadBasicInformation,
                &mut thread_info as *mut _ as ntdef::PVOID,
                mem::size_of::<THREAD_BASIC_INFORMATION>() as DWORD,
                &mut length as *mut _,
            )
        };

        if status != 0 {
            return Err(Error::ThreadTebError(status));
        }

        let base = Address::try_from(thread_info.TebBaseAddress)?;
        Ok(unsafe { process.read::<T>(base)? })
    }

    /// Get the context for a thread.
    pub fn get_context(&self) -> Result<ThreadContext<'_>, Error> {
        let mut context: winnt::CONTEXT = unsafe { mem::zeroed() };
        context.ContextFlags = winnt::CONTEXT_SEGMENTS;

        checked!(processthreadsapi::GetThreadContext(
            **self.handle,
            &mut context as winnt::PCONTEXT
        ))?;

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

pub struct ThreadContext<'a> {
    thread: &'a Thread,
    context: winnt::CONTEXT,
}

impl ThreadContext<'_> {
    /// Find where the thread stack is located.
    pub fn thread_stack(&self, process: &process::Process) -> anyhow::Result<AddressRange> {
        let entry = self
            .thread
            .thread_selector_entry(self.context.SegFs as DWORD)?;

        let address = Address::from(unsafe {
            let bytes = entry.HighWord.Bytes();
            entry.BaseLow as u32
                + ((bytes.BaseMid as u32) << 0x10)
                + ((bytes.BaseHi as u32) << 0x18)
        });

        let tib = unsafe { process.read::<winnt::NT_TIB32>(address)? };
        let stack_base = Address::from(tib.StackBase);
        let stack_limit = Address::from(tib.StackLimit);

        let size = match stack_base.size_from(stack_limit) {
            Some(size) => size,
            None => bail!(
                "stack base `{}` is smaller than stack limit `{}`",
                stack_base,
                stack_limit
            ),
        };

        Ok(AddressRange {
            base: Address::from(tib.StackLimit),
            size,
        })
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
            return Err(Error::System(io::Error::last_os_error()));
        }

        let handle = Arc::new(utils::Handle::new(handle));
        Ok(Thread { thread_id, handle })
    }
}
