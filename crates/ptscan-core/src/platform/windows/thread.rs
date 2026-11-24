use std::{convert::TryFrom, fmt, io, mem, sync::Arc};

use super::utils;
use crate::{error::Error, process, Address, AddressRange, ThreadId};

use anyhow::bail;
use windows_sys::{
    core::BOOL,
    Win32::{
        Foundation::{FALSE, TRUE},
        System::{
            Diagnostics::Debug::{self, CONTEXT, LDT_ENTRY},
            SystemServices::{NT_TIB32, NT_TIB64},
            Threading::{self, THREAD_ACCESS_RIGHTS},
        },
    },
};

#[cfg(target_arch = "x86_64")]
const CONTEXT_SEGMENTS: Debug::CONTEXT_FLAGS = Debug::CONTEXT_SEGMENTS_AMD64;
#[cfg(target_arch = "x86")]
const CONTEXT_SEGMENTS: Debug::CONTEXT_FLAGS = Debug::CONTEXT_SEGMENTS_X86;

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
    pub fn thread_selector_entry(&self, selector: u32) -> Result<LDT_ENTRY, Error> {
        let mut entry: LDT_ENTRY = unsafe { mem::zeroed() };

        checked!(Debug::GetThreadSelectorEntry(
            **self.handle,
            selector,
            &mut entry as *mut LDT_ENTRY,
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
        let tib = self.thread_teb::<NT_TIB64>(process)?;

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
        use ntapi::ntpsapi::THREAD_BASIC_INFORMATION;
        use windows_sys::Wdk::System::Threading::{
            NtQueryInformationThread, ThreadBasicInformation,
        };

        let mut thread_info: THREAD_BASIC_INFORMATION = unsafe { mem::zeroed() };
        let mut length: u32 = 0;

        let status = unsafe {
            NtQueryInformationThread(
                **self.handle,
                ThreadBasicInformation,
                &mut thread_info as *mut _ as *mut core::ffi::c_void,
                mem::size_of::<THREAD_BASIC_INFORMATION>() as u32,
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
        let mut context: CONTEXT = unsafe { mem::zeroed() };
        context.ContextFlags = CONTEXT_SEGMENTS;

        checked!(Debug::GetThreadContext(
            **self.handle,
            &mut context as *mut CONTEXT
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
    context: CONTEXT,
}

impl ThreadContext<'_> {
    /// Find where the thread stack is located.
    pub fn thread_stack(&self, process: &process::Process) -> anyhow::Result<AddressRange> {
        let entry = self
            .thread
            .thread_selector_entry(self.context.SegFs as u32)?;

        let address = Address::from(unsafe {
            let bytes = entry.HighWord.Bytes;
            entry.BaseLow as u32
                + ((bytes.BaseMid as u32) << 0x10)
                + ((bytes.BaseHi as u32) << 0x18)
        });

        let tib = unsafe { process.read::<NT_TIB32>(address)? };
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
    desired_access: THREAD_ACCESS_RIGHTS,
    inherit_handles: BOOL,
}

impl OpenThreadBuilder {
    /// Access everything.
    pub fn all_access(self) -> Self {
        OpenThreadBuilder {
            desired_access: self.desired_access | Threading::THREAD_ALL_ACCESS,
            ..self
        }
    }

    /// Set the THREAD_GET_CONTEXT desired access flag.
    pub fn get_context(self) -> Self {
        OpenThreadBuilder {
            desired_access: self.desired_access | Threading::THREAD_GET_CONTEXT,
            ..self
        }
    }

    /// Set the query information flag.
    pub fn query_information(self) -> Self {
        OpenThreadBuilder {
            desired_access: self.desired_access | Threading::THREAD_QUERY_INFORMATION,
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
        let handle =
            unsafe { Threading::OpenThread(self.desired_access, self.inherit_handles, thread_id) };

        if handle.is_null() {
            return Err(Error::System(io::Error::last_os_error()));
        }

        let handle = Arc::new(utils::Handle::new(handle));
        Ok(Thread { thread_id, handle })
    }
}
