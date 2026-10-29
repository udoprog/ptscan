//! Utilities to query the system for things.

use std::fmt;

use crate::{ProcessId, ThreadId};

pub use crate::platform::processes;
pub(crate) use crate::platform::threads;

/// A thread in the system with an associated parent process.
pub struct Thread {
    process_id: ProcessId,
    thread_id: ThreadId,
}

impl Thread {
    #[cfg_attr(not(windows), allow(dead_code))]
    pub(crate) fn new(process_id: ProcessId, thread_id: ThreadId) -> Self {
        Self {
            process_id,
            thread_id,
        }
    }

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
