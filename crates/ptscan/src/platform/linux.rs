//! Linux support, which does not yet implement any process access.

use std::{ffi::OsString, io};

use crate::{
    error::Error,
    module::ModuleInfo,
    process::{MemoryInformation, OpenProcessBuilder},
    system, Address, AddressRange, Process, ProcessId, ThreadId,
};

fn unsupported_io() -> io::Error {
    io::Error::from(io::ErrorKind::Unsupported)
}

pub(crate) struct RawProcess {
    _private: (),
}

impl RawProcess {
    pub(crate) fn open(_: &OpenProcessBuilder, _: ProcessId) -> Result<Self, Error> {
        Err(Error::Unsupported)
    }

    pub(crate) fn is_64bit(&self) -> Result<bool, Error> {
        Err(Error::Unsupported)
    }

    pub(crate) fn module_base_name(&self) -> io::Result<OsString> {
        Err(unsupported_io())
    }

    pub(crate) fn modules(&self) -> Result<Vec<Module>, Error> {
        Err(Error::Unsupported)
    }

    pub(crate) fn write_memory(&self, _: Address, _: &[u8]) -> Result<bool, Error> {
        Err(Error::Unsupported)
    }

    pub(crate) fn read_memory(&self, _: Address, _: &mut [u8]) -> Result<usize, Error> {
        Err(Error::Unsupported)
    }

    pub(crate) fn virtual_query(&self, _: Address) -> anyhow::Result<Option<MemoryInformation>> {
        Err(Error::Unsupported.into())
    }

    pub(crate) fn suspend(&self) -> Result<(), Error> {
        Err(Error::Unsupported)
    }

    pub(crate) fn resume(&self) -> Result<(), Error> {
        Err(Error::Unsupported)
    }
}

pub(crate) fn process_unavailable(_: &Error) -> bool {
    false
}

pub fn processes() -> Result<Vec<ProcessId>, Error> {
    Err(Error::Unsupported)
}

pub(crate) fn threads() -> Result<std::iter::Empty<system::Thread>, Error> {
    Err(Error::Unsupported)
}

pub struct Module {
    _private: (),
}

impl Module {
    pub fn name(&self, _: &Process) -> io::Result<OsString> {
        Err(unsupported_io())
    }

    pub fn file_name(&self) -> io::Result<OsString> {
        Err(unsupported_io())
    }

    pub fn info(&self, _: &Process) -> io::Result<ModuleInfo> {
        Err(unsupported_io())
    }
}

#[derive(Clone)]
pub struct Thread {
    thread_id: ThreadId,
}

impl Thread {
    pub fn builder() -> OpenThreadBuilder {
        OpenThreadBuilder::default()
    }

    pub fn open(thread_id: ThreadId) -> Result<Thread, Error> {
        Self::builder().build(thread_id)
    }

    pub fn thread_id(&self) -> ThreadId {
        self.thread_id
    }

    pub fn thread_stack(&self, _: &Process) -> anyhow::Result<AddressRange> {
        Err(Error::Unsupported.into())
    }
}

#[derive(Default)]
pub struct OpenThreadBuilder {
    _private: (),
}

impl OpenThreadBuilder {
    pub fn all_access(self) -> Self {
        self
    }

    pub fn get_context(self) -> Self {
        self
    }

    pub fn query_information(self) -> Self {
        self
    }

    pub fn inherit_handles(self) -> Self {
        self
    }

    pub fn build(self, _: ThreadId) -> Result<Thread, Error> {
        Err(Error::Unsupported)
    }
}
