use crate::{filter_expr::Binary, ProcessId, ThreadId, ValueExpr};
use std::io;
use thiserror::Error;

use winapi::shared::{minwindef::DWORD, ntdef::NTSTATUS};

#[derive(Debug, Error)]
pub enum Error {
    #[error("system error: {0}")]
    System(#[source] io::Error),
    #[error("bad region state: {0}")]
    BadRegionState(DWORD),
    #[error("bad region type: {0}")]
    BadRegionType(DWORD),
    #[error("read underflow")]
    ReadUnderflow,
    #[error("buffer too small, expected {0} bytes but got {1}")]
    BufferOverflow(usize, usize),
    #[error("failed to open process: {0}")]
    OpenProcess(ProcessId),
    #[error("failed to build thread interface: {0}")]
    BuildThread(ThreadId),
    #[error("failed to extract thread stack for thread: {0}")]
    ThreadStack(ThreadId),
    #[error("failed to scan for thread exit")]
    ScanForExit,
    #[error("failed to resolved address through thread teb with status: {0}")]
    ThreadTebError(NTSTATUS),
    #[error("failed to convert number to address")]
    AddressConversion,
    #[error("failed to convert string to address")]
    AddressFromStr,
    #[error("value missing numeric base, like 42u64")]
    ValueMissingBase,
    #[error("failed to decode utf-8 string")]
    NonUtf8,
    #[error("unsupported pointer width: {0}")]
    UnsupportedPointerWidth(usize),
    #[error("failed to convert address into native pointer")]
    PointerConversionError,
    #[error("cannot determine type of: {0}")]
    TypeInference(ValueExpr),
    #[error("cannot determine type of: {0}")]
    BinaryTypeInference(Binary),
    #[error("this error literally cannot happen")]
    Infallible,
}

impl Error {
    /// Get last system error.
    pub fn last_system_error() -> Self {
        Self::System(io::Error::last_os_error())
    }

    /// Access the underlying raw OS error.
    pub fn raw_os_error(&self) -> Option<i32> {
        match self {
            Self::System(e) => e.raw_os_error(),
            _ => None,
        }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Self::System(error)
    }
}

impl From<std::convert::Infallible> for Error {
    fn from(_: std::convert::Infallible) -> Self {
        Self::Infallible
    }
}
