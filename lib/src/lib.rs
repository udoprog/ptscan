use winapi::shared::minwindef::DWORD;

#[macro_use]
pub mod utils;
mod address;
mod error;
pub mod filter;
mod module;
pub mod opts;
mod pointer;
mod process;
mod process_handle;
pub mod scan;
pub mod system;
mod thread;
mod token;
mod ty;
mod value;

pub type ProcessId = DWORD;
pub type ThreadId = DWORD;

pub use self::address::{Address, AddressRange, Offset, Size};
pub use self::error::Error;
pub use self::filter::{Filter, Matcher, ValueExpr};
pub use self::module::Module;
pub use self::pointer::{Pointer, PointerBase};
pub use self::process::{MemoryInformation, MemoryState, MemoryType, Process};
pub use self::process_handle::{AddressProxy, Location, ProcessHandle, ProcessName};
pub use self::scan::{InitialScanConfig, Scan, ScanProgress, ScanResult};
pub use self::thread::Thread;
pub use self::token::Token;
pub use self::ty::Type;
pub use self::utils::IteratorExtension;
pub use self::value::Value;
