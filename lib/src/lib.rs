use winapi::shared::minwindef::DWORD;

#[macro_use]
pub mod utils;
mod address;
mod error;
mod filter_expr;
mod module;
mod pointer;
mod process;
mod process_handle;
mod scan;
mod system;
mod thread;
mod token;
mod ty;
mod value;
mod value_expr;

pub type ProcessId = DWORD;
pub type ThreadId = DWORD;
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub use self::address::{Address, AddressRange, Offset, Size};
pub use self::error::Error;
pub use self::filter_expr::{FilterExpr, Special, Test};
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
pub use self::value_expr::ValueExpr;
