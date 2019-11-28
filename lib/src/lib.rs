use winapi::shared::minwindef::DWORD;

#[macro_use]
pub mod utils;
mod address;
mod address_range;
mod encoding;
mod error;
mod filter_expr;
mod module;
mod offset;
mod pointer;
mod pointer_scan;
mod process;
mod process_handle;
mod progress_reporter;
mod scan;
mod scan_result;
mod sign;
mod size;
mod system;
mod thread;
mod token;
mod ty;
mod type_hint;
mod value;
mod value_expr;
mod values;

pub type ProcessId = DWORD;
pub type ThreadId = DWORD;
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// The endianness of a number.
#[derive(Debug, Clone, Copy)]
pub enum Endianness {
    LittleEndian,
    BigEndian,
}

#[derive(Debug, Clone, Copy)]
pub enum Cached<T> {
    Some(T),
    None,
}

pub use self::address::Address;
pub use self::address_range::AddressRange;
pub use self::encoding::Encoding;
pub use self::error::Error;
pub use self::filter_expr::{FilterExpr, Special, Test, ValueInfo};
pub use self::module::Module;
pub use self::offset::Offset;
pub use self::pointer::{Pointer, PointerBase, RawPointer};
pub use self::pointer_scan::{
    PointerScan, PointerScanBackreferenceProgress, PointerScanInitialProgress,
};
pub use self::process::{MemoryInformation, MemoryState, MemoryType, Process, ProcessInfo};
pub use self::process_handle::{
    AddressProxy, Location, ModuleInfo, ModulesState, ProcessHandle, ProcessName, ProcessThread,
};
pub use self::scan::{InitialScanConfig, Scan, ScanProgress};
pub use self::scan_result::ScanResult;
pub use self::sign::Sign;
pub use self::size::Size;
pub use self::system::processes;
pub use self::thread::Thread;
pub use self::token::Token;
pub use self::ty::Type;
pub use self::type_hint::TypeHint;
pub use self::utils::IteratorExtension;
pub use self::value::Value;
pub use self::value_expr::ValueExpr;
pub use self::values::Values;
