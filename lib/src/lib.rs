use winapi::shared::minwindef::DWORD;

#[macro_use]
pub mod utils;
mod address;
mod address_range;
mod alignment;
mod bitset;
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
mod scanner;
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
pub use self::alignment::{Aligned, Alignment, Unaligned};
pub use self::bitset::BitSet;
pub use self::encoding::Encoding;
pub use self::error::Error;
pub use self::filter_expr::{FilterExpr, Special, Test, TypedFilterExpr, ValueInfo};
pub use self::module::Module;
pub use self::offset::Offset;
pub use self::pointer::{
    Base, FollowablePointer, Pointer, PointerInfo, PortableBase, PortablePointer,
};
pub use self::pointer_scan::{
    PointerScan, PointerScanBackreferenceProgress, PointerScanInitialProgress,
};
pub use self::process::{
    MemoryInformation, MemoryState, MemoryType, PointerWidth, Process, ProcessInfo,
};
pub use self::process_handle::{
    AddressProxy, InitialScanConfig, Location, ModuleInfo, ModulesState, ProcessHandle,
    ProcessName, ProcessThread, ScanProgress, ValueHolder,
};
pub use self::scanner::{DefaultScanner, Scanner};
pub use self::sign::Sign;
pub use self::size::Size;
pub use self::system::processes;
pub use self::thread::Thread;
pub use self::token::Token;
pub use self::ty::Type;
pub use self::type_hint::{TypeHint, TypeSolveError};
pub use self::utils::IteratorExtension;
pub use self::value::{Value, ValueRef};
pub use self::value_expr::{TypedValueExpr, ValueExpr};
pub use self::values::Values;
