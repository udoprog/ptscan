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
mod process;
mod process_handle;
mod progress_reporter;
mod scan;
mod scan_result;
mod size;
mod system;
mod thread;
mod token;
mod ty;
mod value;
mod value_expr;

pub type ProcessId = DWORD;
pub type ThreadId = DWORD;
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// The endianness of a number.
#[derive(Debug, Clone, Copy)]
pub enum Endianness {
    LittleEndian,
    BigEndian,
}

/// The sign of a number or offset.
#[derive(
    Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize,
)]
pub enum Sign {
    #[serde(rename = "pos")]
    Pos,
    #[serde(rename = "neg")]
    Neg,
}

pub use self::address::Address;
pub use self::address_range::AddressRange;
pub use self::encoding::Encoding;
pub use self::error::Error;
pub use self::filter_expr::{FilterExpr, Special, Test};
pub use self::module::Module;
pub use self::offset::Offset;
pub use self::pointer::{Pointer, PointerBase, RawPointer};
pub use self::process::{MemoryInformation, MemoryState, MemoryType, Process};
pub use self::process_handle::{AddressProxy, Location, ProcessHandle, ProcessName};
pub use self::scan::{InitialScanConfig, Scan, ScanProgress};
pub use self::scan_result::ScanResult;
pub use self::size::Size;
pub use self::thread::Thread;
pub use self::token::Token;
pub use self::ty::Type;
pub use self::utils::IteratorExtension;
pub use self::value::Value;
pub use self::value_expr::ValueExpr;
