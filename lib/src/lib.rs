#![feature(try_from, alloc, raw_vec_internals)]

extern crate alloc;

use winapi::shared::minwindef::DWORD;

#[macro_use]
mod utils;
mod address;
mod encode;
pub mod filter;
mod module;
pub mod opts;
mod pointer;
mod process;
mod process_handle;
pub mod scan;
mod special;
pub mod system;
mod thread;
mod thread_buffers;
mod token;
mod r#type;
mod value;
mod values;
pub mod watch;

pub type ProcessId = DWORD;
pub type ThreadId = DWORD;

pub use self::address::{Address, AddressRange, Offset, Size};
pub use self::encode::Encode;
pub use self::module::Module;
pub use self::pointer::Pointer;
pub use self::process::{MemoryState, MemoryType, Process};
pub use self::process_handle::{Location, ProcessHandle, ProcessName};
pub use self::r#type::Type;
pub use self::scan::{Scan, ScanResult};
pub use self::thread::Thread;
pub use self::token::Token;
pub use self::utils::IteratorExtension;
pub use self::value::Value;
pub use self::values::Values;
