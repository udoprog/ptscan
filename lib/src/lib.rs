#![feature(try_from, alloc, raw_vec_internals)]

extern crate alloc;

use winapi::shared::minwindef::DWORD;

#[macro_use]
mod utils;
mod address;
pub mod filter;
mod module;
pub mod opts;
mod process;
mod process_handle;
pub mod scan;
pub mod scanner;
pub mod system;
mod thread;
mod thread_buffers;

pub type ProcessId = DWORD;
pub type ThreadId = DWORD;

pub use self::address::{Address, AddressRange, Offset, Pointer, Size};
pub use self::module::Module;
pub use self::process::{MemoryState, MemoryType, Process};
pub use self::process_handle::{Location, ProcessHandle, ProcessName};
pub use self::scanner::{ScanResult, Scanner};
pub use self::thread::Thread;
pub use self::utils::IteratorExtension;
