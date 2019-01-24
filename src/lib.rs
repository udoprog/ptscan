#![feature(try_from, alloc, raw_vec_internals)]

extern crate alloc;

use winapi::shared::minwindef::DWORD;

#[macro_use]
mod utils;
mod address;
mod module;
pub mod opts;
mod process;
mod process_handle;
pub mod scan;
pub mod scanner;
mod system_info;
mod system_thread;
mod thread;
mod thread_buffers;

pub type ProcessId = DWORD;
pub type ThreadId = DWORD;

pub use self::address::{Address, AddressRange, Offset, Size};
pub use self::module::Module;
pub use self::process::{system_processes, MemoryState, MemoryType, Process};
pub use self::process_handle::{Location, ProcessHandle, ProcessName};
pub use self::scanner::{ScanResult, Scanner};
pub use self::system_thread::{system_threads, SystemThread};
pub use self::thread::Thread;
pub use self::utils::IteratorExtension;
