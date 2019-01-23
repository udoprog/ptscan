#![feature(try_from)]

use winapi::shared::minwindef::DWORD;

#[macro_use]
mod utils;
mod address;
mod module;
pub mod opts;
mod process;
mod process_handle;
mod system_info;
mod system_thread;
mod thread;

pub type ProcessId = DWORD;
pub type ThreadId = DWORD;

pub use self::address::{Address, AddressRange, Offset, Size};
pub use self::module::Module;
pub use self::process::{system_processes, MemoryState, MemoryType, Process};
pub use self::process_handle::{Location, ProcessHandle, ProcessName};
pub use self::system_thread::{system_threads, SystemThread};
pub use self::thread::Thread;
pub use self::utils::IteratorExtension;
