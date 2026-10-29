/// Evaluate the checked expression.
macro_rules! checked {
    ($expr:expr) => {
        match unsafe { $expr } {
            windows_sys::Win32::Foundation::TRUE => Ok(()),
            _ => Err($crate::error::Error::last_system_error()),
        }
    };
}

mod module;
mod process;
mod system;
mod thread;
mod utils;

pub use self::module::Module;
pub(crate) use self::process::{process_unavailable, RawProcess};
pub use self::system::processes;
pub(crate) use self::system::threads;
pub use self::thread::Thread;
