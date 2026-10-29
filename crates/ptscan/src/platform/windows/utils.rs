use std::{ffi::OsString, io, ops};

use crate::error::Error;

use super::system;

use windows_sys::{
    core::{BOOL, PWSTR},
    Win32::{
        Foundation::{self, GetLastError, FALSE, HANDLE, TRUE},
        System::Threading::IsWow64Process,
    },
};

/// Fixed string length.
pub fn fixed_string(cb: impl Fn(PWSTR, u32) -> u32) -> io::Result<OsString> {
    use std::os::windows::ffi::OsStringExt;

    let mut buf: [u16; 1024] = [0u16; 1024];

    let out = cb(buf.as_mut_ptr(), buf.len() as u32);

    if out == 0 {
        return Err(io::Error::last_os_error());
    }

    Ok(OsString::from_wide(&buf[..(out as usize)]))
}

/// Call a function that returns a string, indicating with error code if it is
/// longer than the default buffer and grow it appropriately.
pub fn growable_string(cb: impl Fn(PWSTR, u32) -> u32) -> io::Result<OsString> {
    use std::os::windows::ffi::OsStringExt;

    let mut buf = vec![0u16; 64];

    let len = loop {
        let len = cb(buf.as_mut_ptr(), buf.len() as u32);

        let last_error = unsafe { GetLastError() };

        match last_error {
            Foundation::ERROR_INSUFFICIENT_BUFFER => {
                buf.extend(std::iter::repeat(0u16).take(64));
                continue;
            }
            Foundation::ERROR_ACCESS_DENIED => {
                return Err(io::Error::last_os_error());
            }
            Foundation::ERROR_SUCCESS => break len as usize,
            _ => {
                return Err(io::Error::last_os_error());
            }
        }
    };

    Ok(OsString::from_wide(&buf[..len]))
}

/// Call a function that returns an array.
pub fn array<T>(
    initial: usize,
    cb: impl Fn(*mut T, u32, *mut u32) -> BOOL,
) -> Result<Vec<T>, Error> {
    use std::mem;

    let mut buf = vec![0u8; initial * mem::size_of::<T>()];

    loop {
        let mut needed: u32 = 0u32;

        let ok = cb(
            buf.as_mut_ptr() as *mut T,
            buf.capacity() as u32,
            &mut needed as *mut u32,
        );

        if ok != TRUE {
            return Err(Error::last_system_error());
        }

        unsafe {
            buf.set_len(usize::min(needed as usize, buf.capacity()));
        }

        if needed as usize > buf.capacity() {
            buf.reserve(initial);
            continue;
        }

        let ptr = buf.as_mut_ptr() as *mut T;
        let len = buf.len() / mem::size_of::<T>();
        let cap = buf.capacity() / mem::size_of::<T>();

        return Ok(unsafe {
            mem::forget(buf);
            Vec::from_raw_parts(ptr, len, cap)
        });
    }
}

/// Wrapper for handle that takes care of drop.
pub struct Handle(HANDLE);

impl Handle {
    pub fn new(handle: HANDLE) -> Self {
        Handle(handle)
    }

    /// Test if this is a 64-bit process.
    pub fn is_64bit(&self) -> Result<bool, Error> {
        if self.is_wow64()? {
            return Ok(false);
        }

        Ok(system::info()?.arch.is_64bit())
    }

    /// Test if the process is a 32-bit process running under WOW64 compatibility.
    fn is_wow64(&self) -> Result<bool, Error> {
        let mut out: BOOL = FALSE;
        checked!(IsWow64Process(self.0, &mut out as *mut BOOL))?;
        Ok(out == TRUE)
    }
}

impl ops::Deref for Handle {
    type Target = HANDLE;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Drop for Handle {
    fn drop(&mut self) {
        let ok = unsafe { Foundation::CloseHandle(self.0) };

        if ok != TRUE {
            panic!(
                "failed to close handle: {}",
                ::std::io::Error::last_os_error()
            );
        }
    }
}

unsafe impl Sync for Handle {}
unsafe impl Send for Handle {}
