mod words;

use std::{ffi::OsString, fmt, io, ops};

use crate::{
    error::Error,
    process::{MemoryInformation, MemoryProtect},
    system, AddressRange, Size,
};

pub use self::words::Words;

use winapi::{
    shared::{
        minwindef::{BOOL, DWORD, FALSE, LPDWORD, PBOOL, TRUE},
        winerror,
    },
    um::{errhandlingapi, winnt, wow64apiset},
};

/// Evaluate the checked expression.
macro_rules! checked {
    ($expr:expr) => {
        match unsafe { $expr } {
            winapi::shared::minwindef::TRUE => Ok(()),
            _ => Err($crate::error::Error::last_system_error()),
        }
    };
}

/// Helper macro to handle fallible operations inside of a fallible iterator.
macro_rules! try_iter {
    ($expr:expr) => {
        match $expr {
            Ok(value) => value,
            Err(e) => return Some(Err(Into::into(e))),
        }
    };
}

/// Fixed string length.
pub fn fixed_string(cb: impl Fn(winnt::LPWSTR, DWORD) -> DWORD) -> io::Result<OsString> {
    use std::os::windows::ffi::OsStringExt;

    let mut buf: [winnt::WCHAR; 1024] = [0u16; 1024];

    let out = cb(buf.as_mut_ptr(), buf.len() as DWORD);

    if out == 0 {
        return Err(io::Error::last_os_error());
    }

    Ok(OsString::from_wide(&buf[..(out as usize)]))
}

/// Call a function that returns a string, indicating with error code if it is
/// longer than the default buffer and grow it appropriately.
pub fn growable_string(cb: impl Fn(winnt::LPWSTR, DWORD) -> DWORD) -> io::Result<OsString> {
    use std::os::windows::ffi::OsStringExt;

    let mut buf = vec![0u16; 64];

    let len = loop {
        let len = cb(buf.as_mut_ptr(), buf.len() as DWORD);

        let last_error = unsafe { errhandlingapi::GetLastError() };

        match last_error {
            winerror::ERROR_INSUFFICIENT_BUFFER => {
                buf.extend(std::iter::repeat(0u16).take(64));
                continue;
            }
            winerror::ERROR_ACCESS_DENIED => {
                return Err(io::Error::last_os_error());
            }
            winerror::ERROR_SUCCESS => break len as usize,
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
    cb: impl Fn(*mut T, DWORD, LPDWORD) -> BOOL,
) -> Result<Vec<T>, Error> {
    use std::mem;

    let mut buf = vec![0u8; initial * mem::size_of::<T>()];

    loop {
        let mut needed: DWORD = 0u32;

        let ok = cb(
            buf.as_mut_ptr() as *mut T,
            buf.capacity() as DWORD,
            &mut needed as LPDWORD,
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
pub struct Handle(winnt::HANDLE);

impl Handle {
    pub fn new(handle: winnt::HANDLE) -> Self {
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
        checked!(wow64apiset::IsWow64Process(self.0, &mut out as PBOOL))?;
        Ok(out == TRUE)
    }
}

impl ops::Deref for Handle {
    type Target = winnt::HANDLE;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Drop for Handle {
    fn drop(&mut self) {
        use winapi::um::handleapi;

        let ok = unsafe { handleapi::CloseHandle(self.0) };

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

pub trait IteratorExtension {
    fn chunked(self, chunk_size: Size) -> Chunked<Self>
    where
        Self: Sized + Iterator<Item = anyhow::Result<MemoryInformation>>,
    {
        Chunked {
            iter: self,
            current: None,
            offset: Size::new(0),
            chunk_size,
        }
    }

    fn only_relevant(self) -> OnlyRelevant<Self>
    where
        Self: Sized + Iterator<Item = anyhow::Result<MemoryInformation>>,
    {
        OnlyRelevant { iter: self }
    }
}

impl<I> IteratorExtension for I where I: Iterator {}

/// A filter which filters out only relevant pages.
pub struct OnlyRelevant<I>
where
    I: Iterator,
{
    iter: I,
}

impl<I> Iterator for OnlyRelevant<I>
where
    I: Iterator<Item = anyhow::Result<MemoryInformation>>,
{
    type Item = anyhow::Result<MemoryInformation>;

    fn next(&mut self) -> Option<Self::Item> {
        use crate::process::{MemoryState, MemoryType};

        loop {
            let memory_info = match self.iter.next() {
                Some(memory_info) => try_iter!(memory_info),
                None => return None,
            };

            match (memory_info.state, memory_info.ty) {
                (_, Some(MemoryType::Mapped)) => continue,
                (MemoryState::Commit, _) => {
                    if memory_info.protect.contains(MemoryProtect::NoAccess) {
                        continue;
                    }

                    if memory_info.protect.contains(MemoryProtect::Guard) {
                        continue;
                    }

                    if memory_info.protect.contains(MemoryProtect::NoCache) {
                        continue;
                    }

                    if !memory_info.is_writable() {
                        continue;
                    }

                    return Some(Ok(memory_info));
                }
                _ => continue,
            }
        }
    }
}

pub struct Chunked<I>
where
    I: Iterator,
{
    iter: I,
    current: Option<MemoryInformation>,
    offset: Size,
    chunk_size: Size,
}

impl<I> Iterator for Chunked<I>
where
    I: Iterator<Item = anyhow::Result<MemoryInformation>>,
{
    type Item = anyhow::Result<(MemoryInformation, AddressRange)>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.current.is_none() {
                self.current = match self.iter.next() {
                    Some(memory_info) => Some(try_iter!(memory_info)),
                    None => return None,
                }
            }

            let memory_info = match self.current.as_ref() {
                Some(memory_info) => memory_info,
                None => return None,
            };

            if self.offset >= memory_info.range.size {
                self.offset = Size::new(0);
                self.current.take();
                continue;
            }

            let s = self.offset;
            let e = Size::min(
                self.offset.checked_add(self.chunk_size)?,
                memory_info.range.size,
            );

            self.offset = e;

            let range = AddressRange {
                base: memory_info.range.base.checked_add_size(s)?,
                size: e.checked_sub(s)?,
            };

            return Some(Ok((memory_info.clone(), range)));
        }
    }
}

pub struct Hex<'a>(pub &'a [u8]);

impl fmt::Display for Hex<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut it = self.0.iter().cloned();

        let last = it.next_back();

        for c in it {
            write!(fmt, "{:02X} ", c)?;
        }

        if let Some(c) = last {
            write!(fmt, "{:02X}", c)?;
        }

        Ok(())
    }
}

pub struct EscapeString<'a>(pub &'a str);

impl fmt::Display for EscapeString<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "\"")?;

        for c in self.0.chars() {
            match c {
                '\\' => write!(fmt, "\\\\")?,
                '"' => write!(fmt, "\\\"")?,
                ' ' => write!(fmt, " ")?,
                '\t' => write!(fmt, "\\t")?,
                '\n' => write!(fmt, "\\n")?,
                '\r' => write!(fmt, "\\r")?,
                c => write!(fmt, "{}", c)?,
            }
        }

        write!(fmt, "\"")?;
        Ok(())
    }
}
