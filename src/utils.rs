use std::{ffi::OsString, io};

use failure::Error;

use crate::{process::MemoryInformation, AddressRange, Size};

use winapi::{
    shared::minwindef::{BOOL, DWORD, LPDWORD, TRUE},
    um::winnt,
};

/// Evaluate the checked expression.
macro_rules! checked {
    ($expr:expr) => {{
        if unsafe { $expr } != winapi::shared::minwindef::TRUE {
            return Err(failure::Error::from(std::io::Error::last_os_error()));
        }
    }};
}

/// Call a function that returns a string.
pub fn string(cb: impl Fn(winnt::LPWSTR, DWORD) -> DWORD) -> Result<OsString, Error> {
    use std::os::windows::ffi::OsStringExt;

    let mut buf: [winnt::WCHAR; 1024] = [0u16; 1024];

    let out = cb(buf.as_mut_ptr(), buf.len() as DWORD);

    if out == 0 {
        return Err(Error::from(io::Error::last_os_error()));
    }

    Ok(OsString::from_wide(&buf[..(out as usize)]))
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
            return Err(Error::from(io::Error::last_os_error()));
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

/// An implementation to drop a handle.
pub fn drop_handle(handle: winnt::HANDLE) {
    use winapi::um::handleapi;

    let ok = unsafe { handleapi::CloseHandle(handle) };

    if ok != TRUE {
        panic!("failed to close handle: {}", io::Error::last_os_error());
    }
}

/// Helper macro to handle fallible operations inside of a fallible iterator.
macro_rules! try_iter {
    ($expr:expr) => {
        match $expr {
            Ok(value) => value,
            Err(e) => return Some(Err(e)),
        }
    };
}

pub trait IteratorExtension {
    fn chunked(self, chunk_size: Size) -> Chunked<Self>
    where
        Self: Sized + Iterator<Item = Result<MemoryInformation, Error>>,
    {
        Chunked {
            iter: self,
            current: None,
            offset: Size::new(0),
            chunk_size,
        }
    }

    fn only_relevant(self, wants_reserved: bool) -> OnlyRelevant<Self>
    where
        Self: Sized + Iterator<Item = Result<MemoryInformation, Error>>,
    {
        OnlyRelevant {
            iter: self,
            wants_reserved,
        }
    }
}

impl<I> IteratorExtension for I where I: Iterator {}

pub struct OnlyRelevant<I>
where
    I: Iterator,
{
    iter: I,
    wants_reserved: bool,
}

impl<I> Iterator for OnlyRelevant<I>
where
    I: Iterator<Item = Result<MemoryInformation, Error>>,
{
    type Item = Result<MemoryInformation, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        use crate::process::{MemoryState, MemoryType};

        loop {
            let memory_info = match self.iter.next() {
                Some(memory_info) => try_iter!(memory_info),
                None => return None,
            };

            match (memory_info.state, memory_info.ty) {
                (_, MemoryType::Mapped) => continue,
                (MemoryState::Commit, _) => return Some(Ok(memory_info)),
                (MemoryState::Reserve, _) if self.wants_reserved => return Some(Ok(memory_info)),
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
    I: Iterator<Item = Result<MemoryInformation, Error>>,
{
    type Item = Result<(MemoryInformation, AddressRange), Error>;

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

            if self.offset >= memory_info.region_size {
                self.offset = Size::new(0);
                self.current.take();
                continue;
            }

            let s = self.offset;
            let e = Size::min(
                try_iter!(self.offset.add(self.chunk_size)),
                memory_info.region_size,
            );

            self.offset = e;

            let range = AddressRange {
                base: try_iter!(memory_info.base_address.add(s)),
                length: try_iter!(e.sub(s)),
            };

            return Some(Ok((*memory_info, range)));
        }
    }
}
