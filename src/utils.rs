use std::{ffi::OsString, fmt, io};

use failure::Error;

use crate::{process::MemoryInformation, VirtualAddress};

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

/// A simple virtual address wrapper to hex encode the address when formatting for debug.
pub struct Hex(pub VirtualAddress);

impl fmt::Debug for Hex {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "0x{:X}", self.0)
    }
}

pub trait IteratorExtension {
    fn chunked(self, chunk_size: u64) -> Chunked<Self>
    where
        Self: Sized + Iterator<Item = Result<MemoryInformation, Error>>,
    {
        Chunked {
            iter: self,
            current: None,
            offset: 0,
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
                Some(Ok(memory_info)) => memory_info,
                Some(Err(e)) => return Some(Err(e)),
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
    offset: u64,
    chunk_size: u64,
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
                    Some(Ok(memory_info)) => Some(memory_info),
                    Some(Err(e)) => return Some(Err(e)),
                    None => return None,
                }
            }

            let memory_info = match self.current.as_ref() {
                Some(memory_info) => memory_info,
                None => return None,
            };

            if self.offset >= memory_info.region_size {
                self.offset = 0;
                self.current.take();
                continue;
            }

            let s = self.offset;
            let e = u64::min(self.offset + self.chunk_size, memory_info.region_size);

            self.offset = e;

            let range = AddressRange {
                base: memory_info.base_address + s,
                length: e - s,
            };

            return Some(Ok((*memory_info, range)));
        }
    }
}

#[derive(Clone, Copy)]
pub struct AddressRange {
    pub base: VirtualAddress,
    pub length: VirtualAddress,
}

impl fmt::Debug for AddressRange {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("AddressRange")
            .field("base", &Hex(self.base))
            .field("length", &Hex(self.length))
            .finish()
    }
}

impl AddressRange {
    pub fn contains(&self, value: VirtualAddress) -> bool {
        self.base <= value && value <= (self.base + self.length)
    }
}
