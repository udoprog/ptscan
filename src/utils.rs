use std::{ffi::OsString, io, ops, sync, thread};

use crate::{
    process::{MemoryInformation, MemoryProtect},
    AddressRange, Size,
};

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
pub fn string(cb: impl Fn(winnt::LPWSTR, DWORD) -> DWORD) -> Result<OsString, failure::Error> {
    use std::os::windows::ffi::OsStringExt;

    let mut buf: [winnt::WCHAR; 1024] = [0u16; 1024];

    let out = cb(buf.as_mut_ptr(), buf.len() as DWORD);

    if out == 0 {
        return Err(failure::Error::from(io::Error::last_os_error()));
    }

    Ok(OsString::from_wide(&buf[..(out as usize)]))
}

/// Call a function that returns an array.
pub fn array<T>(
    initial: usize,
    cb: impl Fn(*mut T, DWORD, LPDWORD) -> BOOL,
) -> Result<Vec<T>, failure::Error> {
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
            return Err(failure::Error::from(io::Error::last_os_error()));
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
            panic!("failed to close handle: {}", io::Error::last_os_error());
        }
    }
}

unsafe impl Sync for Handle {}
unsafe impl Send for Handle {}

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
        Self: Sized + Iterator<Item = Result<MemoryInformation, failure::Error>>,
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
        Self: Sized + Iterator<Item = Result<MemoryInformation, failure::Error>>,
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
    I: Iterator<Item = Result<MemoryInformation, failure::Error>>,
{
    type Item = Result<MemoryInformation, failure::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        use crate::process::{MemoryState, MemoryType};

        loop {
            let memory_info = match self.iter.next() {
                Some(memory_info) => try_iter!(memory_info),
                None => return None,
            };

            match (memory_info.state, memory_info.ty) {
                (_, MemoryType::Mapped) => continue,
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
    I: Iterator<Item = Result<MemoryInformation, failure::Error>>,
{
    type Item = Result<(MemoryInformation, AddressRange), failure::Error>;

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

            if self.offset >= memory_info.range.length {
                self.offset = Size::new(0);
                self.current.take();
                continue;
            }

            let s = self.offset;
            let e = Size::min(
                try_iter!(self.offset.add(self.chunk_size)),
                memory_info.range.length,
            );

            self.offset = e;

            let range = AddressRange {
                base: try_iter!(memory_info.range.base.add(s)),
                length: try_iter!(e.sub(s)),
            };

            return Some(Ok((memory_info.clone(), range)));
        }
    }
}

/// A collection of buffers mapped to threads.
pub struct ThreadBuffers {
    buffer_size: usize,
    map: sync::RwLock<hashbrown::HashMap<thread::ThreadId, Buffer>>,
}

impl ThreadBuffers {
    pub fn new(buffer_size: usize) -> ThreadBuffers {
        ThreadBuffers {
            buffer_size,
            map: sync::RwLock::new(hashbrown::HashMap::new()),
        }
    }

    /// Get the buffer for the current thread.
    pub unsafe fn get_mut<'a>(&'a self, len: usize) -> Result<Guard<'a>, failure::Error> {
        let id = thread::current().id();

        loop {
            {
                let inner = self
                    .map
                    .read()
                    .map_err(|_| failure::format_err!("lock poisoned"))?;

                if let Some(b) = inner.get(&id) {
                    let ptr = b.ptr.clone();
                    let cap = b.cap.clone();

                    if len > cap {
                        failure::bail!("request length is greater than capacity");
                    }

                    return Ok(Guard {
                        _inner: inner,
                        ptr,
                        len,
                    });
                }
            }

            let mut inner = self
                .map
                .write()
                .map_err(|_| failure::format_err!("lock poisoned"))?;
            let mut buffer = Vec::with_capacity(self.buffer_size);

            let ptr = buffer.as_mut_ptr();
            let len = buffer.capacity();
            let cap = buffer.capacity();

            std::mem::forget(buffer);

            inner.insert(id, Buffer { ptr, len, cap });
        }
    }
}

pub struct Guard<'a> {
    _inner: sync::RwLockReadGuard<'a, hashbrown::HashMap<thread::ThreadId, Buffer>>,
    ptr: *mut u8,
    len: usize,
}

impl<'a> Guard<'a> {
    /// Treat guard as mutable buffer.
    pub fn as_ref<'b>(&'b self) -> &'b [u8] {
        unsafe { std::slice::from_raw_parts(self.ptr, self.len) }
    }

    /// Treat guard as mutable buffer.
    pub fn as_mut<'b>(&'b mut self) -> &'b mut [u8] {
        unsafe { std::slice::from_raw_parts_mut(self.ptr, self.len) }
    }
}

impl<'a> ops::Deref for Guard<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<'a> ops::DerefMut for Guard<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}

struct Buffer {
    ptr: *mut u8,
    len: usize,
    cap: usize,
}

unsafe impl Sync for Buffer {}

/// TODO: why does this have to be Send?
unsafe impl Send for Buffer {}

impl Drop for Buffer {
    fn drop(&mut self) {
        unsafe {
            Vec::from_raw_parts(self.ptr, self.len, self.cap);
        }
    }
}
