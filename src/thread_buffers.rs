//! A data structure to maintain a collection of persistent buffers across threads.

use hashbrown::HashMap;
use std::{
    cell::Cell,
    ops,
    sync::{RwLock, RwLockReadGuard},
    thread::{self, ThreadId},
};

#[derive(Debug, failure::Fail)]
pub enum Error {
    #[fail(display = "lock poisoned")]
    LockPoisoned,
    #[fail(display = "buffer for thread {:?} is already borrowed", _0)]
    AlreadyBorrowed(ThreadId),
}

/// A collection of buffers mapped to threads.
pub struct ThreadBuffers {
    map: RwLock<HashMap<ThreadId, Buffer>>,
}

impl ThreadBuffers {
    pub fn new() -> ThreadBuffers {
        ThreadBuffers {
            map: RwLock::new(hashbrown::HashMap::new()),
        }
    }

    /// Access a mutable persistent buffer for the current thread.
    pub fn get_mut<'a>(&'a self, len: usize) -> Result<Guard<'a>, Error> {
        let id = thread::current().id();

        loop {
            {
                let inner = self.map.read().map_err(|_| Error::LockPoisoned)?;

                if let Some(b) = inner.get(&id) {
                    if b.borrowed.get() {
                        return Err(Error::AlreadyBorrowed(id));
                    }

                    b.borrowed.set(true);
                    let cap = b.cap;

                    // NB: this _should_ be OK since we're guaranteeing unique access above.
                    // Can we use UnsafeCell?
                    let buffer = b as *const Buffer as *mut Buffer;

                    if len <= cap {
                        return Ok(Guard {
                            _inner: inner,
                            buffer,
                        });
                    }
                }
            }

            let mut inner = self.map.write().map_err(|_| Error::LockPoisoned)?;

            // we already have a buffer, but it's probably too small.
            if let Some(existing) = inner.get_mut(&id) {
                if len <= existing.cap {
                    continue;
                }

                existing.reserve(len - existing.cap);
                continue;
            }

            inner.insert(id, Buffer::from_vec(Vec::with_capacity(len)));
        }
    }
}

pub struct Guard<'a> {
    _inner: RwLockReadGuard<'a, hashbrown::HashMap<ThreadId, Buffer>>,
    buffer: *mut Buffer,
}

impl<'a> Drop for Guard<'a> {
    fn drop(&mut self) {
        unsafe {
            (*self.buffer).borrowed.set(false);
        }
    }
}

impl<'a> Guard<'a> {
    /// Treat guard as mutable buffer.
    pub fn as_ref<'b>(&'b self) -> &'b [u8] {
        unsafe {
            let Buffer { ptr, len, .. } = *self.buffer;

            std::slice::from_raw_parts(ptr, len)
        }
    }

    /// Treat guard as mutable buffer.
    pub fn as_mut<'b>(&'b mut self) -> &'b mut [u8] {
        unsafe {
            let Buffer { ptr, len, .. } = *self.buffer;

            std::slice::from_raw_parts_mut(ptr, len)
        }
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
    borrowed: Cell<bool>,
}

unsafe impl Sync for Buffer {}
unsafe impl Send for Buffer {}

impl Buffer {
    /// Create a buffer from a vector.
    pub fn from_vec(mut data: Vec<u8>) -> Buffer {
        use std::mem;

        let buffer = Buffer {
            ptr: data.as_mut_ptr(),
            len: data.capacity(),
            cap: data.capacity(),
            borrowed: Cell::new(false),
        };

        mem::forget(data);
        buffer
    }

    pub fn reserve(&mut self, additional: usize) {
        use std::mem;

        let mut v = unsafe { Vec::from_raw_parts(self.ptr, self.len, self.cap) };

        v.reserve(additional);
        let old = mem::replace(self, Buffer::from_vec(v));

        // NB: need to forget the old one to avoid de-allocating it.
        mem::forget(old);
    }
}

impl Drop for Buffer {
    fn drop(&mut self) {
        unsafe {
            Vec::from_raw_parts(self.ptr, self.len, self.cap);
        }
    }
}
