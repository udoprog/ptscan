//! A data structure to maintain a collection of persistent buffers across threads.

use hashbrown::HashMap;
use std::{
    cell::{Cell, UnsafeCell},
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
    lock: RwLock<HashMap<ThreadId, UnsafeCell<Buffer>>>,
}

unsafe impl Sync for ThreadBuffers {}

impl ThreadBuffers {
    pub fn new() -> ThreadBuffers {
        ThreadBuffers {
            lock: RwLock::new(HashMap::new()),
        }
    }

    /// Access a mutable persistent buffer for the current thread.
    ///
    /// # Safety
    ///
    /// The returned slice is _not_ guaranteed to be initialized and might contain stale data.
    pub fn get_mut<'a>(&'a self, len: usize) -> Result<Guard<'a>, Error> {
        let id = thread::current().id();

        loop {
            {
                let lock_guard = self.lock.read().map_err(|_| Error::LockPoisoned)?;

                if let Some(buffer) = lock_guard.get(&id) {
                    let buffer = buffer.get();

                    let cap = {
                        let buffer = unsafe { &*buffer };

                        if buffer.borrowed.get() {
                            return Err(Error::AlreadyBorrowed(id));
                        }

                        buffer.data.len()
                    };

                    if len <= cap {
                        unsafe { (*buffer).borrowed.set(true) };

                        return Ok(Guard {
                            lock_guard,
                            len,
                            buffer,
                        });
                    }
                }
            }

            let mut lock_guard = self.lock.write().map_err(|_| Error::LockPoisoned)?;
            // NB: guarantees unique access since we are under the lock.

            // we already have a buffer, but it's probably too small.
            if let Some(buffer) = lock_guard.get(&id) {
                let buffer = buffer.get();
                let buffer = unsafe { &mut *buffer };

                let cap = buffer.data.capacity();

                if len <= cap {
                    continue;
                }

                buffer.data.reserve(len - cap);

                // NB: set length to capacity so that slices can be properly fetched.
                unsafe {
                    buffer.data.set_len(buffer.data.capacity());
                }

                continue;
            }

            let mut vec = Vec::with_capacity(len);

            unsafe {
                vec.set_len(vec.capacity());
            }

            lock_guard.insert(id, UnsafeCell::new(Buffer::from_vec(vec)));
        }
    }
}

pub struct Guard<'a> {
    #[allow(unused)]
    lock_guard: RwLockReadGuard<'a, HashMap<ThreadId, UnsafeCell<Buffer>>>,
    len: usize,
    buffer: *mut Buffer,
}

impl<'a> Drop for Guard<'a> {
    fn drop(&mut self) {
        unsafe {
            (*self.buffer).borrowed.set(false);
        }
    }
}

impl<'a> ops::Deref for Guard<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        unsafe { &(*self.buffer).data[..self.len] }
    }
}

impl<'a> ops::DerefMut for Guard<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut (*self.buffer).data[..self.len] }
    }
}

/// Helper struct for a buffer.
///
/// Keeps track of if the buffer is mutably borrowed.
struct Buffer {
    data: Vec<u8>,
    borrowed: Cell<bool>,
}

impl Buffer {
    /// Create a buffer from a vector.
    fn from_vec(data: Vec<u8>) -> Buffer {
        Buffer {
            data: data,
            borrowed: Cell::new(false),
        }
    }
}
