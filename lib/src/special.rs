//! A special, high-performance filter.

use crate::Encode;
use std::mem;

/// A bunch of read-only zeros used for comparison.
static ZEROS: &'static [u8] = &[
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
];

/// Special matching mode to speed up scanning.
///
/// Special matchers are matchers which can look at an entire slice of memory and immediate (without decoding) decide
/// if it is a match or not.
///
/// This has the benefit of speeding up decoding of scanned memory significantly.
#[derive(Debug, Clone)]
pub enum Special {
    /// Must match the given buffer exactly.
    Exact { buffer: Vec<u8> },
    /// Must not match the given buffer exactly.
    NotExact { buffer: Vec<u8> },
    /// All bytes in the given range are expected to be zero.
    Zero,
    /// All bytes in the given range are expected to be non-zero.
    NotZero,
}

impl Special {
    /// Invert what the special matcher does.
    ///
    /// Non-zero matchers become zero matchers,
    /// Exact matchers become not-exact matchers.
    pub fn invert(self) -> Special {
        match self {
            Special::Zero => Special::NotZero,
            Special::NotZero => Special::Zero,
            Special::Exact { buffer } => Special::NotExact { buffer },
            Special::NotExact { buffer } => Special::Exact { buffer },
        }
    }

    /// Set up an exact match for the given value.
    pub fn exact_string(s: &str) -> Special {
        Special::Exact {
            buffer: s.as_bytes().to_vec(),
        }
    }

    /// Set up an exact match for the given value.
    pub fn exact<T>(value: T) -> Special
    where
        T: Encode,
    {
        let mut buffer = vec![0u8; mem::size_of::<T>()];
        value.encode(&mut buffer);
        Special::Exact { buffer }
    }

    /// Set up a not-exact match for the given value.
    pub fn not_exact<T>(value: T) -> Special
    where
        T: Encode,
    {
        let mut buffer = vec![0u8; mem::size_of::<T>()];
        value.encode(&mut buffer);
        Special::NotExact { buffer }
    }

    /// Test if this special scan matches the given slice of memory.
    pub fn test(&self, buf: &[u8]) -> Option<bool> {
        match self {
            Special::Zero => Some(buf == &ZEROS[..buf.len()]),
            Special::NotZero => {
                if buf == &ZEROS[..buf.len()] {
                    return Some(false);
                }

                None
            }
            Special::Exact { ref buffer } => Some(buf == buffer.as_slice()),
            Special::NotExact { ref buffer } => Some(buf != buffer.as_slice()),
        }
    }
}
