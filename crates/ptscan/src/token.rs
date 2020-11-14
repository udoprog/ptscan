use std::sync::atomic::{AtomicBool, Ordering};

/// A thread safe token that can be set to flag some condition.
#[repr(transparent)]
pub struct Token(AtomicBool);

impl Token {
    /// Create from a raw atomic boolean reference.
    ///
    /// This is safe, because Token shares the atomic boolean's internal
    /// representation.
    pub fn from_raw(value: &AtomicBool) -> &Self {
        unsafe { &*(value as *const _ as *const Token) }
    }

    /// Create a new token.
    pub const fn new() -> Token {
        Token(AtomicBool::new(false))
    }

    /// Set the token.
    pub fn set(&self) {
        self.0.store(true, Ordering::Release);
    }

    /// Test if the token is set and unset it if it is.
    pub fn test(&self) -> bool {
        self.0.load(Ordering::Acquire)
    }
}
