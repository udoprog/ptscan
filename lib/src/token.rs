use std::sync::atomic::{AtomicBool, Ordering};

/// A static token that is never set.
static TOKEN_SET: Token = Token::new();

/// A thread safe token that can be set to flag some condition.
pub struct Token(AtomicBool);

impl Token {
    /// Create a new token.
    pub const fn new() -> Token {
        Token(AtomicBool::new(false))
    }

    /// Access a static token that is never set.
    pub fn default() -> &'static Token {
        &TOKEN_SET
    }

    /// Set the token.
    pub fn set(&self) {
        self.0.store(true, Ordering::SeqCst);
    }

    /// Test if the token is set and unset it if it is.
    pub fn test(&self) -> bool {
        self.0.compare_and_swap(true, false, Ordering::SeqCst)
    }
}
