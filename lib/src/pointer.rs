mod lexer;
lalrpop_util::lalrpop_mod!(parser, "/pointer/parser.rs");

use crate::address::{Address, Offset};
use std::fmt;

/// The base of the pointer.
///
/// Can either be a module identified by a string that has to be looked up from a `ProcessHandle`, or a fixed address.
#[derive(Debug, Clone)]
pub enum Base {
    Module(String),
    Fixed(Address),
}

impl fmt::Display for Base {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Base::Fixed(ref address) => fmt::Display::fmt(address, fmt),
            Base::Module(ref name) => {
                // TODO: properly escape
                write!(fmt, "\"{}\"", name)?;
                Ok(())
            }
        }
    }
}

/// A pointer path.
///
/// Each offset is applied and dereferenced on the base in a chain.
///
/// The last step is dereferences as the pointee type.
#[derive(Debug, Clone)]
pub struct Pointer {
    base: Base,
    pub offsets: Vec<Offset>,
}

impl Pointer {
    pub fn new(base: Base) -> Self {
        Self {
            base,
            offsets: Vec::new(),
        }
    }
}

impl fmt::Display for Pointer {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.base, fmt)?;

        let mut it = self.offsets.iter();

        if let Some(o) = it.next() {
            if o.sign() {
                write!(fmt, " + {}", o)?;
            } else {
                write!(fmt, " - {}", o.abs())?;
            }
        }

        while let Some(o) = it.next() {
            write!(fmt, " -> {}", o)?;
        }

        Ok(())
    }
}

impl Pointer {
    /// Parse a a string into a filter.
    pub fn parse(input: &str) -> Result<Pointer, failure::Error> {
        Ok(self::parser::PointerParser::new().parse(lexer::Lexer::new(input))?)
    }
}

#[cfg(test)]
mod tests {
    use super::Pointer;
    use std::error;

    #[test]
    fn basic_parsing() -> Result<(), Box<error::Error>> {
        dbg!(Pointer::parse("ABCDEF + 0F")?);
        dbg!(Pointer::parse("\"Steam.exe\" + 0F")?);
        dbg!(Pointer::parse("\"Steam.exe\" + 0F -> -FFAA")?);
        dbg!(Pointer::parse("\"Steam.exe\" - F0F -> -FFAA")?);
        dbg!(Pointer::parse("\"Steam.exe\" - -F0F -> -FFAA")?);
        Ok(())
    }
}
