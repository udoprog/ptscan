mod lexer;
lalrpop_util::lalrpop_mod!(parser, "/pointer/parser.rs");

use crate::address::{Address, Offset, Sign};
use std::fmt;

/// The base of the pointer.
///
/// Can either be a module identified by a string that has to be looked up from a `ProcessHandle`, or a fixed address.
#[derive(Debug, Clone)]
pub enum Base {
    /// An offset from a module.
    Module(String, Offset),
    /// A fixed address.
    Fixed(Address),
}

impl fmt::Display for Base {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Base::Fixed(ref address) => fmt::Display::fmt(address, fmt),
            Base::Module(ref name, ref offset) => {
                // TODO: properly escape
                write!(fmt, "\"{}\"", name)?;

                if offset.abs() > 0 {
                    match offset.sign() {
                        Sign::Pos => write!(fmt, " + {}", offset)?,
                        Sign::Neg => write!(fmt, " - {}", offset.abs())?,
                    }
                }

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
    pub(crate) base: Base,
    pub(crate) offsets: Vec<Offset>,
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

        for o in &self.offsets {
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
        dbg!(Pointer::parse("0xABCDEF")?);
        dbg!(Pointer::parse("\"Steam.exe\" + 0F")?);
        dbg!(Pointer::parse("\"Steam.exe\" + 0F -> -FFAA")?);
        dbg!(Pointer::parse("\"Steam.exe\" - F0F -> -FFAA")?);
        dbg!(Pointer::parse("\"Steam.exe\" - -F0F -> -FFAA")?);
        dbg!(Pointer::parse("\"Steam.exe\" -> +FFAA")?);
        Ok(())
    }
}
