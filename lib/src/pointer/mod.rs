mod lexer;
lalrpop_util::lalrpop_mod!(
    #[allow(clippy::all)]
    parser,
    "/pointer/parser.rs"
);

use crate::{process_handle::ProcessHandle, utils::EscapeString, Address, Offset, Sign, Size};
use serde::{Deserialize, Serialize};
use std::fmt;

static NULL_POINTER: Pointer = Pointer {
    base: PointerBase::Address {
        address: Address::null(),
    },
    offsets: Vec::new(),
};

/// The base of the pointer.
///
/// Can either be a module identified by a string that has to be looked up from a `ProcessHandle`, or a fixed address.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum PointerBase {
    /// An offset from a named module.
    #[serde(rename = "module")]
    Module { name: String, offset: Offset },
    /// A fixed address.
    #[serde(rename = "address")]
    Address { address: Address },
}

impl PointerBase {
    /// Evaluate a pointer base, trying to translate it into an address.
    pub fn eval(&self, handle: &ProcessHandle) -> anyhow::Result<Option<Address>> {
        match self {
            Self::Module { name, offset, .. } => match handle.modules_address.get(name) {
                Some(address) => Ok(Some(address.saturating_offset(*offset))),
                None => Ok(None),
            },
            Self::Address { address } => Ok(Some(*address)),
        }
    }
}

impl fmt::Display for PointerBase {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PointerBase::Address { address } => fmt::Display::fmt(address, fmt),
            PointerBase::Module { name, offset } => {
                write!(fmt, "{}", EscapeString(name))?;

                if offset.abs() > Size::new(0) {
                    match offset.sign() {
                        Sign::Plus | Sign::NoSign => write!(fmt, " + {}", offset)?,
                        Sign::Minus => write!(fmt, " - {}", offset.abs())?,
                    }
                }

                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Pointer {
    /// Base address.
    pub base: PointerBase,
    /// Offsets and derefs to apply to find the given memory location.
    pub offsets: Vec<Offset>,
}

impl Pointer {
    /// Construct a new pointer.
    pub fn new(base: PointerBase, offsets: impl IntoIterator<Item = Offset>) -> Self {
        Self {
            base,
            offsets: offsets.into_iter().collect(),
        }
    }

    /// Construct the pointer associated with null.
    pub const fn null() -> Self {
        Self {
            base: PointerBase::Address {
                address: Address::null(),
            },
            offsets: Vec::new(),
        }
    }

    /// Get a null raw pointer with a static lifetime.
    pub fn null_ref() -> &'static Pointer {
        &NULL_POINTER
    }

    /// Follow, using default memory resolution.
    pub fn follow_default(&self, handle: &ProcessHandle) -> anyhow::Result<Option<Address>> {
        self.follow(handle, |a, buf| {
            handle
                .process
                .read_process_memory(a, buf)
                .map_err(Into::into)
        })
    }

    /// Try to evaluate the current location into an address.
    pub fn follow<F>(&self, handle: &ProcessHandle, eval: F) -> anyhow::Result<Option<Address>>
    where
        F: Fn(Address, &mut [u8]) -> anyhow::Result<usize>,
    {
        let address = match self.base.eval(handle)? {
            Some(address) => address,
            None => return Ok(None),
        };

        if self.offsets.is_empty() {
            return Ok(Some(address));
        }

        let mut current = address;
        let mut buf = vec![0u8; handle.process.pointer_width];

        for o in &self.offsets {
            let len = eval(current, &mut buf)?;

            if len != buf.len() {
                return Ok(None);
            }

            current = Address::decode(&handle.process, &buf)?;

            current = match current.checked_offset(*o) {
                Some(current) => current,
                None => return Ok(None),
            };
        }

        Ok(Some(current))
    }
}

impl From<Address> for Pointer {
    fn from(address: Address) -> Self {
        Self {
            base: PointerBase::Address { address },
            offsets: Vec::new(),
        }
    }
}

impl fmt::Display for Pointer {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.base)?;

        for o in &self.offsets {
            write!(fmt, " -> {}", o)?;
        }

        Ok(())
    }
}

impl Pointer {
    /// Parse a a string into a filter.
    pub fn parse(input: &str) -> Result<Pointer, anyhow::Error> {
        Ok(self::parser::PointerParser::new().parse(lexer::Lexer::new(input))?)
    }
}

#[cfg(test)]
mod tests {
    use super::Pointer;
    use std::error;

    #[test]
    fn basic_parsing() -> Result<(), Box<dyn error::Error>> {
        dbg!(Pointer::parse("0xABCDEF")?);
        dbg!(Pointer::parse("\"Steam.exe\" + 0x0F")?);
        dbg!(Pointer::parse("\"Steam.exe\" + 0x0F -> -0xFFAA")?);
        dbg!(Pointer::parse("\"Steam.exe\" - 0xF0F -> -0xFFAA")?);
        dbg!(Pointer::parse("\"Steam.exe\" - 0xF0F -> -0xFFAA")?);
        dbg!(Pointer::parse("\"Steam.exe\" -> +0xFFAA")?);
        Ok(())
    }
}
