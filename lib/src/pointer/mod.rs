mod lexer;
lalrpop_util::lalrpop_mod!(parser, "/pointer/parser.rs");

use crate::{address::Sign, process_handle::ProcessHandle, utils::EscapeString, Address, Offset};
use serde::{Deserialize, Serialize};
use std::fmt;

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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Pointer {
    /// Base address.
    pub base: PointerBase,
    /// Offsets and derefs to apply to find the given memory location.
    pub offsets: Vec<Offset>,
    pub last_address: Option<Address>,
}

impl Pointer {
    /// Construct a new pointer.
    pub fn new(
        base: PointerBase,
        offsets: impl IntoIterator<Item = Offset>,
        last_address: Option<Address>,
    ) -> Self {
        Self {
            base,
            offsets: offsets.into_iter().collect(),
            last_address,
        }
    }

    /// Get the best known address for the current pointer.
    pub fn address(&self) -> Option<Address> {
        match self.base {
            PointerBase::Address { address } => Some(address),
            PointerBase::Module { .. } => self.last_address,
        }
    }

    /// Construct a new pointer from an address.
    pub fn from_address(address: Address) -> Self {
        Self::new(PointerBase::Address { address }, vec![], None)
    }

    /// Follow, using default memory resolution.
    pub fn follow_default(&self, handle: &ProcessHandle) -> anyhow::Result<Option<Address>> {
        Self::do_follow_default(&self.base, &self.offsets, handle)
    }

    /// Try to evaluate the current location into an address.
    pub fn follow<F>(&self, handle: &ProcessHandle, eval: F) -> anyhow::Result<Option<Address>>
    where
        F: Fn(Address, &mut [u8]) -> anyhow::Result<bool>,
    {
        Self::do_follow(&self.base, &self.offsets, handle, eval)
    }

    /// Follow, using default memory resolution.
    pub fn do_follow_default(
        base: &PointerBase,
        offsets: &[Offset],
        handle: &ProcessHandle,
    ) -> anyhow::Result<Option<Address>> {
        Self::do_follow(base, offsets, handle, |a, buf| {
            handle
                .process
                .read_process_memory(a, buf)
                .map_err(Into::into)
        })
    }

    /// Try to evaluate the current location into an address.
    pub fn do_follow<F>(
        base: &PointerBase,
        offsets: &[Offset],
        handle: &ProcessHandle,
        eval: F,
    ) -> anyhow::Result<Option<Address>>
    where
        F: Fn(Address, &mut [u8]) -> anyhow::Result<bool>,
    {
        let address = match base.eval(handle)? {
            Some(address) => address,
            None => return Ok(None),
        };

        if offsets.is_empty() {
            return Ok(Some(address));
        }

        let mut current = address;
        let mut buf = vec![0u8; handle.process.pointer_width];

        for o in offsets {
            eval(current, &mut buf)?;
            current = Address::decode(&handle.process, &buf)?;

            current = match current.checked_offset(*o) {
                Some(current) => current,
                None => return Ok(None),
            };
        }

        Ok(Some(current))
    }
}

impl fmt::Display for Pointer {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.base)?;

        for o in &self.offsets {
            write!(fmt, " -> {}", o)?;
        }

        let last_address = match (&self.base, self.last_address) {
            (PointerBase::Address { address }, Some(last_address)) if *address != last_address => {
                Some(last_address)
            }
            (PointerBase::Module { .. }, Some(address)) => Some(address),
            _ => None,
        };

        if let Some(address) = last_address {
            write!(fmt, " => {}", address)?;
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