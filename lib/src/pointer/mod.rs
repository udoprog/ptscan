mod lexer;
lalrpop_util::lalrpop_mod!(
    #[allow(clippy::all)]
    parser,
    "/pointer/parser.rs"
);

use crate::{
    process_handle::ProcessHandle, utils::EscapeString, Address, Offset, PointerWidth, ProcessInfo,
    Sign, Size,
};
use serde::{Deserialize, Serialize};
use std::fmt;

/// Defines how you follow a pointer.
pub trait FollowablePointer {
    /// Follow the pointer.
    fn follow_default(&self, handle: &ProcessHandle) -> anyhow::Result<Option<Address>>;

    /// Follow the pointer using a custom resolver.
    fn follow<F>(&self, handle: &ProcessHandle, eval: F) -> anyhow::Result<Option<Address>>
    where
        F: Fn(Address, &mut [u8]) -> anyhow::Result<usize>;
}

pub trait PointerInfo {
    type ByteOrder: byteorder::ByteOrder;

    /// Get the width of a pointer in the system.
    fn pointer_width(&self) -> PointerWidth;

    /// Encode the pointer.
    fn encode_pointer(&self, buf: &mut [u8], value: u64) -> bool;
}

impl<T> PointerInfo for T
where
    T: ProcessInfo,
{
    type ByteOrder = T::ByteOrder;

    fn pointer_width(&self) -> PointerWidth {
        ProcessInfo::pointer_width(self)
    }

    fn encode_pointer(&self, buf: &mut [u8], value: u64) -> bool {
        ProcessInfo::encode_pointer(self, buf, value)
    }
}

#[cfg(test)]
impl PointerInfo for usize {
    fn pointer_width(&self) -> usize {
        *self
    }
}

static NULL_POINTER: Pointer = Pointer {
    base: Base::Null,
    offsets: Vec::new(),
};

/// A portable base of a pointer.
///
/// Can either be a module identified by a string that has to be looked up from a `ProcessHandle`, or a fixed address.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum PortableBase {
    Null,
    /// A non-portable module, who's indexed in a specific `Handle`.
    Module {
        name: String,
        offset: Offset,
    },
    /// A fixed address.
    Address {
        address: Address,
    },
}

impl PortableBase {
    pub fn as_local(&self, handle: &ProcessHandle) -> Option<Base> {
        Some(match *self {
            Self::Null => Base::Null,
            Self::Module { ref name, offset } => {
                let id = handle.modules.modules_index.get(name).cloned();

                let id = match id {
                    Some(id) => id,
                    None => return None,
                };

                Base::Module { id, offset }
            }
            Self::Address { address } => Base::Address { address },
        })
    }

    /// Evaluate a pointer base, trying to translate it into an address.
    pub fn eval(&self, handle: &ProcessHandle) -> anyhow::Result<Option<Address>> {
        match *self {
            Self::Null => Ok(None),
            Self::Module {
                ref name, offset, ..
            } => match handle.modules.modules_address.get(name) {
                Some(address) => Ok(Some(address.saturating_offset(offset))),
                None => Ok(None),
            },
            Self::Address { address } => Ok(Some(address)),
        }
    }
}

impl fmt::Display for PortableBase {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Null => write!(fmt, "?"),
            Self::Address { ref address } => fmt::Display::fmt(address, fmt),
            Self::Module { ref name, offset } => {
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

/// The base of the pointer.
///
/// Can either be a module identified by a string that has to be looked up from a `ProcessHandle`, or a fixed address.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Base {
    Null,
    /// A non-portable module, who's indexed in a specific `Handle`.
    Module {
        id: u16,
        offset: Offset,
    },
    /// A fixed address.
    Address {
        address: Address,
    },
}

impl Base {
    /// Convert base into a portable base.
    pub fn as_portable(&self, handle: &ProcessHandle) -> Option<PortableBase> {
        Some(match *self {
            Self::Null => PortableBase::Null,
            Self::Module { id, offset } => {
                let module = handle.modules.modules.get(id as usize);

                let module = match module {
                    Some(module) => module,
                    None => return None,
                };

                PortableBase::Module {
                    name: module.name.clone(),
                    offset,
                }
            }
            Self::Address { address } => PortableBase::Address { address },
        })
    }

    /// Evaluate a pointer base, trying to translate it into an address.
    pub fn eval(&self, handle: &ProcessHandle) -> anyhow::Result<Option<Address>> {
        match *self {
            Self::Null => Ok(None),
            Self::Module { id, offset, .. } => match handle.modules.modules.get(usize::from(id)) {
                Some(m) => Ok(Some(m.range.base.saturating_offset(offset))),
                None => Ok(None),
            },
            Self::Address { address } => Ok(Some(address)),
        }
    }
}

impl FollowablePointer for Base {
    fn follow_default(&self, handle: &ProcessHandle) -> anyhow::Result<Option<Address>> {
        self.follow(handle, |a, buf| {
            handle
                .process
                .read_process_memory(a, buf)
                .map_err(Into::into)
        })
    }

    /// Try to evaluate the current location into an address.
    fn follow<F>(&self, handle: &ProcessHandle, _: F) -> anyhow::Result<Option<Address>>
    where
        F: Fn(Address, &mut [u8]) -> anyhow::Result<usize>,
    {
        self.eval(handle)
    }
}

impl fmt::Display for Base {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Null => write!(fmt, "?"),
            Self::Address { ref address } => fmt::Display::fmt(address, fmt),
            Self::Module { id, ref offset } => {
                write!(fmt, "(module:{})", id)?;

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
pub struct PortablePointer {
    /// Base address.
    pub base: PortableBase,
    /// Offsets and derefs to apply to find the given memory location.
    pub offsets: Vec<Offset>,
}

impl PortablePointer {
    /// Create a null pointer.
    pub fn null() -> Self {
        PortablePointer {
            base: PortableBase::Null,
            offsets: Vec::new(),
        }
    }

    /// Parse a a string into a portable pointer.
    pub fn parse(input: &str) -> Result<Self, anyhow::Error> {
        let portable = self::parser::PointerParser::new().parse(lexer::Lexer::new(input))?;
        Ok(portable)
    }

    /// Construct a new pointer.
    pub fn new(base: PortableBase, offsets: impl IntoIterator<Item = Offset>) -> Self {
        Self {
            base,
            offsets: offsets.into_iter().collect(),
        }
    }

    /// Convert into a process-local pointer.
    pub fn as_local(&self, handle: &ProcessHandle) -> Option<Pointer> {
        Some(Pointer {
            base: self.base.as_local(handle)?,
            offsets: self.offsets.clone(),
        })
    }
}

impl FollowablePointer for PortablePointer {
    fn follow_default(&self, handle: &ProcessHandle) -> anyhow::Result<Option<Address>> {
        self.follow(handle, |a, buf| {
            handle
                .process
                .read_process_memory(a, buf)
                .map_err(Into::into)
        })
    }

    /// Try to evaluate the current location into an address.
    fn follow<F>(&self, handle: &ProcessHandle, eval: F) -> anyhow::Result<Option<Address>>
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
        let mut buf = vec![0u8; handle.process.pointer_width.size()];

        for o in &self.offsets {
            let len = eval(current, &mut buf)?;

            if len != buf.len() {
                return Ok(None);
            }

            current = handle.process.decode_pointer(&buf);

            current = match current.checked_offset(*o) {
                Some(current) => current,
                None => return Ok(None),
            };
        }

        Ok(Some(current))
    }
}

impl fmt::Display for PortablePointer {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.base)?;

        for o in &self.offsets {
            write!(fmt, " -> {}", o)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer {
    /// Base address.
    pub base: Base,
    /// Offsets and derefs to apply to find the given memory location.
    pub offsets: Vec<Offset>,
}

impl Pointer {
    /// Construct a new pointer.
    pub fn new(base: Base, offsets: impl IntoIterator<Item = Offset>) -> Self {
        Self {
            base,
            offsets: offsets.into_iter().collect(),
        }
    }

    /// Construct the pointer associated with null.
    pub const fn null() -> Self {
        Self {
            base: Base::Null,
            offsets: Vec::new(),
        }
    }

    /// Get a null raw pointer with a static lifetime.
    pub fn null_ref() -> &'static Pointer {
        &NULL_POINTER
    }
}

impl From<Address> for Pointer {
    fn from(address: Address) -> Self {
        Self {
            base: Base::Address { address },
            offsets: Vec::new(),
        }
    }
}

impl From<Base> for Pointer {
    fn from(base: Base) -> Self {
        Self {
            base,
            offsets: Vec::new(),
        }
    }
}

impl FollowablePointer for Pointer {
    fn follow_default(&self, handle: &ProcessHandle) -> anyhow::Result<Option<Address>> {
        self.follow(handle, |a, buf| {
            handle
                .process
                .read_process_memory(a, buf)
                .map_err(Into::into)
        })
    }

    /// Try to evaluate the current location into an address.
    fn follow<F>(&self, handle: &ProcessHandle, eval: F) -> anyhow::Result<Option<Address>>
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
        let mut buf = vec![0u8; handle.process.pointer_width.size()];

        for o in &self.offsets {
            let len = eval(current, &mut buf)?;

            if len != buf.len() {
                return Ok(None);
            }

            current = handle.process.decode_pointer(&buf);

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

        Ok(())
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
