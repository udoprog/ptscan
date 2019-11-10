use crate::error::Error;
use serde::{Deserialize, Serialize};
use std::{convert::TryFrom, fmt};

#[derive(Clone, Copy, Default, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[repr(transparent)]
pub struct Size(pub(crate) u64);

impl Size {
    /// Construct a new size.
    pub const fn new(value: u64) -> Size {
        Size(value)
    }

    /// Construct a zero-size.
    pub fn zero() -> Size {
        Size(0)
    }

    /// Treat Size as a usize.
    pub fn as_usize(self) -> usize {
        usize::try_from(self.0).expect("usize conversion failed")
    }

    /// Convert into the inner type.
    pub fn into_inner(self) -> u64 {
        self.0
    }

    /// Performed a checked add with two sizes.
    pub fn checked_add(self, rhs: Size) -> Result<Size, Error> {
        let sum = self
            .0
            .checked_add(rhs.0)
            .ok_or_else(|| Error::Add(self.0, rhs.0))?;

        Ok(Size(sum))
    }

    /// Performed a checked add with two sizes.
    pub fn checked_sub(self, rhs: Size) -> Result<Size, Error> {
        let sum = self
            .0
            .checked_sub(rhs.0)
            .ok_or_else(|| Error::Sub(self.0, rhs.0))?;

        Ok(Size(sum))
    }

    pub fn add_assign(&mut self, rhs: Size) -> Result<(), Error> {
        *self = self.checked_add(rhs)?;
        Ok(())
    }

    pub fn min(a: Size, b: Size) -> Size {
        Size(u64::min(a.0, b.0))
    }
}

impl From<u32> for Size {
    fn from(value: u32) -> Self {
        Self(value as u64)
    }
}

impl From<u64> for Size {
    fn from(value: u64) -> Self {
        Self(value)
    }
}

impl fmt::Display for Size {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{:X}", self.0)
    }
}

impl fmt::Debug for Size {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, fmt)
    }
}

impl TryFrom<usize> for Size {
    type Error = Error;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(Size(
            u64::try_from(value).map_err(|_| Error::SizeConversion)?,
        ))
    }
}
