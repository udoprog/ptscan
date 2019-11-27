use std::{convert::TryFrom, fmt, num, str};

#[derive(Clone, Copy, Default, PartialOrd, Ord, PartialEq, Eq, Hash)]
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
    pub fn checked_add(self, rhs: Size) -> Option<Size> {
        Some(Size(self.0.checked_add(rhs.0)?))
    }

    /// Performed a checked add with two sizes.
    pub fn checked_sub(self, rhs: Size) -> Option<Size> {
        Some(Size(self.0.checked_sub(rhs.0)?))
    }

    pub fn add_assign(&mut self, rhs: Size) -> bool {
        if let Some(added) = self.checked_add(rhs) {
            *self = added;
            true
        } else {
            false
        }
    }

    pub fn min(a: Size, b: Size) -> Size {
        Size(u64::min(a.0, b.0))
    }
}

#[derive(Debug, thiserror::Error)]
#[error("failed to parse size")]
pub struct ParseError;

impl str::FromStr for Size {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = if s.starts_with("0x") { &s[2..] } else { s };
        Ok(Size(u64::from_str_radix(s, 16).map_err(|_| ParseError)?))
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
        write!(fmt, "0x{:X}", self.0)
    }
}

impl fmt::Debug for Size {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, fmt)
    }
}

impl TryFrom<usize> for Size {
    type Error = num::TryFromIntError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(Size(u64::try_from(value)?))
    }
}

impl TryFrom<Size> for u64 {
    type Error = num::TryFromIntError;

    fn try_from(value: Size) -> Result<Self, Self::Error> {
        Ok(u64::try_from(value.0)?)
    }
}

impl serde::ser::Serialize for Size {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer
    {
        serializer.collect_str(self)
    }
}

impl<'de> serde::de::Deserialize<'de> for Size {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>
    {
        let s = String::deserialize(deserializer)?;
        str::parse::<Size>(&s).map_err(serde::de::Error::custom)
    }
}
