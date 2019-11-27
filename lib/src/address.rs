//! Abstraction to help deal with virtual addresses.

use crate::{error::Error, Offset, ProcessInfo, Sign, Size};

use std::{
    convert::{self, TryFrom, TryInto},
    fmt, num, str,
};

#[derive(Debug, thiserror::Error)]
#[error("failed to parse address")]
pub struct ParseError;

#[derive(Clone, Default, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Address(pub(crate) u64);

impl Address {
    /// Construct a new address.
    pub const fn new(value: u64) -> Self {
        Self(value)
    }

    /// Construct a null pointer.
    pub const fn null() -> Self {
        Self(0)
    }

    /// If the address is null.
    pub fn is_null(self) -> bool {
        self.0 == 0
    }

    pub fn decode<P>(process: &P, buf: &[u8]) -> Result<Self, Error>
    where
        P: ProcessInfo,
    {
        assert!(
            buf.len() == process.pointer_width(),
            "{} (buffer length) != {} (pointer width)",
            buf.len(),
            process.pointer_width()
        );

        match process.pointer_width() {
            8 => {
                let address = <P::ByteOrder as byteorder::ByteOrder>::read_u64(buf);
                Ok(Address(address))
            }
            4 => {
                let address = <P::ByteOrder as byteorder::ByteOrder>::read_u32(buf);
                Ok(Address(address as u64))
            }
            n => Err(Error::UnsupportedPointerWidth(n)),
        }
    }

    /// Encode the address in a process-dependent manner.
    pub fn encode(self, process: &impl ProcessInfo, buf: &mut [u8]) -> anyhow::Result<(), Error> {
        Ok(process.encode_pointer(buf, self.0)?)
    }

    /// Try to add one address to another.
    pub fn checked_add(self, other: Address) -> Option<Self> {
        Some(Address(self.0.checked_add(other.0)?))
    }

    /// Try to subtract one address from another.
    pub fn checked_sub(self, other: Address) -> Option<Self> {
        Some(Address(self.0.checked_sub(other.0)?))
    }

    /// Try to multiply two addresses.
    pub fn checked_mul(self, other: Address) -> Option<Self> {
        Some(Address(self.0.checked_mul(other.0)?))
    }

    /// Try to divide two addresses.
    pub fn checked_div(self, other: Address) -> Option<Self> {
        Some(Address(self.0.checked_div(other.0)?))
    }

    pub fn checked_add_size(self, rhs: Size) -> Option<Self> {
        Some(Address(self.0.checked_add(rhs.0)?))
    }

    /// Add a size to the current address.
    pub fn checked_add_assign_size(&mut self, rhs: Size) -> bool {
        if let Some(added) = self.0.checked_add(rhs.0) {
            self.0 = added;
            true
        } else {
            false
        }
    }

    /// Add an offset in a checked manner.
    pub fn checked_offset(self, offset: Offset) -> Option<Address> {
        Some(match offset {
            Offset {
                sign: Sign::NoSign, ..
            } => self,
            Offset {
                sign: Sign::Plus,
                offset: o,
            } => Address(self.0.checked_add(o.0)?),
            Offset {
                sign: Sign::Minus,
                offset: o,
            } => Address(self.0.checked_sub(o.0)?),
        })
    }

    /// Add an offset in a saturating manner.
    pub fn saturating_offset(self, offset: Offset) -> Address {
        match offset {
            Offset {
                sign: Sign::NoSign, ..
            } => self,
            Offset {
                sign: Sign::Plus,
                offset: o,
            } => Address(self.0.saturating_add(o.0)),
            Offset {
                sign: Sign::Minus,
                offset: o,
            } => Address(self.0.saturating_sub(o.0)),
        }
    }

    /// Add the given size in a checked manner.
    pub fn checked_size(self, rhs: Size) -> Option<Address> {
        Some(Address(self.0.checked_add(rhs.0)?))
    }

    /// Add the given size in a saturating manner.
    pub fn saturating_add_size(self, rhs: Size) -> Address {
        Address(self.0.saturating_add(rhs.0))
    }

    pub fn align_assign(&mut self, alignment: Size) -> Result<(), Error> {
        let rem = self.0 % alignment.0;

        if rem == 0 {
            return Ok(());
        }

        self.0 -= rem;
        Ok(())
    }

    /// Find how far this address offsets another one.
    pub fn offset_of(self, base: Address) -> Offset {
        if self.0 > base.0 {
            Offset::new(Sign::Plus, self.0.saturating_sub(base.0).into())
        } else if self.0 < base.0 {
            Offset::new(Sign::Minus, base.0.saturating_sub(self.0).into())
        } else {
            Offset::zero()
        }
    }

    /// Safely convert two addresses into a non-negative size.
    pub fn size_from(self, base: Address) -> Option<Size> {
        if self.0 < base.0 {
            return None;
        }

        Some(Size(self.0 - base.0))
    }

    /// Test if the current address is aligned with the given size.
    pub fn is_aligned(self, size: Size) -> bool {
        size.0 != 0 && (self.0 % size.0) == 0
    }

    /// Try to convert into the given type.
    pub fn convert<T>(self) -> Result<T, Error>
    where
        T: Convertible<Self, Error = Error>,
    {
        T::convert(self)
    }

    /// Convert into usize.
    ///
    /// Internal function, use `convert` instead.
    pub fn as_usize(self) -> usize {
        self.0.try_into().expect("usize conversion failed")
    }
}

impl str::FromStr for Address {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = if s.starts_with("0x") { &s[2..] } else { s };
        Ok(Address(u64::from_str_radix(s, 16).map_err(|_| ParseError)?))
    }
}

impl fmt::Display for Address {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "0x{:X}", self.0)
    }
}

impl fmt::Debug for Address {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, fmt)
    }
}

/// Helper trait to handle conversions.
pub trait Convertible<T>: Sized {
    type Error;

    fn convert(value: T) -> Result<Self, Self::Error>;
}

impl<T> Convertible<Address> for *mut T {
    type Error = Error;

    fn convert(value: Address) -> Result<Self, Self::Error> {
        Ok(value.as_usize() as *mut T)
    }
}

impl<T> Convertible<Address> for *const T {
    type Error = Error;

    fn convert(value: Address) -> Result<Self, Self::Error> {
        Ok(value.as_usize() as *const T)
    }
}

macro_rules! try_from_int {
    ($($ty:ty, $error:ty),*) => {
        $(
        impl TryFrom<Address> for $ty {
            type Error = $error;

            fn try_from(value: Address) -> Result<Self, Self::Error> {
                <$ty>::try_from(value.0)
            }
        }
        )*
    }
}

try_from_int!(
    u8,
    num::TryFromIntError,
    i8,
    num::TryFromIntError,
    u16,
    num::TryFromIntError,
    i16,
    num::TryFromIntError,
    u32,
    num::TryFromIntError,
    i32,
    num::TryFromIntError,
    u64,
    convert::Infallible,
    i64,
    num::TryFromIntError,
    u128,
    convert::Infallible,
    i128,
    convert::Infallible,
    usize,
    num::TryFromIntError
);

impl TryFrom<usize> for Address {
    type Error = Error;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(Address(
            value.try_into().map_err(|_| Error::AddressConversion)?,
        ))
    }
}

impl TryFrom<u128> for Address {
    type Error = Error;

    fn try_from(value: u128) -> Result<Self, Self::Error> {
        Ok(Address(
            value.try_into().map_err(|_| Error::AddressConversion)?,
        ))
    }
}

impl TryFrom<i128> for Address {
    type Error = Error;

    fn try_from(value: i128) -> Result<Self, Self::Error> {
        Ok(Address(
            value.try_into().map_err(|_| Error::AddressConversion)?,
        ))
    }
}

impl From<u64> for Address {
    fn from(value: u64) -> Self {
        Address(value)
    }
}

impl TryFrom<i64> for Address {
    type Error = Error;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        Ok(Address(
            value.try_into().map_err(|_| Error::AddressConversion)?,
        ))
    }
}

impl From<u32> for Address {
    fn from(value: u32) -> Self {
        Address(value.into())
    }
}

impl TryFrom<i32> for Address {
    type Error = Error;

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        Ok(Address(
            value.try_into().map_err(|_| Error::AddressConversion)?,
        ))
    }
}

impl From<u16> for Address {
    fn from(value: u16) -> Self {
        Address(value.into())
    }
}

impl TryFrom<i16> for Address {
    type Error = Error;

    fn try_from(value: i16) -> Result<Self, Self::Error> {
        Ok(Address(
            value.try_into().map_err(|_| Error::AddressConversion)?,
        ))
    }
}

impl From<u8> for Address {
    fn from(value: u8) -> Self {
        Address(value.into())
    }
}

impl TryFrom<i8> for Address {
    type Error = Error;

    fn try_from(value: i8) -> Result<Self, Self::Error> {
        Ok(Address(
            value.try_into().map_err(|_| Error::AddressConversion)?,
        ))
    }
}

impl<T> TryFrom<*mut T> for Address {
    type Error = Error;

    fn try_from(value: *mut T) -> Result<Self, Self::Error> {
        Ok(Address(
            (value as usize)
                .try_into()
                .map_err(|_| Error::AddressConversion)?,
        ))
    }
}

impl<T> TryFrom<*const T> for Address {
    type Error = Error;

    fn try_from(value: *const T) -> Result<Self, Self::Error> {
        Ok(Address(
            (value as usize)
                .try_into()
                .map_err(|_| Error::AddressConversion)?,
        ))
    }
}

impl serde::ser::Serialize for Address {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        serializer.collect_str(self)
    }
}

impl<'de> serde::de::Deserialize<'de> for Address {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        str::parse::<Address>(&s).map_err(serde::de::Error::custom)
    }
}
