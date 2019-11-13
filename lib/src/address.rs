//! Abstraction to help deal with virtual addresses.

use crate::{error::Error, Offset, ProcessInfo, Sign, Size};

use std::{
    convert::{self, TryFrom, TryInto},
    fmt, io, num, str,
};

use serde::{Deserialize, Serialize};

#[derive(Clone, Default, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize, Deserialize)]
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

    /// Add an offset in a checked manner.
    pub fn checked_offset(self, offset: Offset) -> Option<Address> {
        Some(match offset {
            Offset(Sign::NoSign, _) => self,
            Offset(Sign::Plus, o) => Address(self.0.checked_add(o)?),
            Offset(Sign::Minus, o) => Address(self.0.checked_sub(o)?),
        })
    }

    /// Add an offset in a saturating manner.
    pub fn saturating_offset(self, offset: Offset) -> Address {
        match offset {
            Offset(Sign::NoSign, _) => self,
            Offset(Sign::Plus, o) => Address(self.0.saturating_add(o)),
            Offset(Sign::Minus, o) => Address(self.0.saturating_sub(o)),
        }
    }

    /// Add the given size in a saturating manner.
    pub fn saturating_add(self, rhs: Size) -> Address {
        let sum = self.0.saturating_add(rhs.0);

        Address(sum)
    }

    /// Performed a checked add with an address and a size.
    pub fn add(self, rhs: Size) -> Result<Address, Error> {
        let sum = self
            .0
            .checked_add(rhs.0)
            .ok_or_else(|| Error::AddressAdd(self, rhs))?;

        Ok(Address(sum))
    }

    /// Add a size to the current address.
    pub fn add_assign(&mut self, rhs: Size) -> Result<(), Error> {
        *self = self.add(rhs)?;
        Ok(())
    }

    pub fn align_assign(&mut self, alignment: Size) -> Result<(), Error> {
        let rem = self.0 % alignment.0;

        if rem == 0 {
            return Ok(());
        }

        self.0 -= rem;
        Ok(())
    }

    /// Performed a checked subtraction between two addresses.
    pub fn sub_address(self, rhs: Address) -> Result<Address, Error> {
        let sum = self
            .0
            .checked_sub(rhs.0)
            .ok_or_else(|| Error::Sub(self.0, rhs.0))?;

        Ok(Address(sum))
    }

    /// Find how far this address offsets another one.
    pub fn offset_of(self, base: Address) -> Offset {
        if self.0 > base.0 {
            Offset(Sign::Plus, self.0.saturating_sub(base.0))
        } else if self.0 < base.0 {
            Offset(Sign::Minus, base.0.saturating_sub(self.0))
        } else {
            Offset(Sign::NoSign, 0)
        }
    }

    /// Safely convert two addresses into a non-negative size.
    pub fn size_from(self, base: Address) -> Result<Size, Error> {
        if self.0 < base.0 {
            return Err(Error::SizeFrom(self, base));
        }

        Ok(Size(self.0 - base.0))
    }

    /// Test if the current address is aligned with the given size.
    pub fn is_aligned(self, size: Size) -> Result<bool, io::Error> {
        if size.0 == 0 {
            return Ok(false);
        }

        Ok((self.0 % size.0) == 0)
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
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = if s.starts_with("0x") { &s[2..] } else { s };

        Ok(Address(
            u64::from_str_radix(s, 16).map_err(|_| Error::AddressFromStr)?,
        ))
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

impl TryFrom<u64> for Address {
    type Error = Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Ok(Address(
            value.try_into().map_err(|_| Error::AddressConversion)?,
        ))
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

impl TryFrom<u32> for Address {
    type Error = Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        Ok(Address(
            value.try_into().map_err(|_| Error::AddressConversion)?,
        ))
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

impl TryFrom<u16> for Address {
    type Error = Error;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        Ok(Address(
            value.try_into().map_err(|_| Error::AddressConversion)?,
        ))
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

impl TryFrom<u8> for Address {
    type Error = Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(Address(
            value.try_into().map_err(|_| Error::AddressConversion)?,
        ))
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
