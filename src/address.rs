//! Abstraction to help deal with virtual addresses.

use std::{
    convert::{TryFrom, TryInto},
    fmt,
};

#[derive(Debug, failure::Fail)]
pub enum Error {
    #[fail(display = "address {} is not based on {}", _0, _1)]
    SizeFrom(Address, Address),
    #[fail(display = "add operation `{} + {}` overflowed", _0, _1)]
    AddOverflow(u64, u64),
    #[fail(display = "sub operation `{} - {}` underflowed", _0, _1)]
    SubUnderflow(u64, u64),
}

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Address(u64);

impl Address {
    /// Construct a new address.
    pub fn new(value: u64) -> Address {
        Address(value)
    }

    /// Performed a checked add with an address and a size.
    pub fn add(self, rhs: Size) -> Result<Address, failure::Error> {
        let sum = self
            .0
            .checked_add(rhs.0)
            .ok_or_else(|| Error::AddOverflow(self.0, rhs.0))?;
        Ok(Address(sum))
    }

    /// Find how far this address offsets another one.
    pub fn offset_of(self, base: Address) -> Result<Offset, failure::Error> {
        if self.0 >= base.0 {
            Ok(Offset(true, self.0 - base.0))
        } else {
            Ok(Offset(false, base.0 - self.0))
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
    pub fn is_aligned(self, size: Size) -> Result<bool, failure::Error> {
        if size.0 == 0 {
            return Ok(false);
        }

        Ok((self.0 % size.0) == 0)
    }

    /// Try to convert into the given type.
    pub fn convert<T>(self) -> Result<T, failure::Error>
    where
        T: Convertible<Self, Error = failure::Error>,
    {
        T::convert(self)
    }

    /// Convert into usize.
    ///
    /// Internal function, use `convert` instead.
    fn into_usize(self) -> Result<usize, failure::Error> {
        Ok(self.0.try_into()?)
    }
}

/// Helper trait to handle conversions.
pub trait Convertible<T>: Sized {
    type Error;

    fn convert(value: T) -> Result<Self, Self::Error>;
}

impl<T> Convertible<Address> for *mut T {
    type Error = failure::Error;

    fn convert(value: Address) -> Result<Self, Self::Error> {
        Ok(value.into_usize()? as *mut T)
    }
}

impl<T> Convertible<Address> for *const T {
    type Error = failure::Error;

    fn convert(value: Address) -> Result<Self, Self::Error> {
        Ok(value.into_usize()? as *const T)
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

impl TryFrom<usize> for Address {
    type Error = failure::Error;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(Address(value.try_into()?))
    }
}

impl TryFrom<u32> for Address {
    type Error = failure::Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        Ok(Address(value.try_into()?))
    }
}

impl TryFrom<u64> for Address {
    type Error = failure::Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Ok(Address(value.try_into()?))
    }
}

impl<T> TryFrom<*mut T> for Address {
    type Error = failure::Error;

    fn try_from(value: *mut T) -> Result<Self, Self::Error> {
        Ok(Address((value as usize).try_into()?))
    }
}

impl<T> TryFrom<*const T> for Address {
    type Error = failure::Error;

    fn try_from(value: *const T) -> Result<Self, Self::Error> {
        Ok(Address((value as usize).try_into()?))
    }
}

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Size(u64);

impl Size {
    /// Construct a new size.
    pub fn new(value: u64) -> Size {
        Size(value)
    }

    /// Convert into the inner type.
    pub fn into_inner(self) -> u64 {
        self.0
    }

    /// Convert into u64.
    pub fn into_u64(self) -> Result<u64, failure::Error> {
        Ok(self.0.try_into()?)
    }

    /// Convert into usize.
    pub fn into_usize(self) -> Result<usize, failure::Error> {
        Ok(self.0.try_into()?)
    }

    /// Performed a checked add with two sizes.
    pub fn add(self, rhs: Size) -> Result<Size, failure::Error> {
        let sum = self
            .0
            .checked_add(rhs.0)
            .ok_or_else(|| Error::AddOverflow(self.0, rhs.0))?;

        Ok(Size(sum))
    }

    /// Performed a checked add with two sizes.
    pub fn sub(self, rhs: Size) -> Result<Size, failure::Error> {
        let sum = self
            .0
            .checked_sub(rhs.0)
            .ok_or_else(|| Error::SubUnderflow(self.0, rhs.0))?;

        Ok(Size(sum))
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
    type Error = failure::Error;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(Size(value.try_into()?))
    }
}

impl TryFrom<u64> for Size {
    type Error = failure::Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Ok(Size(value.try_into()?))
    }
}

impl TryFrom<u32> for Size {
    type Error = failure::Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        Ok(Size(value.try_into()?))
    }
}

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Offset(bool, u64);

impl fmt::Display for Offset {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Offset(sign, value) = *self;

        if sign {
            write!(fmt, "+{:X}", value)
        } else {
            write!(fmt, "-{:X}", value)
        }
    }
}

impl fmt::Debug for Offset {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, fmt)
    }
}

/// A helper structure to define a range of addresses.
#[derive(Debug, Clone, Copy)]
pub struct AddressRange {
    pub base: Address,
    pub size: Size,
}

impl AddressRange {
    pub fn contains(&self, value: Address) -> Result<bool, failure::Error> {
        Ok(self.base <= value && value <= self.base.add(self.size)?)
    }
}
