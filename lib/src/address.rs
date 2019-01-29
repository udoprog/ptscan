//! Abstraction to help deal with virtual addresses.

use crate::scan;
use err_derive::Error;
use std::{
    convert::{TryFrom, TryInto},
    error, fmt, io, str,
};

#[derive(Debug, Error)]
pub enum Error {
    #[error(display = "address {} is not based on {}", _0, _1)]
    SizeFrom(Address, Address),
    #[error(display = "add operation `{} + {}` overflowed", _0, _1)]
    AddOverflow(u64, u64),
    #[error(display = "sub operation `{} - {}` underflowed", _0, _1)]
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
    pub fn add(self, rhs: Size) -> Result<Address, io::Error> {
        let sum = self.0.checked_add(rhs.0).ok_or_else(|| {
            io::Error::new(io::ErrorKind::Other, Error::AddOverflow(self.0, rhs.0))
        })?;
        Ok(Address(sum))
    }

    /// Find how far this address offsets another one.
    pub fn offset_of(self, base: Address) -> Result<Offset, io::Error> {
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
    pub fn is_aligned(self, size: Size) -> Result<bool, io::Error> {
        if size.0 == 0 {
            return Ok(false);
        }

        Ok((self.0 % size.0) == 0)
    }

    /// Try to convert into the given type.
    pub fn convert<T>(self) -> Result<T, io::Error>
    where
        T: Convertible<Self, Error = io::Error>,
    {
        T::convert(self)
    }

    /// Convert into usize.
    ///
    /// Internal function, use `convert` instead.
    fn into_usize(self) -> Result<usize, io::Error> {
        Ok(self.0.try_into().map_err(into_io_error)?)
    }
}

impl str::FromStr for Address {
    type Err = io::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = if s.starts_with("0x") { &s[2..] } else { s };

        Ok(Address(u64::from_str_radix(s, 16).map_err(into_io_error)?))
    }
}

/// Helper trait to handle conversions.
pub trait Convertible<T>: Sized {
    type Error;

    fn convert(value: T) -> Result<Self, Self::Error>;
}

impl<T> Convertible<Address> for *mut T {
    type Error = io::Error;

    fn convert(value: Address) -> Result<Self, Self::Error> {
        Ok(value.into_usize()? as *mut T)
    }
}

impl<T> Convertible<Address> for *const T {
    type Error = io::Error;

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
    type Error = io::Error;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(Address(value.try_into().map_err(into_io_error)?))
    }
}

impl TryFrom<u128> for Address {
    type Error = io::Error;

    fn try_from(value: u128) -> Result<Self, Self::Error> {
        Ok(Address(value.try_into().map_err(into_io_error)?))
    }
}

impl TryFrom<i128> for Address {
    type Error = io::Error;

    fn try_from(value: i128) -> Result<Self, Self::Error> {
        Ok(Address(value.try_into().map_err(into_io_error)?))
    }
}

impl TryFrom<u64> for Address {
    type Error = io::Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Ok(Address(value.try_into().map_err(into_io_error)?))
    }
}

impl TryFrom<i64> for Address {
    type Error = io::Error;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        Ok(Address(value.try_into().map_err(into_io_error)?))
    }
}

impl TryFrom<u32> for Address {
    type Error = io::Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        Ok(Address(value.try_into().map_err(into_io_error)?))
    }
}

impl TryFrom<i32> for Address {
    type Error = io::Error;

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        Ok(Address(value.try_into().map_err(into_io_error)?))
    }
}

impl TryFrom<u16> for Address {
    type Error = io::Error;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        Ok(Address(value.try_into().map_err(into_io_error)?))
    }
}

impl TryFrom<i16> for Address {
    type Error = io::Error;

    fn try_from(value: i16) -> Result<Self, Self::Error> {
        Ok(Address(value.try_into().map_err(into_io_error)?))
    }
}

impl TryFrom<u8> for Address {
    type Error = io::Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(Address(value.try_into().map_err(into_io_error)?))
    }
}

impl TryFrom<i8> for Address {
    type Error = io::Error;

    fn try_from(value: i8) -> Result<Self, Self::Error> {
        Ok(Address(value.try_into().map_err(into_io_error)?))
    }
}

impl<T> TryFrom<*mut T> for Address {
    type Error = io::Error;

    fn try_from(value: *mut T) -> Result<Self, Self::Error> {
        Ok(Address((value as usize).try_into().map_err(into_io_error)?))
    }
}

impl<T> TryFrom<*const T> for Address {
    type Error = io::Error;

    fn try_from(value: *const T) -> Result<Self, Self::Error> {
        Ok(Address((value as usize).try_into().map_err(into_io_error)?))
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
    pub fn into_u64(self) -> Result<u64, io::Error> {
        Ok(self.0.try_into().map_err(into_io_error)?)
    }

    /// Convert into usize.
    pub fn into_usize(self) -> Result<usize, io::Error> {
        Ok(self.0.try_into().map_err(into_io_error)?)
    }

    /// Performed a checked add with two sizes.
    pub fn add(self, rhs: Size) -> Result<Size, io::Error> {
        let sum = self
            .0
            .checked_add(rhs.0)
            .ok_or_else(|| Error::AddOverflow(self.0, rhs.0))
            .map_err(into_io_error)?;

        Ok(Size(sum))
    }

    /// Performed a checked add with two sizes.
    pub fn sub(self, rhs: Size) -> Result<Size, io::Error> {
        let sum = self
            .0
            .checked_sub(rhs.0)
            .ok_or_else(|| Error::SubUnderflow(self.0, rhs.0))
            .map_err(into_io_error)?;

        Ok(Size(sum))
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
    type Error = io::Error;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(Size(value.try_into().map_err(into_io_error)?))
    }
}

impl TryFrom<u64> for Size {
    type Error = io::Error;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        Ok(Size(value.try_into().map_err(into_io_error)?))
    }
}

impl TryFrom<u32> for Size {
    type Error = io::Error;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        Ok(Size(value.try_into().map_err(into_io_error)?))
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
    pub fn contains(&self, value: Address) -> Result<bool, io::Error> {
        Ok(self.base <= value && value <= self.base.add(self.size)?)
    }

    /// Helper function to find which memory region a given address is contained in.
    ///
    /// This assumes the memory regions are sorted by their `base`.
    pub fn find_in_range<T>(
        things: &Vec<T>,
        accessor: impl Fn(&T) -> &AddressRange,
        address: Address,
    ) -> Result<Option<&T>, io::Error> {
        let thing = match things.binary_search_by(|m| accessor(m).base.cmp(&address)) {
            Ok(exact) => &things[exact],
            Err(closest) => {
                if closest <= 0 {
                    return Ok(None);
                }

                &things[closest - 1]
            }
        };

        if accessor(thing).contains(address)? {
            return Ok(Some(thing));
        }

        Ok(None)
    }
}

/// A pointer path.
///
/// Each offset is applied and dereferenced on the base in a chain.
///
/// The last step is dereferences as the pointee type.
#[allow(unused)]
pub struct Pointer {
    pointee_type: scan::Type,
    base: Address,
    offset: Vec<Offset>,
}

/// Convert the given error into an I/O error.
fn into_io_error<E: 'static + Send + Sync + error::Error>(input: E) -> io::Error {
    io::Error::new(io::ErrorKind::Other, input)
}
