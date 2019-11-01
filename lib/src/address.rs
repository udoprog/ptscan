//! Abstraction to help deal with virtual addresses.

use crate::{
    error::Error,
    process_handle::{Location, ProcessHandle},
    Process,
};

use std::{
    convert::{TryFrom, TryInto},
    fmt, io, str,
};

use serde::{Deserialize, Serialize};

#[derive(Clone, Default, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Address(u64);

impl Address {
    /// Construct a new address.
    pub const fn new(value: u64) -> Self {
        Self(value)
    }

    pub fn decode(process: &Process, buf: &[u8]) -> Result<Self, Error> {
        use byteorder::ByteOrder as _;

        assert!(buf.len() == process.pointer_width);

        match process.pointer_width {
            8 => {
                let address = byteorder::LittleEndian::read_u64(buf);
                Ok(Address(address))
            }
            4 => {
                let address = byteorder::LittleEndian::read_u32(buf);
                Ok(Address(address as u64))
            }
            n => Err(Error::UnsupportedPointerWidth(n)),
        }
    }

    /// Encode the address in a process-dependent manner.
    pub fn encode(&self, process: &Process, buf: &mut [u8]) -> anyhow::Result<(), Error> {
        use crate::encode::Encode as _;

        assert!(buf.len() == process.pointer_width);

        match process.pointer_width {
            8 => self.0.encode(buf),
            4 => u32::try_from(self.0)
                .map_err(|_| Error::PointerConversionError)?
                .encode(buf),
            n => return Err(Error::UnsupportedPointerWidth(n)),
        }

        Ok(())
    }

    /// Add an offset in a checked manner.
    pub fn checked_offset(self, offset: Offset) -> Option<Address> {
        Some(Address(match offset {
            Offset(Sign::Pos, o) => self.0.checked_add(o)?,
            Offset(Sign::Neg, o) => self.0.checked_sub(o)?,
        }))
    }

    /// Add an offset in a saturating manner.
    pub fn saturating_offset(self, offset: Offset) -> Address {
        Address(match offset {
            Offset(Sign::Pos, o) => self.0.saturating_add(o),
            Offset(Sign::Neg, o) => self.0.saturating_sub(o),
        })
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
    pub fn offset_of(self, base: Address) -> Result<Offset, io::Error> {
        if self.0 >= base.0 {
            Ok(Offset(Sign::Pos, self.0 - base.0))
        } else {
            Ok(Offset(Sign::Neg, base.0 - self.0))
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

    /// Display the address relative to the process handle.
    pub fn display<'a>(&'a self, handle: &'a ProcessHandle) -> AddressDisplay<'a> {
        AddressDisplay {
            address: self,
            handle,
        }
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

// An address displayed with a process handle.
pub struct AddressDisplay<'a> {
    address: &'a Address,
    handle: &'a ProcessHandle,
}

impl<'a> fmt::Display for AddressDisplay<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let AddressDisplay { address, handle } = *self;

        let location = handle.find_location(*address);

        match location {
            Location::Module(module) => match address.offset_of(module.range.base).ok() {
                Some(offset) => {
                    write!(fmt, "{}{}", module.name, offset)?;
                }
                // TODO: handle error differently?
                None => {
                    write!(fmt, "{}", address)?;
                }
            },
            Location::Thread(thread) => {
                let base = thread
                    .stack_exit
                    .as_ref()
                    .cloned()
                    .unwrap_or(thread.stack.base);

                match address.offset_of(base).ok() {
                    Some(offset) => {
                        write!(fmt, "THREADSTACK{}{}", thread.id, offset)?;
                    }
                    // TODO: handle error differently?
                    None => {
                        write!(fmt, "{}", address)?;
                    }
                }
            }
            Location::None => {
                write!(fmt, "{}", address)?;
            }
        };

        Ok(())
    }
}

#[derive(Clone, Copy, Default, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[repr(transparent)]
pub struct Size(u64);

impl Size {
    /// Construct a new size.
    pub fn new(value: u64) -> Size {
        Size(value)
    }

    /// Construct a zero-size.
    pub fn zero() -> Size {
        Size(0)
    }

    /// Treat Size as a usize.
    pub fn as_usize(self) -> usize {
        self.0.try_into().expect("usize conversion failed")
    }

    /// Convert into the inner type.
    pub fn into_inner(self) -> u64 {
        self.0
    }

    /// Performed a checked add with two sizes.
    pub fn add(self, rhs: Size) -> Result<Size, Error> {
        let sum = self
            .0
            .checked_add(rhs.0)
            .ok_or_else(|| Error::Add(self.0, rhs.0))?;

        Ok(Size(sum))
    }

    /// Performed a checked add with two sizes.
    pub fn sub(self, rhs: Size) -> Result<Size, Error> {
        let sum = self
            .0
            .checked_sub(rhs.0)
            .ok_or_else(|| Error::Sub(self.0, rhs.0))?;

        Ok(Size(sum))
    }

    pub fn add_assign(&mut self, rhs: Size) -> Result<(), Error> {
        *self = self.add(rhs)?;
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
        Ok(Size(value.try_into().map_err(|_| Error::SizeConversion)?))
    }
}

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Sign {
    #[serde(rename = "pos")]
    Pos,
    #[serde(rename = "neg")]
    Neg,
}

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Offset(Sign, u64);

impl Offset {
    /// Construct a new offset.
    /// If `sign` is true, the offset is positive. Otherwise it is negative.
    pub fn new(sign: Sign, offset: u64) -> Self {
        Offset(sign, offset)
    }

    /// Initialize the zero offset.
    pub fn zero() -> Self {
        Offset(Sign::Pos, 0)
    }

    /// Add two offsets together in a saturating manner.
    pub fn saturating_add(self, offset: Offset) -> Offset {
        use self::Sign::*;

        match (self, offset) {
            (Offset(Pos, a), Offset(Pos, b)) => Offset(Pos, a.saturating_add(b)),
            (Offset(Pos, a), Offset(Neg, b)) => {
                if a >= b {
                    Offset(Pos, a - b)
                } else {
                    Offset(Neg, b - a)
                }
            }
            (Offset(Neg, a), Offset(Pos, b)) => {
                if a > b {
                    Offset(Neg, a - b)
                } else {
                    Offset(Pos, b - a)
                }
            }
            (Offset(Neg, a), Offset(Neg, b)) => Offset(Neg, a.saturating_add(b)),
        }
    }

    /// Return the sign which is `true` for positive, `false` for negative.
    pub fn sign(self) -> Sign {
        self.0
    }

    /// Return the absolute offset of this offset.
    pub fn abs(self) -> u64 {
        self.1
    }

    /// Check if the offset is within the given size.
    pub fn is_within(self, size: Size) -> bool {
        self.1 <= size.0
    }

    /// Convert into an absolute distance.
    pub fn distance(self) -> u64 {
        self.1
    }
}

impl fmt::Display for Offset {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Offset(Sign::Pos, o) => write!(fmt, "0x{:X}", o),
            Offset(Sign::Neg, o) => write!(fmt, "-0x{:X}", o),
        }
    }
}

impl fmt::Debug for Offset {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, fmt)
    }
}

/// A helper structure to define a range of addresses.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AddressRange {
    pub base: Address,
    pub size: Size,
}

impl AddressRange {
    pub fn contains(&self, value: Address) -> bool {
        self.base <= value && value <= self.base.saturating_add(self.size)
    }

    /// Helper function to find which memory region a given address is contained in.
    ///
    /// This assumes the memory regions are sorted by their `base`.
    pub fn find_in_range<T>(
        things: &Vec<T>,
        accessor: impl Fn(&T) -> &AddressRange,
        address: Address,
    ) -> Option<&T> {
        let thing = match things.binary_search_by(|m| accessor(m).base.cmp(&address)) {
            Ok(exact) => &things[exact],
            Err(closest) => {
                if closest <= 0 {
                    return None;
                }

                &things[closest - 1]
            }
        };

        if accessor(thing).contains(address) {
            return Some(thing);
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::{Offset, Sign};

    #[test]
    fn test_offsets() {
        use self::Sign::*;

        assert_eq!(
            Offset(Pos, 0),
            Offset(Neg, 0x10).saturating_add(Offset(Pos, 0x10))
        );
        assert_eq!(
            Offset(Pos, 0x10),
            Offset(Neg, 0x10).saturating_add(Offset(Pos, 0x20))
        );
        assert_eq!(
            Offset(Neg, 0x10),
            Offset(Neg, 0x20).saturating_add(Offset(Pos, 0x10))
        );

        assert_eq!(
            Offset(Pos, 0),
            Offset(Pos, 0x10).saturating_add(Offset(Neg, 0x10))
        );
        assert_eq!(
            Offset(Neg, 0x10),
            Offset(Pos, 0x10).saturating_add(Offset(Neg, 0x20))
        );
        assert_eq!(
            Offset(Pos, 0x10),
            Offset(Pos, 0x20).saturating_add(Offset(Neg, 0x10))
        );

        assert_eq!(
            Offset(Pos, 0),
            Offset(Pos, 0).saturating_add(Offset(Pos, 0))
        );
        assert_eq!(
            Offset(Pos, 0x20),
            Offset(Pos, 0x10).saturating_add(Offset(Pos, 0x10))
        );
    }
}
