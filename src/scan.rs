//! Predicates used for matching against memory.

use byteorder::{ByteOrder, LittleEndian};
use std::{fmt, mem, str};

/// A single dynamic literal value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    U128(u128),
    I128(i128),
    U64(u64),
    I64(i64),
    U32(u32),
    I32(i32),
    U16(u16),
    I16(i16),
    U8(u8),
    I8(i8),
}

impl Value {
    /// Decode the given buffer into a value.
    pub fn encode(&self, buf: &mut [u8]) {
        use self::Value::*;

        match *self {
            U128(value) => u128::encode(buf, value),
            I128(value) => i128::encode(buf, value),
            U64(value) => u64::encode(buf, value),
            I64(value) => i64::encode(buf, value),
            U32(value) => u32::encode(buf, value),
            I32(value) => i32::encode(buf, value),
            U16(value) => u16::encode(buf, value),
            I16(value) => i16::encode(buf, value),
            U8(value) => u8::encode(buf, value),
            I8(value) => i8::encode(buf, value),
        }
    }

    /// Get the type of the value.
    pub fn ty(&self) -> Type {
        use self::Value::*;

        match *self {
            U128(..) => Type::U128,
            I128(..) => Type::I128,
            U64(..) => Type::U64,
            I64(..) => Type::I64,
            U32(..) => Type::U32,
            I32(..) => Type::I32,
            U16(..) => Type::U16,
            I16(..) => Type::I16,
            U8(..) => Type::U8,
            I8(..) => Type::I8,
        }
    }
}

impl str::FromStr for Value {
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut it = s.split(|c| c == 'i' || c == 'u');

        let base = it
            .next()
            .ok_or_else(|| failure::format_err!("missing numeric base"))?;

        let (s, ty) = match it.next() {
            Some(suffix) => {
                // NB: need to extract full suffix from original input.
                let e = s.len() - suffix.len() - 1;
                (base, str::parse::<Type>(&s[e..])?)
            }
            None => (s, Type::I32),
        };

        ty.parse(s)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Value::U128(value) => write!(fmt, "{}u128", value),
            Value::I128(value) => write!(fmt, "{}i128", value),
            Value::U64(value) => write!(fmt, "{}u64", value),
            Value::I64(value) => write!(fmt, "{}i64", value),
            Value::U32(value) => write!(fmt, "{}u32", value),
            Value::I32(value) => write!(fmt, "{}i32", value),
            Value::U16(value) => write!(fmt, "{}u16", value),
            Value::I16(value) => write!(fmt, "{}i16", value),
            Value::U8(value) => write!(fmt, "{}u8", value),
            Value::I8(value) => write!(fmt, "{}i8", value),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    U128,
    I128,
    U64,
    I64,
    U32,
    I32,
    U16,
    I16,
    U8,
    I8,
}

impl Type {
    /// Parse a string value of the type.
    pub fn parse(&self, input: &str) -> Result<Value, failure::Error> {
        use self::Type::*;

        let value = match *self {
            U128 => Value::U128(str::parse::<u128>(input)?),
            I128 => Value::I128(str::parse::<i128>(input)?),
            U64 => Value::U64(str::parse::<u64>(input)?),
            I64 => Value::I64(str::parse::<i64>(input)?),
            U32 => Value::U32(str::parse::<u32>(input)?),
            I32 => Value::I32(str::parse::<i32>(input)?),
            U16 => Value::U16(str::parse::<u16>(input)?),
            I16 => Value::I16(str::parse::<i16>(input)?),
            U8 => Value::U8(str::parse::<u8>(input)?),
            I8 => Value::I8(str::parse::<i8>(input)?),
        };

        Ok(value)
    }

    /// The size in-memory that a value has.
    pub fn size(&self) -> usize {
        use self::Type::*;

        match *self {
            U128 => mem::size_of::<u128>(),
            I128 => mem::size_of::<i128>(),
            U64 => mem::size_of::<u64>(),
            I64 => mem::size_of::<i64>(),
            U32 => mem::size_of::<u32>(),
            I32 => mem::size_of::<i32>(),
            U16 => mem::size_of::<u16>(),
            I16 => mem::size_of::<i16>(),
            U8 => mem::size_of::<u8>(),
            I8 => mem::size_of::<i8>(),
        }
    }

    /// Decode the given buffer into a value.
    pub fn decode(&self, buf: &[u8]) -> Value {
        use self::Type::*;
        use byteorder::{ByteOrder, LittleEndian};

        match *self {
            U128 => Value::U128(LittleEndian::read_u128(buf)),
            I128 => Value::I128(LittleEndian::read_i128(buf)),
            U64 => Value::U64(LittleEndian::read_u64(buf)),
            I64 => Value::I64(LittleEndian::read_i64(buf)),
            U32 => Value::U32(LittleEndian::read_u32(buf)),
            I32 => Value::I32(LittleEndian::read_i32(buf)),
            U16 => Value::U16(LittleEndian::read_u16(buf)),
            I16 => Value::I16(LittleEndian::read_i16(buf)),
            U8 => Value::U8(buf[0]),
            I8 => Value::I8(buf[0] as i8),
        }
    }
}

impl str::FromStr for Type {
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ty = match s {
            "u128" => Type::U128,
            "i128" => Type::I128,
            "u64" => Type::U64,
            "i64" => Type::I64,
            "u32" => Type::U32,
            "i32" => Type::I32,
            "u16" => Type::U16,
            "i16" => Type::I16,
            "u8" => Type::U8,
            "i8" => Type::I8,
            other => failure::bail!("bad type: {}", other),
        };

        Ok(ty)
    }
}

/// A bunch of read-only zeros used for comparison.
static ZEROS: &'static [u8] = &[
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
];

/// Special matching mode to speed up scanning.
#[derive(Debug, Clone)]
pub enum Special {
    Exact {
        buffer: Vec<u8>,
    },
    /// All bytes in the given range are expected to be zero.
    Zero,
    /// All bytes in the given range are expected to be non-zero.
    NonZero,
}

impl Special {
    /// Set up an exact match for the given value.
    fn exact<T>(value: T) -> Special
    where
        T: Encode,
    {
        let mut buffer = vec![0u8; mem::size_of::<T>()];
        T::encode(&mut buffer, value);

        Special::Exact { buffer }
    }

    pub fn test(&self, buf: &[u8]) -> Option<bool> {
        match self {
            Special::Zero => Some(buf == &ZEROS[..buf.len()]),
            Special::NonZero => {
                if buf == &ZEROS[..buf.len()] {
                    return Some(false);
                }

                None
            }
            Special::Exact { ref buffer } => Some(buf == buffer.as_slice()),
        }
    }
}

pub trait Predicate: Send + Sync + fmt::Debug {
    /// TODO: implement predicate simplification.

    /// The type value the predicate is scanning for.
    fn ty(&self) -> Option<Type>;

    /// If the predicate supports special (more efficient matching).
    fn special(&self) -> Option<Special> {
        None
    }

    /// Test the specified memory region.
    fn test(&self, value: &Value) -> bool;
}

macro_rules! numeric_match {
    ($expr_a:expr, $expr_b:expr, $a:ident, $b:ident, $test:expr) => {
        match ($expr_a, $expr_b) {
            (Value::U64($a), Value::U64($b)) => $test,
            (Value::U32($a), Value::U32($b)) => $test,
            (Value::I64($a), Value::I64($b)) => $test,
            (Value::I32($a), Value::I32($b)) => $test,
            _ => false,
        }
    };
}

/// Only match values which are exactly equal.
#[derive(Debug, Clone)]
pub struct Eq(pub Value);

impl Predicate for Eq {
    fn ty(&self) -> Option<Type> {
        Some(self.0.ty())
    }

    fn special(&self) -> Option<Special> {
        macro_rules! specials {
            ($(($field:ident, $ty:ident),)*) => {
                match self.0 {
                $(Value::$field(v) => match v {
                    0 => Special::Zero,
                    o => Special::exact(o),
                },)*
                }
            };
        }

        Some(specials! {
            (U128, u128),
            (I128, i128),
            (U64, u64),
            (I64, i64),
            (U32, u32),
            (I32, i32),
            (U16, u16),
            (I16, i16),
            (U8, u8),
            (I8, i8),
        })
    }

    fn test(&self, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a == b)
    }
}

/// Only match values which are greater than the specified value.
#[derive(Debug, Clone)]
pub struct Gt(pub Value);

impl Predicate for Gt {
    fn ty(&self) -> Option<Type> {
        Some(self.0.ty())
    }

    fn special(&self) -> Option<Special> {
        let s = match self.0 {
            Value::U128(..) | Value::U64(..) | Value::U32(..) | Value::U16(..) | Value::U8(..) => {
                Special::NonZero
            }
            Value::I128(v) if v > 0 => Special::NonZero,
            Value::I64(v) if v > 0 => Special::NonZero,
            Value::I32(v) if v > 0 => Special::NonZero,
            Value::I16(v) if v > 0 => Special::NonZero,
            Value::I8(v) if v > 0 => Special::NonZero,
            _ => return None,
        };

        Some(s)
    }

    fn test(&self, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a > b)
    }
}

/// Only match values which are greater than or equal to the specified value.
#[derive(Debug, Clone)]
pub struct Gte(pub Value);

impl Predicate for Gte {
    fn ty(&self) -> Option<Type> {
        Some(self.0.ty())
    }

    fn special(&self) -> Option<Special> {
        let s = match self.0 {
            Value::U128(v) if v > 0 => Special::NonZero,
            Value::U64(v) if v > 0 => Special::NonZero,
            Value::U32(v) if v > 0 => Special::NonZero,
            Value::U16(v) if v > 0 => Special::NonZero,
            Value::U8(v) if v > 0 => Special::NonZero,
            Value::I128(v) if v > 0 => Special::NonZero,
            Value::I64(v) if v > 0 => Special::NonZero,
            Value::I32(v) if v > 0 => Special::NonZero,
            Value::I16(v) if v > 0 => Special::NonZero,
            Value::I8(v) if v > 0 => Special::NonZero,
            _ => return None,
        };

        Some(s)
    }

    fn test(&self, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a >= b)
    }
}

/// Only match values which are less than the specified value.
#[derive(Debug, Clone)]
pub struct Lt(pub Value);

impl Predicate for Lt {
    fn ty(&self) -> Option<Type> {
        Some(self.0.ty())
    }

    fn special(&self) -> Option<Special> {
        let s = match self.0 {
            Value::U128(v) if v == 1 => Special::Zero,
            Value::U64(v) if v == 1 => Special::Zero,
            Value::U32(v) if v == 1 => Special::Zero,
            Value::U16(v) if v == 1 => Special::Zero,
            Value::U8(v) if v == 1 => Special::Zero,
            _ => return None,
        };

        Some(s)
    }

    fn test(&self, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a < b)
    }
}

/// Only match values which are less than or equal to the specified value.
#[derive(Debug, Clone)]
pub struct Lte(pub Value);

impl Predicate for Lte {
    fn ty(&self) -> Option<Type> {
        Some(self.0.ty())
    }

    fn special(&self) -> Option<Special> {
        let s = match self.0 {
            Value::U128(v) if v == 0 => Special::Zero,
            Value::U64(v) if v == 0 => Special::NonZero,
            Value::U32(v) if v == 0 => Special::NonZero,
            Value::U16(v) if v == 0 => Special::NonZero,
            Value::U8(v) if v == 0 => Special::NonZero,
            _ => return None,
        };

        Some(s)
    }

    fn test(&self, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a <= b)
    }
}

/// Only matches when all nested predicates match.
#[derive(Debug)]
pub struct All(Option<Type>, Vec<Box<Predicate>>);

impl All {
    pub fn new(
        predicates: impl IntoIterator<Item = Box<Predicate>>,
    ) -> Result<All, failure::Error> {
        collection(predicates, All)
    }
}

impl Predicate for All {
    fn ty(&self) -> Option<Type> {
        self.0.clone()
    }

    fn test(&self, value: &Value) -> bool {
        self.1.iter().all(|p| p.test(value))
    }
}

/// Only matches when any nested predicate match.
#[derive(Debug)]
pub struct Any(Option<Type>, Vec<Box<Predicate>>);

impl Any {
    pub fn new(
        predicates: impl IntoIterator<Item = Box<Predicate>>,
    ) -> Result<Any, failure::Error> {
        collection(predicates, Any)
    }
}

impl Predicate for Any {
    fn ty(&self) -> Option<Type> {
        self.0.clone()
    }

    fn test(&self, value: &Value) -> bool {
        self.1.iter().any(|p| p.test(value))
    }
}

pub trait Encode: Sized {
    /// Encode the value into the buffer.
    fn encode(buffer: &mut [u8], value: Self);
}

impl Encode for u128 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u128(buffer, value);
    }
}

impl Encode for i128 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i128(buffer, value);
    }
}

impl Encode for u64 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u64(buffer, value);
    }
}

impl Encode for i64 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i64(buffer, value);
    }
}

impl Encode for u32 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u32(buffer, value);
    }
}

impl Encode for i32 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i32(buffer, value);
    }
}

impl Encode for u16 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u16(buffer, value);
    }
}

impl Encode for i16 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i16(buffer, value);
    }
}

impl Encode for u8 {
    fn encode(buffer: &mut [u8], value: Self) {
        buffer[0] = value;
    }
}

impl Encode for i8 {
    fn encode(buffer: &mut [u8], value: Self) {
        buffer[0] = value as u8;
    }
}

#[cfg(test)]
mod tests {
    use super::{Eq, Gt, Lt, Predicate, Type, Value};

    #[test]
    fn test_exact() {
        let a = Value::U32(0xffffffffu32);
        let b = Type::U32.decode(&[0xff, 0xff, 0xff, 0xff]);
        assert!(Eq(a.clone()).test(&b));
        assert!(!Lt(a.clone()).test(&b));
        assert!(!Gt(a.clone()).test(&b));
    }
}

/// Helper function to build a collection predicate.
///
/// Checks that every child predicate have the same type.
fn collection<T>(
    predicates: impl IntoIterator<Item = Box<Predicate>>,
    constructor: impl FnOnce(Option<Type>, Vec<Box<Predicate>>) -> T,
) -> Result<T, failure::Error> {
    use std::collections::HashSet;

    let mut all = Vec::new();

    for p in predicates {
        all.push(p);
    }

    let types = all.iter().flat_map(|p| p.ty()).collect::<HashSet<_>>();

    if types.len() > 1 {
        failure::bail!(
            "predicates of different types are not supported: {:?}",
            types
        );
    }

    Ok(constructor(types.into_iter().next(), all))
}
