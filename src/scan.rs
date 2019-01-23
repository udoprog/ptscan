//! Predicates used for matching against memory.

use std::{fmt, mem};

/// A single dynamic literal value.
#[derive(Debug, Clone)]
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
        use byteorder::{ByteOrder, LittleEndian};

        match *self {
            U128(value) => LittleEndian::write_u128(buf, value),
            I128(value) => LittleEndian::write_i128(buf, value),
            U64(value) => LittleEndian::write_u64(buf, value),
            I64(value) => LittleEndian::write_i64(buf, value),
            U32(value) => LittleEndian::write_u32(buf, value),
            I32(value) => LittleEndian::write_i32(buf, value),
            U16(value) => LittleEndian::write_u16(buf, value),
            I16(value) => LittleEndian::write_i16(buf, value),
            U8(value) => buf[0] = value,
            I8(value) => buf[0] = value as u8,
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

impl Value {}

pub trait Predicate: Sync + Send + fmt::Debug {
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
    fn test(&self, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a == b)
    }
}

/// Only match values which are greater than the specified value.
#[derive(Debug, Clone)]
pub struct Gt(pub Value);

impl Predicate for Gt {
    fn test(&self, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a > b)
    }
}

/// Only match values which are greater than or equal to the specified value.
#[derive(Debug, Clone)]
pub struct Gte(pub Value);

impl Predicate for Gte {
    fn test(&self, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a >= b)
    }
}

/// Only match values which are less than the specified value.
#[derive(Debug, Clone)]
pub struct Lt(pub Value);

impl Predicate for Lt {
    fn test(&self, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a < b)
    }
}

/// Only match values which are less than or equal to the specified value.
#[derive(Debug, Clone)]
pub struct Lte(pub Value);

impl Predicate for Lte {
    fn test(&self, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a <= b)
    }
}

/// Only matches when all nested predicates match.
#[derive(Debug)]
pub struct All(pub Vec<Box<Predicate>>);

impl Predicate for All {
    fn test(&self, value: &Value) -> bool {
        self.0.iter().all(|p| p.test(value))
    }
}

/// Only matches when any nested predicate match.
#[derive(Debug)]
pub struct Any(pub Vec<Box<Predicate>>);

impl Predicate for Any {
    fn test(&self, value: &Value) -> bool {
        self.0.iter().any(|p| p.test(value))
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
