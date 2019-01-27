use crate::{
    scan::{Special, Type, Value},
    scanner,
};
use std::fmt;

mod ast;
mod lexer;
lalrpop_util::lalrpop_mod!(parser, "/filter/parser.rs");

/// Parse a a string into a filter.
pub fn parse(input: &str) -> Result<Box<Filter>, failure::Error> {
    parser::OrParser::new()
        .parse(lexer::Lexer::new(input))?
        .into_filter()
}

pub trait Filter: Send + Sync + fmt::Debug + fmt::Display {
    /// The type value the filter is scanning for.
    fn ty(&self) -> Option<Type> {
        None
    }

    /// If the filter supports special (more efficient matching).
    fn special(&self) -> Option<Special> {
        None
    }

    /// Test the specified memory region.
    fn test(&self, _: Option<&scanner::ScanResult>, _: &Value) -> bool;
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
#[derive(Debug)]
pub struct Not(Box<dyn Filter>);

impl Filter for Not {
    fn ty(&self) -> Option<Type> {
        self.0.ty()
    }

    fn special(&self) -> Option<Special> {
        self.0.special().map(|s| s.invert())
    }

    fn test(&self, last: Option<&scanner::ScanResult>, value: &Value) -> bool {
        !self.0.test(last, value)
    }
}

impl fmt::Display for Not {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "!{}", self.0)
    }
}

/// Match values which are smaller than before.
#[derive(Debug, Clone)]
pub struct Dec;

impl Filter for Dec {
    fn test(&self, last: Option<&scanner::ScanResult>, value: &Value) -> bool {
        match last {
            Some(last) => Lt(last.value).test(None, value),
            None => false,
        }
    }
}

impl fmt::Display for Dec {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "dec($value)")
    }
}

/// Match values which are larger than before.
#[derive(Debug, Clone)]
pub struct Inc;

impl Filter for Inc {
    fn test(&self, last: Option<&scanner::ScanResult>, value: &Value) -> bool {
        match last {
            Some(last) => Gt(last.value).test(None, value),
            None => false,
        }
    }
}

impl fmt::Display for Inc {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "inc($value)")
    }
}

/// Match values which are larger than before.
#[derive(Debug, Clone)]
pub struct Changed;

impl Filter for Changed {
    fn test(&self, last: Option<&scanner::ScanResult>, value: &Value) -> bool {
        match last {
            Some(last) => !Eq(last.value).test(None, value),
            None => false,
        }
    }
}

impl fmt::Display for Changed {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "changed($value)")
    }
}

/// Only match values which are exactly equal.
#[derive(Debug, Clone)]
pub struct Eq(pub Value);

impl Filter for Eq {
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

    fn test(&self, _: Option<&scanner::ScanResult>, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a == b)
    }
}

impl fmt::Display for Eq {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "$value == {}", self.0)
    }
}

/// Only match values which are exactly equal.
#[derive(Debug, Clone)]
pub struct Neq(pub Value);

impl Filter for Neq {
    fn ty(&self) -> Option<Type> {
        Some(self.0.ty())
    }

    fn special(&self) -> Option<Special> {
        macro_rules! specials {
            ($(($field:ident, $ty:ident),)*) => {
                match self.0 {
                $(Value::$field(v) => match v {
                    0 => Special::NotZero,
                    o => Special::not_exact(o),
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

    fn test(&self, _: Option<&scanner::ScanResult>, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a != b)
    }
}

impl fmt::Display for Neq {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "$value != {}", self.0)
    }
}

/// Only match values which are greater than the specified value.
#[derive(Debug, Clone)]
pub struct Gt(pub Value);

impl Filter for Gt {
    fn ty(&self) -> Option<Type> {
        Some(self.0.ty())
    }

    fn special(&self) -> Option<Special> {
        let s = match self.0 {
            Value::U128(..) | Value::U64(..) | Value::U32(..) | Value::U16(..) | Value::U8(..) => {
                Special::NotZero
            }
            Value::I128(v) if v > 0 => Special::NotZero,
            Value::I64(v) if v > 0 => Special::NotZero,
            Value::I32(v) if v > 0 => Special::NotZero,
            Value::I16(v) if v > 0 => Special::NotZero,
            Value::I8(v) if v > 0 => Special::NotZero,
            _ => return None,
        };

        Some(s)
    }

    fn test(&self, _: Option<&scanner::ScanResult>, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a > b)
    }
}

impl fmt::Display for Gt {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "$value > {}", self.0)
    }
}

/// Only match values which are greater than or equal to the specified value.
#[derive(Debug, Clone)]
pub struct Gte(pub Value);

impl Filter for Gte {
    fn ty(&self) -> Option<Type> {
        Some(self.0.ty())
    }

    fn special(&self) -> Option<Special> {
        let s = match self.0 {
            Value::U128(v) if v > 0 => Special::NotZero,
            Value::U64(v) if v > 0 => Special::NotZero,
            Value::U32(v) if v > 0 => Special::NotZero,
            Value::U16(v) if v > 0 => Special::NotZero,
            Value::U8(v) if v > 0 => Special::NotZero,
            Value::I128(v) if v > 0 => Special::NotZero,
            Value::I64(v) if v > 0 => Special::NotZero,
            Value::I32(v) if v > 0 => Special::NotZero,
            Value::I16(v) if v > 0 => Special::NotZero,
            Value::I8(v) if v > 0 => Special::NotZero,
            _ => return None,
        };

        Some(s)
    }

    fn test(&self, _: Option<&scanner::ScanResult>, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a >= b)
    }
}

impl fmt::Display for Gte {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "$value >= {}", self.0)
    }
}

/// Only match values which are less than the specified value.
#[derive(Debug, Clone)]
pub struct Lt(pub Value);

impl Filter for Lt {
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

    fn test(&self, _: Option<&scanner::ScanResult>, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a < b)
    }
}

impl fmt::Display for Lt {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "$value < {}", self.0)
    }
}

/// Only match values which are less than or equal to the specified value.
#[derive(Debug, Clone)]
pub struct Lte(pub Value);

impl Filter for Lte {
    fn ty(&self) -> Option<Type> {
        Some(self.0.ty())
    }

    fn special(&self) -> Option<Special> {
        let s = match self.0 {
            Value::U128(v) if v == 0 => Special::Zero,
            Value::U64(v) if v == 0 => Special::NotZero,
            Value::U32(v) if v == 0 => Special::NotZero,
            Value::U16(v) if v == 0 => Special::NotZero,
            Value::U8(v) if v == 0 => Special::NotZero,
            _ => return None,
        };

        Some(s)
    }

    fn test(&self, _: Option<&scanner::ScanResult>, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a <= b)
    }
}

impl fmt::Display for Lte {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "$value <= {}", self.0)
    }
}

/// Only matches when all nested filters match.
#[derive(Debug)]
pub struct All(Option<Type>, Vec<Box<Filter>>);

impl All {
    pub fn new(filters: impl IntoIterator<Item = Box<Filter>>) -> Result<All, failure::Error> {
        collection(filters, All)
    }
}

impl fmt::Display for All {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut it = self.1.iter().peekable();

        if let Some(p) = it.next() {
            if it.peek().is_some() {
                write!(fmt, "{} and ", p)?;
            } else {
                write!(fmt, "{}", p)?;
            }
        }

        Ok(())
    }
}

impl Filter for All {
    fn ty(&self) -> Option<Type> {
        self.0.clone()
    }

    fn test(&self, last: Option<&scanner::ScanResult>, value: &Value) -> bool {
        self.1.iter().all(|p| p.test(last, value))
    }
}

/// Only matches when any nested filter match.
#[derive(Debug)]
pub struct Any(Option<Type>, Vec<Box<Filter>>);

impl Any {
    pub fn new(filters: impl IntoIterator<Item = Box<Filter>>) -> Result<Any, failure::Error> {
        collection(filters, Any)
    }
}

impl Filter for Any {
    fn ty(&self) -> Option<Type> {
        self.0.clone()
    }

    fn test(&self, last: Option<&scanner::ScanResult>, value: &Value) -> bool {
        self.1.iter().any(|p| p.test(last, value))
    }
}

impl fmt::Display for Any {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut it = self.1.iter().peekable();

        if let Some(p) = it.next() {
            if it.peek().is_some() {
                write!(fmt, "{} or ", p)?;
            } else {
                write!(fmt, "{}", p)?;
            }
        }

        Ok(())
    }
}

/// Helper function to build a collection filter.
///
/// Checks that every child filter have the same type.
fn collection<T>(
    filters: impl IntoIterator<Item = Box<Filter>>,
    constructor: impl FnOnce(Option<Type>, Vec<Box<Filter>>) -> T,
) -> Result<T, failure::Error> {
    use std::collections::HashSet;

    let mut all = Vec::new();

    for p in filters {
        all.push(p);
    }

    let types = all.iter().flat_map(|p| p.ty()).collect::<HashSet<_>>();

    if types.len() > 1 {
        failure::bail!("filters of different types are not supported: {:?}", types);
    }

    Ok(constructor(types.into_iter().next(), all))
}

#[cfg(test)]
mod tests {
    use super::{lexer, parser};
    use std::error;

    #[test]
    fn basic_parsing() -> Result<(), Box<error::Error>> {
        let mut parser = parser::OrParser::new();
        let a = parser.parse(lexer::Lexer::new("$value == 1"))?;
        let b = parser.parse(lexer::Lexer::new("$value == 1 and $value == 2"))?;
        let c = parser.parse(lexer::Lexer::new(
            "$value == 1 and $value == 2 or $value == 3",
        ))?;

        dbg!(a);
        dbg!(b);
        dbg!(c);

        Ok(())
    }
}
