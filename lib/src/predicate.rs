use crate::{
    scan::{Special, Type, Value},
    scanner,
};
use std::fmt;

mod ast;
mod lexer;
lalrpop_util::lalrpop_mod!(parser, "/predicate/parser.rs");

pub trait Predicate: Send + Sync + fmt::Debug {
    /// TODO: implement predicate simplification.

    /// The type value the predicate is scanning for.
    fn ty(&self) -> Option<Type> {
        None
    }

    /// If the predicate supports special (more efficient matching).
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
#[derive(Debug, Clone)]
pub struct Not<P>(pub P);

impl<P> Predicate for Not<P>
where
    P: Predicate,
{
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

/// Match values which are smaller than before.
#[derive(Debug, Clone)]
pub struct Dec;

impl Predicate for Dec {
    fn test(&self, last: Option<&scanner::ScanResult>, value: &Value) -> bool {
        match last {
            Some(last) => Lt(last.value).test(None, value),
            None => false,
        }
    }
}

/// Match values which are larger than before.
#[derive(Debug, Clone)]
pub struct Inc;

impl Predicate for Inc {
    fn test(&self, last: Option<&scanner::ScanResult>, value: &Value) -> bool {
        match last {
            Some(last) => Gt(last.value).test(None, value),
            None => false,
        }
    }
}

/// Match values which are larger than before.
#[derive(Debug, Clone)]
pub struct Changed;

impl Predicate for Changed {
    fn test(&self, last: Option<&scanner::ScanResult>, value: &Value) -> bool {
        match last {
            Some(last) => Not(Eq(last.value)).test(None, value),
            None => false,
        }
    }
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

    fn test(&self, _: Option<&scanner::ScanResult>, value: &Value) -> bool {
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

/// Only match values which are greater than or equal to the specified value.
#[derive(Debug, Clone)]
pub struct Gte(pub Value);

impl Predicate for Gte {
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

    fn test(&self, _: Option<&scanner::ScanResult>, value: &Value) -> bool {
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

    fn test(&self, last: Option<&scanner::ScanResult>, value: &Value) -> bool {
        self.1.iter().all(|p| p.test(last, value))
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

    fn test(&self, last: Option<&scanner::ScanResult>, value: &Value) -> bool {
        self.1.iter().any(|p| p.test(last, value))
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
