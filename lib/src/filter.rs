use crate::{special::Special, Type, Value};
use std::fmt;

pub mod ast;
mod lexer;
lalrpop_util::lalrpop_mod!(parser, "/filter/parser.rs");

/// Parse a a string into a filter.
pub fn parse(input: &str, ty: Type) -> anyhow::Result<Filter> {
    let matcher = parser::OrParser::new()
        .parse(lexer::Lexer::new(input))?
        .into_matcher(ty)?;

    let special = matcher.special();

    Ok(Filter {
        ty,
        matcher,
        special,
    })
}

#[derive(Debug)]
pub struct Filter {
    pub ty: Type,
    pub matcher: Box<dyn Matcher>,
    special: Option<Special>,
}

impl Filter {
    /// Test the given bytes against this filter.
    pub fn test(&self, last: &Value, bytes: &[u8]) -> Option<Value> {
        match self.special.as_ref().and_then(|s| s.test(bytes)) {
            // A special, more efficient kind of matching is available.
            Some(true) => self.ty.decode(bytes).ok(),
            // special match is negative.
            Some(false) => None,
            None => {
                let value = self.ty.decode(bytes).ok()?;

                if self.matcher.test(last, &value) {
                    Some(value)
                } else {
                    None
                }
            }
        }
    }

    /// The size required to match this filter.
    pub fn size(&self) -> Option<usize> {
        self.matcher.size().or_else(|| self.ty.size())
    }

    /// Test if filter is aligned by default.
    pub fn is_default_aligned(&self) -> bool {
        self.ty.is_default_aligned()
    }
}

pub trait Matcher: Send + Sync + fmt::Debug + fmt::Display {
    /// If the filter supports special (more efficient matching).
    fn special(&self) -> Option<Special> {
        None
    }

    /// Test the specified memory region.
    fn test(&self, _: &Value, _: &Value) -> bool;

    /// The size of buffer required to drive this matcher.
    fn size(&self) -> Option<usize>;

    /// Indicates if the matched for value can be aligned.
    fn is_default_aligned(&self) -> Option<bool>;
}

macro_rules! numeric_match {
    ($expr_a:expr, $expr_b:expr, $a:ident, $b:ident, $test:expr) => {
        match ($expr_a, $expr_b) {
            (Value::U128($a), Value::U128($b)) => $test,
            (Value::I128($a), Value::I128($b)) => $test,
            (Value::U64($a), Value::U64($b)) => $test,
            (Value::I64($a), Value::I64($b)) => $test,
            (Value::U32($a), Value::U32($b)) => $test,
            (Value::I32($a), Value::I32($b)) => $test,
            (Value::U8($a), Value::U8($b)) => $test,
            (Value::I8($a), Value::I8($b)) => $test,
            (Value::String($a), Value::String($b)) => $test,
            _ => false,
        }
    };
}

/// Only match values which are exactly equal.
#[derive(Debug)]
pub struct Not(Box<dyn Matcher>);

impl Matcher for Not {
    fn special(&self) -> Option<Special> {
        self.0.special().map(|s| s.invert())
    }

    fn test(&self, last: &Value, value: &Value) -> bool {
        !self.0.test(last, value)
    }

    fn size(&self) -> Option<usize> {
        self.0.size()
    }

    fn is_default_aligned(&self) -> Option<bool> {
        self.0.is_default_aligned()
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

impl Matcher for Dec {
    fn test(&self, last: &Value, value: &Value) -> bool {
        Lt(last.clone()).test(&Value::None, value)
    }

    fn size(&self) -> Option<usize> {
        None
    }

    fn is_default_aligned(&self) -> Option<bool> {
        None
    }
}

impl fmt::Display for Dec {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "dec")
    }
}

/// Match values which are larger than before.
#[derive(Debug, Clone)]
pub struct Inc;

impl Matcher for Inc {
    fn test(&self, last: &Value, value: &Value) -> bool {
        Gt(last.clone()).test(&Value::None, value)
    }

    fn size(&self) -> Option<usize> {
        None
    }

    fn is_default_aligned(&self) -> Option<bool> {
        None
    }
}

impl fmt::Display for Inc {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "inc")
    }
}

/// Match a value that has changed from the last scan.
#[derive(Debug, Clone)]
pub struct Changed;

impl Matcher for Changed {
    fn test(&self, last: &Value, value: &Value) -> bool {
        !Eq(last.clone()).test(&Value::None, value)
    }

    fn size(&self) -> Option<usize> {
        None
    }

    fn is_default_aligned(&self) -> Option<bool> {
        None
    }
}

impl fmt::Display for Changed {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "changed")
    }
}

/// Match a value that is the same as last scan.
#[derive(Debug, Clone)]
pub struct Same;

impl Matcher for Same {
    fn test(&self, last: &Value, value: &Value) -> bool {
        Eq(last.clone()).test(&Value::None, value)
    }

    fn size(&self) -> Option<usize> {
        None
    }

    fn is_default_aligned(&self) -> Option<bool> {
        None
    }
}

impl fmt::Display for Same {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "same")
    }
}

/// Only match values which are exactly equal.
#[derive(Debug, Clone)]
pub struct Eq(pub Value);

impl Matcher for Eq {
    fn special(&self) -> Option<Special> {
        macro_rules! specials {
            ($(($field:ident, $ty:ident),)*) => {
                match self.0 {
                Value::None => return None,
                Value::String(ref string) => Special::exact_string(string.as_str()),
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

    fn test(&self, _: &Value, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a == b)
    }

    fn size(&self) -> Option<usize> {
        Some(self.0.size())
    }

    fn is_default_aligned(&self) -> Option<bool> {
        Some(self.0.is_default_aligned())
    }
}

impl fmt::Display for Eq {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "value == {}", self.0)
    }
}

/// Only match values which are exactly equal.
#[derive(Debug, Clone)]
pub struct Neq(pub Value);

impl Matcher for Neq {
    fn special(&self) -> Option<Special> {
        macro_rules! specials {
            ($(($field:ident, $ty:ident),)*) => {
                match self.0 {
                Value::None => return None,
                Value::String(ref string) => Special::not_exact(string.as_str()),
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

    fn test(&self, _: &Value, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a != b)
    }

    fn size(&self) -> Option<usize> {
        Some(self.0.size())
    }

    fn is_default_aligned(&self) -> Option<bool> {
        Some(self.0.is_default_aligned())
    }
}

impl fmt::Display for Neq {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "value != {}", self.0)
    }
}

/// Only match values which are greater than the specified value.
#[derive(Debug, Clone)]
pub struct Gt(pub Value);

impl Matcher for Gt {
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

    fn test(&self, _: &Value, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a > b)
    }

    fn size(&self) -> Option<usize> {
        Some(self.0.size())
    }

    fn is_default_aligned(&self) -> Option<bool> {
        Some(self.0.is_default_aligned())
    }
}

impl fmt::Display for Gt {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "value > {}", self.0)
    }
}

/// Only match values which are greater than or equal to the specified value.
#[derive(Debug, Clone)]
pub struct Gte(pub Value);

impl Matcher for Gte {
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

    fn test(&self, _: &Value, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a >= b)
    }

    fn size(&self) -> Option<usize> {
        Some(self.0.size())
    }

    fn is_default_aligned(&self) -> Option<bool> {
        Some(self.0.is_default_aligned())
    }
}

impl fmt::Display for Gte {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "value >= {}", self.0)
    }
}

/// Only match values which are less than the specified value.
#[derive(Debug, Clone)]
pub struct Lt(pub Value);

impl Matcher for Lt {
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

    fn test(&self, _: &Value, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a < b)
    }

    fn size(&self) -> Option<usize> {
        Some(self.0.size())
    }

    fn is_default_aligned(&self) -> Option<bool> {
        Some(self.0.is_default_aligned())
    }
}

impl fmt::Display for Lt {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "value < {}", self.0)
    }
}

/// Only match values which are less than or equal to the specified value.
#[derive(Debug, Clone)]
pub struct Lte(pub Value);

impl Matcher for Lte {
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

    fn test(&self, _: &Value, value: &Value) -> bool {
        numeric_match!(value, &self.0, a, b, a <= b)
    }

    fn size(&self) -> Option<usize> {
        Some(self.0.size())
    }

    fn is_default_aligned(&self) -> Option<bool> {
        Some(self.0.is_default_aligned())
    }
}

impl fmt::Display for Lte {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "value <= {}", self.0)
    }
}

/// Only matches when all nested filters match.
#[derive(Debug)]
pub struct All(Vec<Box<dyn Matcher>>);

impl All {
    pub fn new(filters: impl IntoIterator<Item = Box<dyn Matcher>>) -> Self {
        All(filters.into_iter().collect())
    }
}

impl fmt::Display for All {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut it = self.0.iter().peekable();

        while let Some(p) = it.next() {
            fmt::Display::fmt(p, fmt)?;

            if it.peek().is_some() {
                write!(fmt, " and ")?;
            }
        }

        Ok(())
    }
}

impl Matcher for All {
    fn test(&self, last: &Value, value: &Value) -> bool {
        self.0.iter().all(|p| p.test(last, value))
    }

    fn size(&self) -> Option<usize> {
        self.0.iter().flat_map(|w| w.size()).max()
    }

    fn is_default_aligned(&self) -> Option<bool> {
        let mut it = self.0.iter().flat_map(|w| w.is_default_aligned());

        match it.next() {
            Some(aligned) => Some(aligned && it.all(|aligned| aligned)),
            None => None,
        }
    }
}

/// Only matches when any nested filter match.
#[derive(Debug)]
pub struct Any(Vec<Box<dyn Matcher>>);

impl Any {
    pub fn new(filters: impl IntoIterator<Item = Box<dyn Matcher>>) -> Self {
        Any(filters.into_iter().collect())
    }
}

impl Matcher for Any {
    fn test(&self, last: &Value, value: &Value) -> bool {
        self.0.iter().any(|p| p.test(last, value))
    }

    fn size(&self) -> Option<usize> {
        self.0.iter().flat_map(|w| w.size()).max()
    }

    fn is_default_aligned(&self) -> Option<bool> {
        let mut it = self.0.iter().flat_map(|w| w.is_default_aligned());

        match it.next() {
            Some(aligned) => Some(aligned && it.all(|aligned| aligned)),
            None => None,
        }
    }
}

impl fmt::Display for Any {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut it = self.0.iter().peekable();

        while let Some(p) = it.next() {
            write!(fmt, "{}", p)?;

            if it.peek().is_some() {
                write!(fmt, " or ")?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{lexer, parser};
    use std::error;

    #[test]
    fn basic_parsing() -> Result<(), Box<dyn error::Error>> {
        let parser = parser::OrParser::new();
        let a = parser.parse(lexer::Lexer::new("value == 1"))?;
        let b = parser.parse(lexer::Lexer::new("value == 1 and value == 2"))?;
        let c = parser.parse(lexer::Lexer::new("value == 1 and value == 2 or value == 3"))?;

        dbg!(a);
        dbg!(b);
        dbg!(c);

        Ok(())
    }
}
