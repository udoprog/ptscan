use self::ast::{Op, ValueExpr, ValueTrait};
use crate::{
    process::{AddressProxy, Process},
    process_handle::PointerMatcher,
    value::{self, Value},
    Type,
};
use num_bigint::Sign;
use std::fmt;

pub mod ast;
mod lexer;
lalrpop_util::lalrpop_mod!(parser, "/filter/parser.rs");

/// Parse a a string into a filter.
pub fn parse(input: &str, ty: Type, process: Option<&Process>) -> anyhow::Result<Filter> {
    let matcher = parser::OrParser::new()
        .parse(lexer::Lexer::new(input))?
        .into_matcher(ty, process)?;

    Ok(Filter { ty, matcher })
}

pub enum Special {
    Bytes(Vec<u8>),
    NonZero,
}

#[derive(Debug)]
pub struct Filter {
    pub ty: Type,
    pub matcher: Matcher,
}

impl Filter {
    /// Test the given bytes against this filter.
    pub fn test(&self, last: &Value, proxy: AddressProxy<'_>) -> anyhow::Result<bool> {
        Ok(self.matcher.test(self.ty, last, proxy)?)
    }

    pub fn special(&self) -> anyhow::Result<Option<Special>> {
        self.matcher.special(self.ty)
    }
}

#[derive(Debug, Clone)]
pub enum Matcher {
    Changed(Changed),
    Same(Same),
    Binary(Binary),
    All(All),
    Any(Any),
    Inc(Inc),
    Dec(Dec),
    PointerMatcher(PointerMatcher),
}

impl Matcher {
    /// Test the specified memory region.
    fn test(&self, ty: Type, last: &Value, proxy: AddressProxy<'_>) -> anyhow::Result<bool> {
        match self {
            Self::Changed(m) => m.test(ty, last, proxy),
            Self::Same(m) => m.test(ty, last, proxy),
            Self::Binary(m) => m.test(ty, last, proxy),
            Self::All(m) => m.test(ty, last, proxy),
            Self::Any(m) => m.test(ty, last, proxy),
            Self::Inc(m) => m.test(ty, last, proxy),
            Self::Dec(m) => m.test(ty, last, proxy),
            Self::PointerMatcher(m) => m.test(ty, last, proxy),
        }
    }

    /// Construct a special matcher.
    fn special(&self, ty: Type) -> anyhow::Result<Option<Special>> {
        match self {
            Self::PointerMatcher(..) => Ok(Some(Special::NonZero)),
            Self::Binary(m) => m.special(ty),
            _ => Ok(None),
        }
    }
}

impl fmt::Display for Matcher {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Changed(m) => m.fmt(fmt),
            Self::Same(m) => m.fmt(fmt),
            Self::Binary(m) => m.fmt(fmt),
            Self::All(m) => m.fmt(fmt),
            Self::Any(m) => m.fmt(fmt),
            Self::Inc(m) => m.fmt(fmt),
            Self::Dec(m) => m.fmt(fmt),
            Self::PointerMatcher(m) => m.fmt(fmt),
        }
    }
}

macro_rules! value_match {
    ($expr_a:expr, $expr_b:expr, $a:ident $op:tt $b:ident) => {
        match ($expr_a, $expr_b) {
            (Value::U128($a), Value::U128($b)) => $a $op $b,
            (Value::I128($a), Value::I128($b)) => $a $op $b,
            (Value::U64($a), Value::U64($b)) => $a $op $b,
            (Value::I64($a), Value::I64($b)) => $a $op $b,
            (Value::U32($a), Value::U32($b)) => $a $op $b,
            (Value::I32($a), Value::I32($b)) => $a $op $b,
            (Value::U8($a), Value::U8($b)) => $a $op $b,
            (Value::I8($a), Value::I8($b)) => $a $op $b,
            (Value::String($a), Value::String($b)) => {
                let len = usize::min($a.len(), $b.len());
                let $a = &$a[..len];
                let $b = &$b[..len];
                $a $op $b
            },
            _ => false,
        }
    };
}

/// Match values which are smaller than before.
#[derive(Debug, Clone)]
pub struct Dec;

impl Dec {
    fn test(&self, _: Type, _: &Value, _: AddressProxy<'_>) -> anyhow::Result<bool> {
        Ok(false)
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

impl Inc {
    fn test(&self, _: Type, _: &Value, _: AddressProxy<'_>) -> anyhow::Result<bool> {
        Ok(false)
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

impl Changed {
    fn test(&self, _: Type, _: &Value, _: AddressProxy<'_>) -> anyhow::Result<bool> {
        Ok(false)
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

impl Same {
    fn test(&self, _: Type, _: &Value, _: AddressProxy<'_>) -> anyhow::Result<bool> {
        Ok(false)
    }
}

impl fmt::Display for Same {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "same")
    }
}

/// Only match values which are exactly equal.
#[derive(Debug, Clone)]
pub struct Binary(Op, ValueExpr, ValueExpr);

impl Binary {
    fn test(&self, ty: Type, _: &Value, proxy: AddressProxy<'_>) -> anyhow::Result<bool> {
        let Binary(op, lhs, rhs) = self;

        let lhs = match lhs.eval(ty, proxy)? {
            Value::None => return Ok(false),
            lhs => lhs,
        };

        let rhs = match rhs.eval(ty, proxy)? {
            Value::None => return Ok(false),
            rhs => rhs,
        };

        Ok(match op {
            Op::Eq => value_match!(lhs, rhs, a == b),
            Op::Neq => value_match!(lhs, rhs, a != b),
            Op::Lt => value_match!(lhs, rhs, a < b),
            Op::Lte => value_match!(lhs, rhs, a <= b),
            Op::Gt => value_match!(lhs, rhs, a > b),
            Op::Gte => value_match!(lhs, rhs, a >= b),
        })
    }

    fn special(&self, ty: Type) -> anyhow::Result<Option<Special>> {
        use self::Sign::*;
        use self::ValueExpr::*;
        use self::ValueTrait::*;

        let Binary(op, lhs, rhs) = self;

        let bytes = match (op, lhs, rhs) {
            (Op::Eq, String(string), Value) | (Op::Eq, Value, String(string)) => {
                Some(value::Value::String(string.as_bytes().to_vec()))
            }
            (Op::Eq, Number(number), Value) | (Op::Eq, Value, Number(number)) => {
                Some(value::Value::from_bigint(ty, number)?)
            }
            _ => None,
        };

        if let Some(value) = bytes {
            let mut buf = vec![0u8; value.size()];
            value.encode(&mut buf);
            return Ok(Some(Special::Bytes(buf)));
        }

        // expressions that only matches non-zeroed regions.
        let non_zero = match (op, lhs.traits(), rhs.traits()) {
            // any derefs must be done on a non-zero value.
            (_, (IsDeref, _), _) | (_, _, (IsDeref, _)) => true,
            // value equals something that is non-zero.
            (Op::Eq, (IsValue, _), (NonZero, _)) | (Op::Eq, (NonZero, _), (IsValue, _)) => true,
            // value does not equal to zero.
            (Op::Neq, (IsValue, _), (Zero, _)) | (Op::Neq, (Zero, _), (IsValue, _)) => true,
            // value is greater than non-zero positive.
            (Op::Gte, (IsValue, _), (NonZero, Plus)) => true,
            // value is less than non-zero negative.
            (Op::Lte, (IsValue, _), (NonZero, Minus)) => true,
            // less than zero.
            (Op::Lt, (IsValue, _), (NonZero, Minus)) => true,
            (Op::Lt, (IsValue, _), (Zero, _)) => true,
            // greater than zero.
            (Op::Gt, (IsValue, _), (NonZero, Plus)) => true,
            (Op::Gt, (IsValue, _), (Zero, _)) => true,
            _ => false,
        };

        if non_zero {
            return Ok(Some(Special::NonZero));
        }

        Ok(None)
    }
}

impl fmt::Display for Binary {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{1} {0} {2}", self.0, self.1, self.2)
    }
}

/// Only matches when all nested filters match.
#[derive(Debug, Clone)]
pub struct All(Vec<Matcher>);

impl All {
    pub fn new(filters: impl IntoIterator<Item = Matcher>) -> Self {
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

impl All {
    fn test(&self, _: Type, _: &Value, _: AddressProxy<'_>) -> anyhow::Result<bool> {
        Ok(false)
    }
}

/// Only matches when any nested filter match.
#[derive(Debug, Clone)]
pub struct Any(Vec<Matcher>);

impl Any {
    pub fn new(filters: impl IntoIterator<Item = Matcher>) -> Self {
        Any(filters.into_iter().collect())
    }
}

impl Any {
    fn test(&self, _: Type, _: &Value, _: AddressProxy<'_>) -> anyhow::Result<bool> {
        Ok(false)
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
