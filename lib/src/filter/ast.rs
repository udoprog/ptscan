use crate::{
    filter,
    process::{AddressProxy, Process},
    process_handle::PointerMatcher,
    Offset, Type, Value,
};
use num_bigint::{BigInt, Sign};
use std::fmt;

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
}

impl fmt::Display for Op {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Eq => "==".fmt(fmt),
            Op::Neq => "!=".fmt(fmt),
            Op::Gt => ">".fmt(fmt),
            Op::Gte => ">=".fmt(fmt),
            Op::Lt => "<".fmt(fmt),
            Op::Lte => "<=".fmt(fmt),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueTrait {
    IsValue,
    IsDeref,
    NonZero,
    Zero,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueExpr {
    /// The value of the address.
    Value,
    /// *<value>
    Deref(Box<ValueExpr>),
    /// Offset
    Offset(Box<ValueExpr>, Offset),
    /// A numerical literal.
    Number(BigInt),
    /// A string literal.
    String(String),
}

impl ValueExpr {
    /// Evaluate the expression.
    pub fn eval(&self, ty: Type, address: AddressProxy<'_>) -> anyhow::Result<Value> {
        match self {
            Self::Value => address.eval(ty),
            Self::String(string) => Ok(Value::String(string.as_bytes().to_vec())),
            Self::Number(number) => Ok(Value::from_bigint(ty, number)?),
            expr => anyhow::bail!("value expression not supported yet: {}", expr),
        }
    }

    /// Get relevant traits of the expression.
    pub fn traits(&self) -> (ValueTrait, Sign) {
        use num_traits::Zero as _;

        let value_trait = match self {
            Self::Value => ValueTrait::IsValue,
            Self::Deref(v) if **v == ValueExpr::Value => ValueTrait::IsDeref,
            Self::Number(num) if num.is_zero() => ValueTrait::Zero,
            Self::String(s) if s.as_bytes().iter().all(|c| *c == 0) => ValueTrait::Zero,
            _ => ValueTrait::NonZero,
        };

        let sign = match self {
            Self::Number(num) => num.sign(),
            _ => Sign::NoSign,
        };

        (value_trait, sign)
    }
}

impl fmt::Display for ValueExpr {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value => "value".fmt(fmt),
            Self::Deref(value) => write!(fmt, "*{}", value),
            Self::Offset(value, offset) => write!(fmt, "{}{}", value, offset),
            Self::Number(number) => write!(fmt, "{}", number),
            Self::String(string) => write!(fmt, "{}", EscapeString(string.as_bytes())),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    /// A value that stays the same.
    Same,
    /// A value that has changed.
    Changed,
    /// A value that has increased.
    Inc,
    /// A value that has decreased.
    Dec,
    /// Value expression _is a pointer_.
    IsPointer(ValueExpr),
    /// Test that the value equals the expected value.
    Binary(Op, ValueExpr, ValueExpr),
    /// Multiple expressions and:ed together.
    And(Vec<Expression>),
    /// Multiple expressions or:ed together.
    Or(Vec<Expression>),
}

impl Expression {
    /// Convert expression into a matcher.
    pub fn into_matcher(
        self,
        ty: Type,
        process: Option<&Process>,
    ) -> anyhow::Result<filter::Matcher> {
        use self::Expression::*;

        Ok(match self {
            Same => filter::Matcher::Same(filter::Same),
            Changed => filter::Matcher::Changed(filter::Changed),
            Inc => filter::Matcher::Inc(filter::Inc),
            Dec => filter::Matcher::Dec(filter::Dec),
            Binary(op, lhs, rhs) => filter::Matcher::Binary(filter::Binary(op, lhs, rhs)),
            IsPointer(expr) => {
                let process = match process {
                    Some(process) => process,
                    None => {
                        anyhow::bail!("can only perform pointer matching when attached to process")
                    }
                };

                filter::Matcher::PointerMatcher(PointerMatcher::new(expr, process)?)
            }
            And(expressions) => {
                let filters = expressions
                    .into_iter()
                    .map(|e| e.into_matcher(ty, process))
                    .collect::<anyhow::Result<Vec<filter::Matcher>>>()?;
                filter::Matcher::All(filter::All::new(filters))
            }
            Or(expressions) => {
                let filters = expressions
                    .into_iter()
                    .map(|e| e.into_matcher(ty, process))
                    .collect::<anyhow::Result<Vec<filter::Matcher>>>()?;
                filter::Matcher::Any(filter::Any::new(filters))
            }
        })
    }
}

pub struct EscapeString<'a>(pub &'a [u8]);

impl fmt::Display for EscapeString<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "\"")?;

        for c in self.0 {
            match c {
                b'\\' => write!(fmt, "\\\\")?,
                b'"' => write!(fmt, "\\\"")?,
                b' ' => write!(fmt, " ")?,
                b'\t' => write!(fmt, "\\t")?,
                b'\n' => write!(fmt, "\\n")?,
                b'\r' => write!(fmt, "\\r")?,
                b'\0' => write!(fmt, "\\0")?,
                c if *c < 0x80 => write!(fmt, "{}", *c as char)?,
                c => write!(fmt, "\\x{:02x}", c)?,
            }
        }

        write!(fmt, "\"")?;
        Ok(())
    }
}
