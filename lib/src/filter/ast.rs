use crate::{
    filter::{self, Matcher},
    process::Process,
    Offset, Type,
};
use anyhow::bail;
use num_bigint::BigInt;
use std::fmt;

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
    StartsWith,
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
            Op::StartsWith => "^".fmt(fmt),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueExpr {
    /// The value of the address.
    Value,
    /// The last known value.
    Last,
    /// addressof `&<value>`.
    AddressOf(Box<ValueExpr>),
    /// deref `*<value>`
    Deref(Box<ValueExpr>),
    /// Offset
    Offset(Box<ValueExpr>, Offset),
    /// A numerical literal.
    Number(BigInt, Option<Type>),
    /// A string literal.
    String(String),
    /// A number of raw bytes.
    Bytes(Vec<u8>),
    /// Casting a value.
    As(Box<ValueExpr>, Type),
}

impl ValueExpr {
    /// Evaluate the expression.
    pub fn eval(self, process: &Process) -> anyhow::Result<filter::ValueExpr> {
        use filter::ValueExpr::*;

        Ok(match self {
            Self::Value => Value,
            Self::Last => Last,
            Self::Offset(value, offset) => Offset {
                value: Box::new(value.eval(process)?),
                offset,
            },
            Self::Number(value, _) => Number { value },
            Self::String(value) => String { value },
            Self::Bytes(value) => Bytes { value },
            Self::AddressOf(value) => AddressOf {
                value: Box::new(value.eval(process)?),
            },
            Self::Deref(value) => Deref {
                value: Box::new(value.eval(process)?),
            },
            Self::As(expr, ty) => {
                let expr = expr.eval(process)?;
                As {
                    expr: Box::new(expr),
                    ty,
                }
            }
        })
    }
}

impl fmt::Display for ValueExpr {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value => "value".fmt(fmt),
            Self::Last => "last".fmt(fmt),
            Self::AddressOf(expr) => write!(fmt, "&{}", expr),
            Self::Deref(expr) => write!(fmt, "*{}", expr),
            Self::Offset(expr, offset) => write!(fmt, "{} {}", expr, offset),
            Self::Number(number, Some(ty)) => write!(fmt, "{}{}", number, ty),
            Self::Number(number, None) => write!(fmt, "{}", number),
            Self::String(s) => EscapeString(s).fmt(fmt),
            Self::Bytes(bytes) => Hex(bytes).fmt(fmt),
            Self::As(expr, ty) => write!(fmt, "{} as {}", expr, ty),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueTrait {
    IsValue,
    IsDeref,
    IsNone,
    NonZero,
    Zero,
}

#[derive(Debug)]
pub enum Expression {
    /// Invert the truth value of an expression.
    Not(Box<Expression>),
    /// Value expression _is a pointer_.
    IsType(ValueExpr, Type),
    /// Test that the value equals the expected value.
    Binary(Op, ValueExpr, ValueExpr),
    /// Multiple expressions and:ed together.
    And(Vec<Expression>),
    /// Multiple expressions or:ed together.
    Or(Vec<Expression>),
    /// Match a value against a regular expression.
    Regex(ValueExpr, ValueExpr),
}

impl Expression {
    /// Convert expression into a matcher.
    pub fn into_matcher(self, process: &Process) -> anyhow::Result<Matcher> {
        Ok(match self {
            Self::Not(expr) => {
                let matcher = expr.into_matcher(process)?;

                Matcher::Not(filter::Not {
                    matcher: Box::new(matcher),
                })
            }
            Self::Binary(op, lhs, rhs) => {
                let lhs = lhs.eval(process)?;
                let rhs = rhs.eval(process)?;
                Matcher::Binary(filter::Binary(op, lhs, rhs))
            }
            Self::IsType(expr, ty) => {
                let expr = expr.eval(process)?;

                match ty {
                    Type::Pointer => Matcher::IsPointer(filter::IsPointer::new(expr, process)?),
                    Type::None => Matcher::IsNone(filter::IsNone::new(expr)?),
                    ty => bail!("cannot type check for {}", ty),
                }
            }
            Self::And(expressions) => {
                let filters = expressions
                    .into_iter()
                    .map(|e| e.into_matcher(process))
                    .collect::<anyhow::Result<Vec<Matcher>>>()?;
                Matcher::All(filter::All::new(filters))
            }
            Self::Or(expressions) => {
                let filters = expressions
                    .into_iter()
                    .map(|e| e.into_matcher(process))
                    .collect::<anyhow::Result<Vec<Matcher>>>()?;
                Matcher::Any(filter::Any::new(filters))
            }
            Self::Regex(expr, pattern) => {
                let expr = expr.eval(process)?;

                let regex = match pattern {
                    ValueExpr::String(s) => regex::bytes::Regex::new(&s)?,
                    other => bail!("cannot use expression {} as a regular expression", other),
                };

                Matcher::Regex(filter::Regex::new(expr, regex))
            }
        })
    }
}

pub struct EscapeString<'a>(pub &'a str);

impl fmt::Display for EscapeString<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "\"")?;

        for c in self.0.chars() {
            match c {
                '\\' => write!(fmt, "\\\\")?,
                '"' => write!(fmt, "\\\"")?,
                ' ' => write!(fmt, " ")?,
                '\t' => write!(fmt, "\\t")?,
                '\n' => write!(fmt, "\\n")?,
                '\r' => write!(fmt, "\\r")?,
                c => write!(fmt, "{}", c)?,
            }
        }

        write!(fmt, "\"")?;
        Ok(())
    }
}

pub struct Hex<'a>(pub &'a [u8]);

impl fmt::Display for Hex<'_> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut it = self.0.iter().cloned();

        let last = it.next_back();

        while let Some(c) = it.next() {
            write!(fmt, "{:02X} ", c)?;
        }

        if let Some(c) = last {
            write!(fmt, "{:02X}", c)?;
        }

        Ok(())
    }
}
