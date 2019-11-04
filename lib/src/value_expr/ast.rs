use crate::{
    process::Process,
    utils::{EscapeString, Hex},
    Offset, Type,
};
use num_bigint::BigInt;
use std::fmt;

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
    pub fn eval(self, process: &Process) -> anyhow::Result<super::ValueExpr> {
        use super::ValueExpr::*;

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
