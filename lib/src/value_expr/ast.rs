use crate::{
    process::Process,
    utils::{EscapeString, Hex},
    Encoding, Type,
};
use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueExpr {
    /// The value of the address.
    Value,
    /// The last known value.
    Last,
    /// The initial value of the scan result.
    Initial,
    /// addressof `&<value>`.
    AddressOf(Box<ValueExpr>),
    /// deref `*<value>`
    Deref(Box<ValueExpr>),
    /// a binary operation over two values.
    Binary(super::ValueOp, Box<ValueExpr>, Box<ValueExpr>),
    /// A whole number literal.
    Number(BigInt, Option<Type>),
    /// A decimal literal.
    Decimal(BigDecimal, Option<Type>),
    /// A string literal.
    String(Encoding, String),
    /// A number of raw bytes.
    Bytes(Vec<u8>),
    /// Casting a value.
    Cast(Box<ValueExpr>, Type),
}

impl ValueExpr {
    /// Evaluate the expression.
    pub fn eval(self, process: &Process) -> anyhow::Result<super::ValueExpr> {
        use super::ValueExpr::*;

        Ok(match self {
            Self::Value => Value,
            Self::Last => Last,
            Self::Initial => Initial,
            Self::Binary(op, lhs, rhs) => {
                let lhs = lhs.eval(process)?;
                let rhs = rhs.eval(process)?;
                Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }
            Self::Number(value, ty) => Number { value, ty },
            Self::Decimal(value, ty) => Decimal { value, ty },
            Self::String(encoding, value) => String { encoding, value },
            Self::Bytes(value) => Bytes { value },
            Self::AddressOf(value) => AddressOf {
                value: Box::new(value.eval(process)?),
            },
            Self::Deref(value) => Deref {
                value: Box::new(value.eval(process)?),
            },
            Self::Cast(expr, ty) => {
                let expr = expr.eval(process)?;
                Cast {
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
            Self::Initial => "initial".fmt(fmt),
            Self::AddressOf(expr) => write!(fmt, "&{}", expr),
            Self::Deref(expr) => write!(fmt, "*{}", expr),
            Self::Binary(op, lhs, rhs) => write!(fmt, "{} {} {}", lhs, op, rhs),
            Self::Number(number, Some(ty)) => write!(fmt, "{}{}", number, ty),
            Self::Number(number, None) => write!(fmt, "{}", number),
            Self::Decimal(decimal, None) => write!(fmt, "{}", decimal),
            Self::Decimal(decimal, Some(ty)) => write!(fmt, "{}{}", decimal, ty),
            Self::String(_, s) => EscapeString(s).fmt(fmt),
            Self::Bytes(bytes) => Hex(bytes).fmt(fmt),
            Self::Cast(expr, ty) => write!(fmt, "{} as {}", expr, ty),
        }
    }
}
