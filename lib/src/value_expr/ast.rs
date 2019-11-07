use crate::{
    process::Process,
    utils::{EscapeString, Hex},
    Type,
};
use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
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
    /// add two values together.
    Add(Box<ValueExpr>, Box<ValueExpr>),
    /// subtract two values.
    Sub(Box<ValueExpr>, Box<ValueExpr>),
    /// A whole number literal.
    Number(BigInt, Option<Type>),
    /// A decimal literal.
    Decimal(BigDecimal, Option<Type>),
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
            Self::Initial => Initial,
            Self::Add(lhs, rhs) => {
                let lhs = lhs.eval(process)?;
                let rhs = rhs.eval(process)?;
                Add {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }
            Self::Sub(lhs, rhs) => {
                let lhs = lhs.eval(process)?;
                let rhs = rhs.eval(process)?;
                Sub {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }
            Self::Number(value, ty) => Number { value, ty },
            Self::Decimal(value, ty) => Decimal { value, ty },
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
            Self::Initial => "initial".fmt(fmt),
            Self::AddressOf(expr) => write!(fmt, "&{}", expr),
            Self::Deref(expr) => write!(fmt, "*{}", expr),
            Self::Add(lhs, rhs) => write!(fmt, "{} + {}", lhs, rhs),
            Self::Sub(lhs, rhs) => write!(fmt, "{} - {}", lhs, rhs),
            Self::Number(number, Some(ty)) => write!(fmt, "{}{}", number, ty),
            Self::Number(number, None) => write!(fmt, "{}", number),
            Self::Decimal(decimal, None) => write!(fmt, "{}", decimal),
            Self::Decimal(decimal, Some(ty)) => write!(fmt, "{}{}", decimal, ty),
            Self::String(s) => EscapeString(s).fmt(fmt),
            Self::Bytes(bytes) => Hex(bytes).fmt(fmt),
            Self::As(expr, ty) => write!(fmt, "{} as {}", expr, ty),
        }
    }
}
