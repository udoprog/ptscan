use crate::{
    filter::{self, Matcher},
    process::Process,
    value, Offset, Type,
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
    /// Get a hint at what type to use for this expression.
    pub fn type_from_hint(&self, external: Option<Type>) -> Option<Type> {
        let implicit = match self {
            Self::Number(.., ty) => Some(ty.unwrap_or(Type::U32)),
            Self::String(s) => Some(Type::String(s.len())),
            Self::Bytes(s) => Some(Type::Bytes(s.len())),
            Self::AddressOf(..) => Some(Type::Pointer),
            _ => None,
        };

        let explicit = match self {
            Self::As(_, ty) => Some(*ty),
            _ => None,
        };

        explicit.or(external).or(implicit)
    }

    /// Evaluate the expression.
    pub fn eval(self, ty: Type, process: &Process) -> anyhow::Result<filter::ValueExpr> {
        use filter::ValueExpr::*;

        Ok(match self {
            Self::Value => Value,
            Self::Last => Last,
            Self::Offset(value, offset) => Offset {
                value: Box::new(value.eval(ty, process)?),
                offset,
            },
            Self::Number(number, _) => Literal {
                value: value::Value::from_bigint(ty, &number)?,
            },
            Self::String(string) => Literal {
                value: ty.decode(process, string.as_bytes())?,
            },
            Self::Bytes(bytes) => Literal {
                value: ty.decode(process, &bytes)?,
            },
            Self::As(value, ty) => value.eval(ty, process)?,
            Self::AddressOf(value) => AddressOf {
                value: Box::new(value.eval(ty, process)?),
            },
            Self::Deref(value) => Deref {
                value: Box::new(value.eval(ty, process)?),
            },
        })
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
}

impl Expression {
    pub fn type_from_hint(&self, ty: Option<Type>) -> Option<Type> {
        match self {
            Self::Not(expr) => expr.type_from_hint(ty),
            Self::IsType(_, ty) => Some(*ty),
            Self::Binary(_, lhs, rhs) => lhs.type_from_hint(ty).or(rhs.type_from_hint(ty)),
            Self::And(exprs) | Self::Or(exprs) => {
                exprs.iter().flat_map(|e| e.type_from_hint(ty)).next()
            }
        }
    }

    /// Convert expression into a matcher.
    pub fn into_matcher(self, ty: Type, process: &Process) -> anyhow::Result<Matcher> {
        Ok(match self {
            Self::Not(expr) => {
                let matcher = expr.into_matcher(ty, process)?;

                Matcher::Not(filter::Not {
                    matcher: Box::new(matcher),
                })
            }
            Self::Binary(op, lhs, rhs) => {
                let lhs = lhs.eval(ty, process)?;
                let rhs = rhs.eval(ty, process)?;
                Matcher::Binary(filter::Binary(op, lhs, rhs))
            }
            Self::IsType(expr, ty) => {
                let expr = expr.eval(ty, process)?;

                match ty {
                    Type::Pointer => Matcher::IsPointer(filter::IsPointer::new(expr, process)?),
                    Type::None => Matcher::IsNone(filter::IsNone::new(expr)?),
                    ty => bail!("cannot type check for {}", ty),
                }
            }
            Self::And(expressions) => {
                let filters = expressions
                    .into_iter()
                    .map(|e| e.into_matcher(ty, process))
                    .collect::<anyhow::Result<Vec<Matcher>>>()?;
                Matcher::All(filter::All::new(filters))
            }
            Self::Or(expressions) => {
                let filters = expressions
                    .into_iter()
                    .map(|e| e.into_matcher(ty, process))
                    .collect::<anyhow::Result<Vec<Matcher>>>()?;
                Matcher::Any(filter::Any::new(filters))
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
                b'\0' => break,
                c if *c < 0x80 => write!(fmt, "{}", *c as char)?,
                c => write!(fmt, "\\x{:02x}", c)?,
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
