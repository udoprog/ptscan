use crate::{process::Process, value_expr::ast::ValueExpr, Type};
use anyhow::bail;
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
pub enum ValueTrait {
    IsValue,
    IsDeref,
    IsNone,
    NonZero,
    Zero,
}

#[derive(Debug)]
pub enum FilterExpr {
    /// Invert the truth value of an expression.
    Not(Box<FilterExpr>),
    /// Value expression _is a pointer_.
    IsType(ValueExpr, Type),
    /// Test if value is nan.
    IsNan(ValueExpr),
    /// Test that the value equals the expected value.
    Binary(Op, ValueExpr, ValueExpr),
    /// Multiple expressions and:ed together.
    And(Vec<FilterExpr>),
    /// Multiple expressions or:ed together.
    Or(Vec<FilterExpr>),
    /// Match a value against a regular expression.
    Regex(ValueExpr, ValueExpr),
}

impl FilterExpr {
    /// Convert expression into a filter.
    pub fn into_filter(self, process: &Process) -> anyhow::Result<super::FilterExpr> {
        Ok(match self {
            Self::Not(expr) => {
                let filter = expr.into_filter(process)?;

                super::FilterExpr::Not(super::Not {
                    filter: Box::new(filter),
                })
            }
            Self::Binary(op, lhs, rhs) => {
                let lhs = lhs.eval(process)?;
                let rhs = rhs.eval(process)?;
                super::FilterExpr::Binary(super::Binary(op, lhs, rhs))
            }
            Self::IsType(expr, ty) => {
                let expr = expr.eval(process)?;

                match ty {
                    Type::Pointer => {
                        super::FilterExpr::IsPointer(super::IsPointer::new(expr, process)?)
                    }
                    ty => super::FilterExpr::IsType(super::IsType::new(expr, ty)?),
                }
            }
            Self::IsNan(expr) => {
                let expr = expr.eval(process)?;
                super::FilterExpr::IsNan(super::IsNan::new(expr)?)
            }
            Self::And(expressions) => {
                let filters = expressions
                    .into_iter()
                    .map(|e| e.into_filter(process))
                    .collect::<anyhow::Result<Vec<super::FilterExpr>>>()?;
                super::FilterExpr::All(super::All::new(filters))
            }
            Self::Or(expressions) => {
                let filters = expressions
                    .into_iter()
                    .map(|e| e.into_filter(process))
                    .collect::<anyhow::Result<Vec<super::FilterExpr>>>()?;
                super::FilterExpr::Any(super::Any::new(filters))
            }
            Self::Regex(expr, pattern) => {
                let expr = expr.eval(process)?;

                let regex = match pattern {
                    ValueExpr::String(s) => regex::bytes::Regex::new(&s)?,
                    other => bail!("cannot use expression {} as a regular expression", other),
                };

                super::FilterExpr::Regex(super::Regex::new(expr, regex))
            }
        })
    }
}