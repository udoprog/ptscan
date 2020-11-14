use crate::{filter_expr::FilterOp, value_expr::ast::ValueExpr, ProcessInfo, Type};
use anyhow::bail;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueTrait {
    IsValue,
    IsDeref,
    IsNone,
    NonZero,
    Zero,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FilterExpr {
    /// Invert the truth value of an expression.
    Not(Box<FilterExpr>),
    /// Value expression _is a pointer_.
    IsType(ValueExpr, Type),
    /// Test if value is nan.
    IsNan(ValueExpr),
    /// Test that the value equals the expected value.
    Binary(FilterOp, ValueExpr, ValueExpr),
    /// Multiple expressions and:ed together.
    And(Vec<FilterExpr>),
    /// Multiple expressions or:ed together.
    Or(Vec<FilterExpr>),
    /// Match a value against a regular expression.
    Regex(ValueExpr, ValueExpr),
}

impl FilterExpr {
    /// Convert expression into a filter.
    pub fn into_filter(self, process: &impl ProcessInfo) -> anyhow::Result<super::FilterExpr> {
        Ok(match self {
            Self::Not(expr) => {
                let filter = expr.into_filter(process)?;

                super::FilterExpr::Not(super::Not {
                    filter: Box::new(filter),
                })
            }
            Self::Binary(op, lhs, rhs) => {
                let lhs = lhs.eval()?;
                let rhs = rhs.eval()?;
                super::FilterExpr::Binary(super::Binary(op, lhs, rhs))
            }
            Self::IsType(expr, ty) => {
                let expr = expr.eval()?;

                match ty {
                    Type::Pointer(..) => {
                        super::FilterExpr::IsPointer(super::IsPointer::new(expr, process)?)
                    }
                    ty => super::FilterExpr::IsType(super::IsType::new(expr, ty)?),
                }
            }
            Self::IsNan(expr) => {
                let expr = expr.eval()?;
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
                let expr = expr.eval()?;

                let regex = match pattern {
                    ValueExpr::String(s) => regex::bytes::Regex::new(&s)?,
                    other => bail!("cannot use expression {} as a regular expression", other),
                };

                super::FilterExpr::Regex(super::Regex::new(expr, regex))
            }
        })
    }
}
