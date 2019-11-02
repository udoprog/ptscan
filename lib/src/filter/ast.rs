use crate::{filter, process::Process};
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

#[derive(Debug)]
pub enum Expression {
    /// Invert the truth value of an expression.
    Not(Box<Expression>),
    /// Value expression _is a pointer_.
    IsPointer(filter::ValueExpr),
    /// Test that the value equals the expected value.
    Binary(Op, filter::ValueExpr, filter::ValueExpr),
    /// Multiple expressions and:ed together.
    And(Vec<Expression>),
    /// Multiple expressions or:ed together.
    Or(Vec<Expression>),
}

impl Expression {
    /// Convert expression into a matcher.
    pub fn into_matcher(self, process: Option<&Process>) -> anyhow::Result<filter::Matcher> {
        use self::Expression::*;

        Ok(match self {
            Not(expr) => {
                let matcher = expr.into_matcher(process)?;

                filter::Matcher::Not(filter::Not {
                    matcher: Box::new(matcher),
                })
            }
            Binary(op, lhs, rhs) => filter::Matcher::Binary(filter::Binary(op, lhs, rhs)),
            IsPointer(expr) => {
                let process = match process {
                    Some(process) => process,
                    None => {
                        anyhow::bail!("can only perform pointer matching when attached to process")
                    }
                };

                filter::Matcher::Pointer(filter::Pointer::new(expr, process)?)
            }
            And(expressions) => {
                let filters = expressions
                    .into_iter()
                    .map(|e| e.into_matcher(process))
                    .collect::<anyhow::Result<Vec<filter::Matcher>>>()?;
                filter::Matcher::All(filter::All::new(filters))
            }
            Or(expressions) => {
                let filters = expressions
                    .into_iter()
                    .map(|e| e.into_matcher(process))
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
