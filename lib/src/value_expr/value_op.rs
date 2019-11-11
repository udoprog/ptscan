use crate::Value;
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ValueOp {
    #[serde(rename = "add")]
    Add,
    #[serde(rename = "sub")]
    Sub,
    #[serde(rename = "mul")]
    Mul,
    #[serde(rename = "div")]
    Div,
}

impl ValueOp {
    /// Apply the operation to the two arguments.
    pub fn apply(self, lhs: Value, rhs: Value) -> anyhow::Result<Option<Value>> {
        match self {
            Self::Add => lhs.checked_add(rhs),
            Self::Sub => lhs.checked_sub(rhs),
            Self::Mul => lhs.checked_mul(rhs),
            Self::Div => lhs.checked_div(rhs),
        }
    }
}

impl fmt::Display for ValueOp {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Add => "+".fmt(fmt),
            Self::Sub => "-".fmt(fmt),
            Self::Mul => "*".fmt(fmt),
            Self::Div => "/".fmt(fmt),
        }
    }
}
