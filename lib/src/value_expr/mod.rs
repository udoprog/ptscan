use crate::{
    filter_expr::{ast::ValueTrait, lexer, parser},
    pointer,
    process::Process,
    utils::{EscapeString, Hex},
    value::Value,
    Address, AddressProxy, Type,
};
use anyhow::bail;
use bigdecimal::BigDecimal;
use num_bigint::{BigInt, Sign};
use serde::{Deserialize, Serialize};
use std::fmt;

pub(crate) mod ast;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum ValueExpr {
    /// The value of the address.
    #[serde(rename = "value")]
    Value,
    /// The last known value.
    #[serde(rename = "value")]
    Last,
    /// The initial value of the scan result.
    #[serde(rename = "initial")]
    Initial,
    /// *<value>
    #[serde(rename = "deref")]
    Deref { value: Box<ValueExpr> },
    /// &<value>
    #[serde(rename = "address-of")]
    AddressOf { value: Box<ValueExpr> },
    /// add two values together
    #[serde(rename = "binary")]
    Binary {
        op: ValueOp,
        lhs: Box<ValueExpr>,
        rhs: Box<ValueExpr>,
    },
    /// A whole number literal.
    #[serde(rename = "number")]
    Number {
        value: BigInt,
        #[serde(rename = "type_hint")]
        ty: Option<Type>,
    },
    /// A decimal literal.
    #[serde(rename = "decimal")]
    Decimal {
        value: BigDecimal,
        #[serde(rename = "type_hint")]
        ty: Option<Type>,
    },
    /// A string literal.
    #[serde(rename = "string")]
    String { value: String },
    /// A number of raw bytes.
    #[serde(rename = "bytes")]
    Bytes { value: Vec<u8> },
    /// Cast expression.
    #[serde(rename = "as")]
    Cast {
        expr: Box<ValueExpr>,
        #[serde(rename = "as_type")]
        ty: Type,
    },
}

impl ValueExpr {
    /// If this is a default expression.
    pub fn is_default(&self) -> bool {
        match self {
            ValueExpr::Value => true,
            _ => false,
        }
    }

    pub fn reduced(&self) -> &Self {
        match self {
            Self::Cast { expr, .. } => expr.reduced(),
            other => other,
        }
    }

    /// Parse a value expression to use.
    pub fn parse(input: &str, process: &Process) -> anyhow::Result<ValueExpr> {
        let expr = parser::ValueExprParser::new().parse(lexer::Lexer::new(input))?;
        Ok(expr.eval(process)?)
    }

    /// Get relevant traits of the expression.
    pub fn traits(&self) -> (ValueTrait, Sign) {
        use num_traits::Zero as _;

        let value_trait = match self {
            Self::Value => ValueTrait::IsValue,
            Self::Deref { value } if **value == ValueExpr::Value => ValueTrait::IsDeref,
            Self::Number { value, .. } => {
                if value.is_zero() {
                    ValueTrait::Zero
                } else {
                    ValueTrait::NonZero
                }
            }
            Self::Decimal { value, .. } => {
                if value.is_zero() {
                    ValueTrait::Zero
                } else {
                    ValueTrait::NonZero
                }
            }
            Self::String { value } => {
                if value.as_bytes().iter().all(|c| *c == 0) {
                    ValueTrait::Zero
                } else {
                    ValueTrait::NonZero
                }
            }
            Self::Bytes { value } => {
                if value.iter().all(|c| *c == 0) {
                    ValueTrait::Zero
                } else {
                    ValueTrait::NonZero
                }
            }
            _ => ValueTrait::NonZero,
        };

        let sign = match self {
            Self::Number { value, .. } => value.sign(),
            _ => Sign::NoSign,
        };

        (value_trait, sign)
    }

    pub fn address_of(
        &self,
        initial: &Value,
        last: &Value,
        value_type: Type,
        expr_type: Type,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Option<Address>> {
        let mut stack = Vec::new();
        self.stacked_address_of(&mut stack, initial, last, value_type, expr_type, proxy)?;
        Ok(stack.last().copied().and_then(|a| a))
    }

    /// Get the address of a value expression.
    pub fn stacked_address_of(
        &self,
        stack: &mut Vec<Option<Address>>,
        initial: &Value,
        last: &Value,
        value_type: Type,
        expr_type: Type,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<()> {
        match self {
            Self::Value => {
                stack.push(proxy.follow_default()?);
            }
            Self::Deref { value } => {
                let value = value.eval(initial, last, value_type, expr_type, proxy)?;
                stack.push(value.as_address());
            }
            Self::AddressOf { value } => {
                value.stacked_address_of(stack, initial, last, value_type, expr_type, proxy)?;
                stack.pop();
            }
            other => bail!("cannot get address of: {}", other),
        }

        Ok(())
    }

    /// Get the type of the expression.
    pub fn type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> anyhow::Result<Option<Type>> {
        Ok(match self {
            Self::Value => value_type,
            Self::Last => last_type,
            Self::Initial => initial_type,
            Self::Number { ty, .. } => Some(ty.unwrap_or(Type::U32)),
            Self::Decimal { ty, .. } => Some(ty.unwrap_or(Type::F32)),
            Self::String { value } => Some(Type::String(value.len())),
            Self::Bytes { value } => Some(Type::Bytes(value.len())),
            Self::Deref { .. } => None,
            Self::AddressOf { .. } => Some(Type::Pointer),
            Self::Binary { op, lhs, rhs } => {
                let lhs_type = lhs.type_of(initial_type, last_type, value_type)?;
                let rhs_type = rhs.type_of(initial_type, last_type, value_type)?;

                match (lhs_type, rhs_type) {
                    (Some(lhs_type), Some(rhs_type)) if lhs_type == rhs_type => Some(lhs_type),
                    (Some(lhs_type), Some(rhs_type)) => bail!(
                        "incompatible types in expression: {} {} {}",
                        lhs_type,
                        op,
                        rhs_type
                    ),
                    (Some(expr_type), None) => Some(expr_type),
                    (None, Some(expr_type)) => Some(expr_type),
                    _ => None,
                }
            }
            Self::Cast { ty, .. } => Some(*ty),
        })
    }

    /// Get a type hint for the value type.
    pub fn value_type_of(&self) -> anyhow::Result<Option<Type>> {
        Ok(match self {
            Self::Deref { value, .. } => match &**value {
                Self::Value => Some(Type::Pointer),
                other => other.value_type_of()?,
            },
            Self::Binary { lhs, rhs, .. } => {
                if let Some(lhs) = lhs.value_type_of()? {
                    return Ok(Some(lhs));
                }

                rhs.value_type_of()?
            }
            Self::Cast { expr, ty } => match &**expr {
                Self::Value => Some(*ty),
                other => other.value_type_of()?,
            },
            _ => None,
        })
    }

    pub fn eval(
        &self,
        initial: &Value,
        last: &Value,
        value_type: Type,
        expr_type: Type,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Value> {
        match *self {
            Self::Value => Ok(proxy.eval(value_type)?),
            Self::Initial => Ok(initial.clone()),
            Self::Last => Ok(last.clone()),
            Self::Number { ref value, .. } => Ok(Value::from_bigint(expr_type, value)?),
            Self::Decimal { ref value, .. } => Ok(Value::from_bigdecimal(expr_type, value)?),
            Self::String { ref value } => Ok(Value::String(value.to_owned(), value.len())),
            Self::Bytes { ref value } => Ok(Value::Bytes(value.clone())),
            Self::Deref { ref value } => {
                let value = value.eval(initial, last, value_type, Type::Pointer, proxy)?;

                let address = match value.as_address() {
                    Some(address) => address,
                    None => return Ok(Value::None(value_type)),
                };

                let pointer = pointer::Pointer::from_address(address);
                let mut proxy = proxy.handle.address_proxy(&pointer);
                Ok(proxy.eval(expr_type)?)
            }
            Self::AddressOf { ref value } => {
                let new_address =
                    match value.address_of(initial, last, value_type, expr_type, proxy)? {
                        Some(address) => address,
                        None => return Ok(Value::None(value_type)),
                    };

                Ok(Value::Pointer(new_address))
            }
            Self::Cast { ref expr, ty } => {
                let value = expr.eval(initial, last, value_type, ty, proxy)?;
                let value = ty.convert(value).unwrap_or_else(|| Value::None(ty));
                Ok(value)
            }
            Self::Binary {
                ref lhs,
                ref rhs,
                op,
            } => {
                let lhs = lhs.eval(initial, last, value_type, expr_type, proxy)?;
                let rhs = rhs.eval(initial, last, value_type, expr_type, proxy)?;
                Ok(op
                    .apply(lhs, rhs)?
                    .unwrap_or_else(|| Value::None(Type::None)))
            }
        }
    }
}

impl Default for ValueExpr {
    fn default() -> Self {
        Self::Value
    }
}

impl fmt::Display for ValueExpr {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value => "value".fmt(fmt),
            Self::Last => "last".fmt(fmt),
            Self::Initial => "initial".fmt(fmt),
            Self::Deref { value } => write!(fmt, "*{}", value),
            Self::AddressOf { value } => write!(fmt, "&{}", value),
            Self::Binary { op, lhs, rhs } => write!(fmt, "{} {} {}", lhs, op, rhs),
            Self::Number { value, ty: None } => write!(fmt, "{}", value),
            Self::Number {
                value,
                ty: Some(ty),
            } => write!(fmt, "{}{}", value, ty),
            Self::Decimal { value, ty: None } => write!(fmt, "{}", value),
            Self::Decimal {
                value,
                ty: Some(ty),
            } => write!(fmt, "{}{}", value, ty),
            Self::String { value } => write!(fmt, "{}", EscapeString(value)),
            Self::Bytes { value } => write!(fmt, "{}", Hex(value)),
            Self::Cast { expr, ty } => write!(fmt, "{} as {}", expr, ty),
        }
    }
}
