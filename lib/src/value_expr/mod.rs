use crate::{
    filter::{ast::ValueTrait, lexer, parser},
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
    #[serde(rename = "add")]
    Add {
        lhs: Box<ValueExpr>,
        rhs: Box<ValueExpr>,
    },
    /// subtract two values.
    #[serde(rename = "sub")]
    Sub {
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
    As {
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

    /// Get the address of a value expression.
    pub fn address_of(&self, address: AddressProxy<'_>) -> anyhow::Result<Option<Address>> {
        match self {
            Self::Value => Ok(address.follow_default()?),
            Self::Last => Ok(address.follow_default()?),
            other => bail!("cannot get address of: {}", other),
        }
    }

    /// Get the type of the expression.
    pub fn type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        match self {
            Self::Value => value_type,
            Self::Last => last_type,
            Self::Initial => initial_type,
            Self::Number { ty, .. } => Some(ty.unwrap_or(Type::U32)),
            Self::Decimal { ty, .. } => Some(ty.unwrap_or(Type::F32)),
            Self::String { value } => Some(Type::String(value.len())),
            Self::Bytes { value } => Some(Type::Bytes(value.len())),
            Self::Deref { .. } => None,
            Self::AddressOf { .. } => Some(Type::Pointer),
            Self::Add { lhs, rhs } => lhs
                .type_of(initial_type, last_type, value_type)
                .or_else(|| rhs.type_of(initial_type, last_type, value_type)),
            Self::Sub { lhs, rhs } => lhs
                .type_of(initial_type, last_type, value_type)
                .or_else(|| rhs.type_of(initial_type, last_type, value_type)),
            Self::As { ty, .. } => Some(*ty),
        }
    }

    pub fn eval(
        &self,
        ty: Type,
        initial: &Value,
        last: &Value,
        proxy: AddressProxy<'_>,
    ) -> anyhow::Result<(Option<Address>, Value)> {
        match self {
            Self::Value => Ok(proxy.eval(ty)?),
            Self::Initial => Ok((None, initial.clone())),
            Self::Last => Ok((None, last.clone())),
            Self::Number { value, .. } => Ok((None, Value::from_bigint(ty, value)?)),
            Self::Decimal { value, .. } => Ok((None, Value::from_bigdecimal(ty, value)?)),
            Self::String { value } => Ok((None, Value::String(value.to_owned(), value.len()))),
            Self::Bytes { value } => Ok((None, Value::Bytes(value.clone()))),
            Self::Deref { value } => {
                let value = value.eval(Type::Pointer, initial, last, proxy)?;

                let new_address = match value {
                    (_, Value::Pointer(address)) => address,
                    _ => return Ok((None, Value::None(ty))),
                };

                let pointer = pointer::Pointer::from_address(new_address);
                let address = proxy.handle.address_proxy(&pointer);
                Ok(address.eval(ty)?)
            }
            Self::AddressOf { value } => {
                let new_address = match value.address_of(proxy)? {
                    Some(address) => address,
                    None => return Ok((None, Value::None(ty))),
                };

                Ok((None, Value::Pointer(new_address)))
            }
            Self::As { expr, .. } => expr.eval(ty, initial, last, proxy),
            Self::Add { lhs, rhs } => {
                let (_, lhs) = lhs.eval(ty, initial, last, proxy)?;
                let (_, rhs) = rhs.eval(ty, initial, last, proxy)?;
                Ok((None, lhs.add(rhs)?.unwrap_or_else(|| Value::None(ty))))
            }
            Self::Sub { lhs, rhs } => {
                let (_, lhs) = lhs.eval(ty, initial, last, proxy)?;
                let (_, rhs) = rhs.eval(ty, initial, last, proxy)?;
                Ok((None, lhs.sub(rhs)?.unwrap_or_else(|| Value::None(ty))))
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
            Self::Add { lhs, rhs } => write!(fmt, "{} + {}", lhs, rhs),
            Self::Sub { lhs, rhs } => write!(fmt, "{} - {}", lhs, rhs),
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
            Self::As { expr, ty } => write!(fmt, "{} as {}", expr, ty),
        }
    }
}
