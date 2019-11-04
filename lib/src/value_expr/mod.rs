use crate::{
    filter::{ast::ValueTrait, lexer, parser},
    pointer,
    process::Process,
    utils::{EscapeString, Hex},
    value::Value,
    Address, AddressProxy, Offset, Type,
};
use anyhow::bail;
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
    /// *<value>
    #[serde(rename = "deref")]
    Deref { value: Box<ValueExpr> },
    /// &<value>
    #[serde(rename = "address-of")]
    AddressOf { value: Box<ValueExpr> },
    /// Offset
    #[serde(rename = "offset")]
    Offset {
        value: Box<ValueExpr>,
        offset: Offset,
    },
    /// A numerical literal.
    #[serde(rename = "number")]
    Number { value: BigInt },
    /// A string literal.
    #[serde(rename = "number")]
    String { value: String },
    /// A number of raw bytes.
    #[serde(rename = "number")]
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
            Self::Number { value } => {
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
            Self::Number { value } => value.sign(),
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
    pub fn type_of(&self, last_type: Option<Type>, value_type: Option<Type>) -> Option<Type> {
        match self {
            Self::Value => value_type,
            Self::Last => last_type,
            Self::Number { .. } => Some(Type::U32),
            Self::String { value } => Some(Type::String(value.len())),
            Self::Bytes { value } => Some(Type::Bytes(value.len())),
            Self::Deref { .. } => None,
            Self::AddressOf { .. } => Some(Type::Pointer),
            Self::Offset { value, .. } => value.type_of(last_type, value_type),
            Self::As { ty, .. } => Some(*ty),
        }
    }

    pub fn eval(
        &self,
        ty: Type,
        last: &Value,
        proxy: AddressProxy<'_>,
    ) -> anyhow::Result<(Option<Address>, Value)> {
        match self {
            Self::Value => Ok(proxy.eval(ty)?),
            Self::Last => Ok((None, last.clone())),
            Self::Number { value } => Ok((None, Value::from_bigint(ty, value)?)),
            Self::String { value } => Ok((None, Value::String(value.to_owned(), value.len()))),
            Self::Bytes { value } => Ok((None, Value::Bytes(value.clone()))),
            Self::Deref { value } => {
                let new_address = match proxy.eval(Type::Pointer)? {
                    (_, Value::Pointer(address)) => address,
                    _ => return Ok((None, Value::None(ty))),
                };

                let pointer = pointer::Pointer::from_address(new_address);
                let address = proxy.handle.address_proxy(&pointer);
                Ok(value.eval(ty, last, address)?)
            }
            Self::AddressOf { value } => {
                let new_address = match value.address_of(proxy)? {
                    Some(address) => address,
                    None => return Ok((None, Value::None(ty))),
                };

                Ok((None, Value::Pointer(new_address)))
            }
            Self::As { expr, .. } => expr.eval(ty, last, proxy),
            _ => bail!("can't evaluate expression yet: {}", self),
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
            Self::Deref { value } => write!(fmt, "*{}", value),
            Self::AddressOf { value } => write!(fmt, "&{}", value),
            Self::Offset { value, offset } => write!(fmt, "{}{}", value, offset),
            Self::Number { value } => write!(fmt, "{}", value),
            Self::String { value } => write!(fmt, "{}", EscapeString(value)),
            Self::Bytes { value } => write!(fmt, "{}", Hex(value)),
            Self::As { expr, ty } => write!(fmt, "{} as {}", expr, ty),
        }
    }
}
