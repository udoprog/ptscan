use crate::{
    filter_expr::{ast::ValueTrait, lexer, parser},
    pointer,
    process::Process,
    utils::{EscapeString, Hex},
    value::Value,
    Address, AddressProxy, Encoding, Sign, Type, TypeHint,
};
use anyhow::{anyhow, bail};
use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};
use std::fmt;

pub use self::value_op::ValueOp;

pub(crate) mod ast;
mod value_op;

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
    String { encoding: Encoding, value: String },
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
            Self::String { value, .. } => {
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
            Self::Number { value, .. } => Sign::from(value.sign()),
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
        initial_type: TypeHint,
        last_type: TypeHint,
        value_type: TypeHint,
    ) -> anyhow::Result<TypeHint> {
        use self::TypeHint::*;

        Ok(match *self {
            Self::Initial => initial_type.unwrap_or(NoHint),
            Self::Last => last_type.unwrap_or(NoHint),
            Self::Value => value_type.unwrap_or(NoHint),
            Self::Deref { .. } => NoHint,
            Self::AddressOf { .. } => Explicit(Type::Pointer),
            Self::Binary {
                op,
                ref lhs,
                ref rhs,
            } => {
                let lhs = lhs.type_of(initial_type, last_type, value_type)?;
                let rhs = rhs.type_of(initial_type, last_type, value_type)?;

                match (lhs, rhs) {
                    (Explicit(lhs), Explicit(rhs)) => {
                        if lhs != rhs {
                            bail!("incompatible types in expression: {} {} {}", lhs, op, rhs)
                        }

                        Explicit(lhs)
                    }
                    (Explicit(ty), _) | (_, Explicit(ty)) => Explicit(ty),
                    (Implicit(ty), _) | (_, Implicit(ty)) => Implicit(ty),
                    _ => NoHint,
                }
            }
            Self::Cast { ty, .. } => Explicit(ty),
            Self::Number { ty: Some(ty), .. } => Explicit(ty),
            Self::Number { ty: None, .. } => Implicit(Type::U32),
            Self::Decimal { ty: Some(ty), .. } => Explicit(ty),
            Self::Decimal { ty: None, .. } => Implicit(Type::F32),
            Self::String { encoding, .. } => Implicit(Type::String(encoding)),
            Self::Bytes { ref value } => Implicit(Type::Bytes(Some(value.len()))),
        })
    }

    /// Get the implicit type of the expression.
    pub fn implicit_type_of(&self) -> anyhow::Result<Option<Type>> {
        Ok(match self {
            Self::Number { .. } => Some(Type::U32),
            Self::Decimal { .. } => Some(Type::F32),
            Self::String { .. } => Some(Type::String(Encoding::default())),
            Self::Bytes { value } => Some(Type::Bytes(Some(value.len()))),
            Self::Binary { lhs, rhs, .. } => {
                if let Some(ty) = lhs.implicit_type_of()? {
                    return Ok(Some(ty));
                }

                if let Some(ty) = rhs.implicit_type_of()? {
                    return Ok(Some(ty));
                }

                None
            }
            _ => None,
        })
    }

    /// Get a type hint for the value type.
    pub fn value_type_of(&self, cast_type: TypeHint) -> anyhow::Result<TypeHint> {
        use self::TypeHint::*;

        Ok(match *self {
            Self::Value => cast_type,
            Self::Deref { ref value, .. } => match &**value {
                Self::Value => Explicit(Type::Pointer),
                other => other.value_type_of(NoHint)?,
            },
            Self::Binary {
                op,
                ref lhs,
                ref rhs,
                ..
            } => match (&**lhs, &**rhs) {
                (Self::Value, other) | (other, Self::Value) => {
                    other.type_of(NoHint, NoHint, NoHint)?
                }
                (lhs, rhs) => {
                    let lhs = lhs.value_type_of(cast_type)?;
                    let rhs = rhs.value_type_of(cast_type)?;

                    match (lhs, rhs) {
                        (Explicit(lhs), Explicit(rhs)) => {
                            if lhs != rhs {
                                bail!("incompatible types in expression: {} {} {}", lhs, op, rhs)
                            }

                            Explicit(lhs)
                        }
                        (Explicit(ty), _) | (_, Explicit(ty)) => (Explicit(ty)),
                        (Implicit(ty), _) | (_, Implicit(ty)) => (Implicit(ty)),
                        _ => NoHint,
                    }
                }
            },
            Self::Cast { ref expr, ty } => match &**expr {
                Self::Value => Explicit(ty),
                ref other => other.value_type_of(Explicit(ty))?,
            },
            _ => NoHint,
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
            Self::Value => Ok(proxy.eval(expr_type)?.0),
            Self::Initial => Ok(initial.clone()),
            Self::Last => Ok(last.clone()),
            Self::Number { ref value, .. } => Ok(Value::from_bigint(expr_type, value)?),
            Self::Decimal { ref value, .. } => Ok(Value::from_bigdecimal(expr_type, value)?),
            Self::String {
                encoding,
                ref value,
            } => Ok(Value::String(encoding, value.to_owned())),
            Self::Bytes { ref value } => Ok(Value::Bytes(value.clone())),
            Self::Deref { ref value } => {
                let value = value.eval(initial, last, value_type, Type::Pointer, proxy)?;

                let address = match value.as_address() {
                    Some(address) => address,
                    None => return Ok(Value::None(value_type)),
                };

                let pointer = pointer::Pointer::from_address(address);
                let mut proxy = proxy.handle.address_proxy(&pointer);
                let (value, _) = proxy.eval(expr_type)?;
                Ok(value)
            }
            Self::AddressOf { ref value } => {
                let new_address =
                    match value.address_of(initial, last, value_type, expr_type, proxy)? {
                        Some(address) => address,
                        None => return Ok(Value::None(expr_type)),
                    };

                Ok(Value::Pointer(new_address))
            }
            Self::Cast { ref expr, .. } => {
                let inner_type = expr
                    .type_of(
                        TypeHint::Explicit(initial.ty()),
                        TypeHint::Explicit(last.ty()),
                        TypeHint::Explicit(value_type),
                    )?
                    .ok_or_else(|| anyhow!("cannot determine type of expression: {}", expr))?;

                let value = expr.eval(initial, last, value_type, inner_type, proxy)?;

                Ok(expr_type
                    .convert(value)
                    .unwrap_or_else(|| Value::None(expr_type)))
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
            Self::String { value, .. } => write!(fmt, "{}", EscapeString(value)),
            Self::Bytes { value } => write!(fmt, "{}", Hex(value)),
            Self::Cast { expr, ty } => write!(fmt, "{} as {}", expr, ty),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{ast::ValueExpr as V, ValueOp};
    use crate::{
        filter_expr::{lexer, parser},
        Type,
    };

    fn parse(input: &str) -> anyhow::Result<V> {
        Ok(parser::ValueExprParser::new().parse(lexer::Lexer::new(input))?)
    }

    #[test]
    fn test_parsing() -> anyhow::Result<()> {
        use self::ValueOp::*;

        assert_eq!(V::Value, parse("value")?);
        assert_eq!(V::Number(42.into(), None), parse("42")?);
        assert_eq!(V::Number(42.into(), Some(Type::U32)), parse("42u32")?);
        assert_eq!(
            V::Decimal(42.42.into(), Some(Type::U32)),
            parse("42.42u32")?
        );
        assert_eq!(
            V::Cast(Box::new(V::Value), Type::U32),
            parse("value as u32")?
        );
        assert_eq!(
            V::Binary(Add, Box::new(V::Value), Box::new(V::Value)),
            parse("value + value")?
        );
        assert_eq!(
            V::Binary(Sub, Box::new(V::Value), Box::new(V::Value)),
            parse("value - value")?
        );
        assert_eq!(
            V::Binary(Mul, Box::new(V::Value), Box::new(V::Value)),
            parse("value * value")?
        );
        assert_eq!(
            V::Binary(Div, Box::new(V::Value), Box::new(V::Value)),
            parse("value / value")?
        );

        assert_eq!(
            V::Binary(
                Mul,
                Box::new(V::Binary(Add, Box::new(V::Value), Box::new(V::Value))),
                Box::new(V::Value)
            ),
            parse("(value + value) * value")?
        );

        Ok(())
    }
}
