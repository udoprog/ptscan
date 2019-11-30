use crate::{
    filter_expr::{ast::ValueTrait, lexer, parser},
    process::Process,
    utils::{EscapeString, Hex},
    value::Value,
    Address, AddressProxy, Pointer, Sign, Type, TypeHint, ValueRef,
};
use anyhow::{anyhow, bail};
use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use serde::{Deserialize, Serialize};
use std::fmt;

pub use self::value_op::ValueOp;

pub(crate) mod ast;
mod value_op;

/// A value expression after it has been type checked.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedValueExpr<'a> {
    Value(Type),
    Last(Type),
    Initial(Type),
    Deref(Type, Box<TypedValueExpr<'a>>),
    AddressOf(Type, Box<TypedValueExpr<'a>>),
    Binary(
        Type,
        ValueOp,
        Box<TypedValueExpr<'a>>,
        Box<TypedValueExpr<'a>>,
    ),
    Number(Type, &'a BigInt),
    Decimal(Type, &'a BigDecimal),
    String(Type, &'a String),
    Bytes(Type, &'a Vec<u8>),
    Cast(Type, Box<TypedValueExpr<'a>>),
}

impl TypedValueExpr<'_> {
    /// Evaluate the expression to return a value.
    pub fn eval(
        &self,
        initial: ValueRef<'_>,
        last: ValueRef<'_>,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Value> {
        Ok(match *self {
            Self::Value(expr_type) => proxy.eval(expr_type)?.0,
            Self::Initial(expr_type) => expr_type.convert(proxy.handle, initial.to_owned()),
            Self::Last(expr_type) => expr_type.convert(proxy.handle, last.to_owned()),
            Self::Number(expr_type, value) => Value::from_bigint(expr_type, value)?,
            Self::Decimal(expr_type, value) => Value::from_bigdecimal(expr_type, value)?,
            Self::String(expr_type, value) => {
                expr_type.convert(proxy.handle, Value::String(value.to_string()))
            }
            Self::Bytes(expr_type, value) => {
                expr_type.convert(proxy.handle, Value::Bytes(value.clone()))
            }
            Self::Deref(expr_type, ref value) => {
                let value = Type::Pointer.convert(proxy.handle, value.eval(initial, last, proxy)?);

                let address = match value.as_address() {
                    Some(address) => address,
                    None => return Ok(Value::None),
                };

                let pointer = Pointer::from(address);
                let (value, _) = proxy.handle.address_proxy(&pointer).eval(expr_type)?;
                expr_type.convert(proxy.handle, value)
            }
            Self::AddressOf(expr_type, ref value) => {
                let new_address = match value.address_of(initial, last, proxy)? {
                    Some(address) => address,
                    None => return Ok(Value::None),
                };

                expr_type.convert(proxy.handle, Value::Pointer(new_address))
            }
            Self::Cast(expr_type, ref expr) => {
                expr_type.convert(proxy.handle, expr.eval(initial, last, proxy)?)
            }
            Self::Binary(expr_type, op, ref lhs, ref rhs) => {
                let lhs = lhs.eval(initial, last, proxy)?;
                let rhs = rhs.eval(initial, last, proxy)?;

                let value = match op.apply(lhs, rhs)? {
                    Some(value) => value,
                    None => return Ok(Value::None),
                };

                expr_type.convert(proxy.handle, value)
            }
        })
    }

    pub fn address_of(
        &self,
        initial: ValueRef<'_>,
        last: ValueRef<'_>,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Option<Address>> {
        let mut stack = Vec::new();
        self.stacked_address_of(&mut stack, initial, last, proxy)?;
        Ok(stack.last().copied().and_then(|a| a))
    }

    /// Get the address of a value expression.
    pub fn stacked_address_of(
        &self,
        stack: &mut Vec<Option<Address>>,
        initial: ValueRef<'_>,
        last: ValueRef<'_>,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<()> {
        match self {
            Self::Value(..) => {
                stack.push(proxy.address()?);
            }
            Self::Deref(_, value) => {
                let value = value.eval(initial, last, proxy)?;
                stack.push(value.as_address());
            }
            Self::AddressOf(_, value) => {
                value.stacked_address_of(stack, initial, last, proxy)?;
                stack.pop();
            }
            other => bail!("cannot get address of: {:?}", other),
        }

        Ok(())
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
        type_hint: Option<Type>,
    },
    /// A decimal literal.
    #[serde(rename = "decimal")]
    Decimal {
        value: BigDecimal,
        type_hint: Option<Type>,
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
        cast_type: Type,
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

    /// Get the type of the expression.
    pub fn type_of(
        &self,
        initial_type: TypeHint,
        last_type: TypeHint,
        value_type: TypeHint,
        default_hint: TypeHint,
    ) -> anyhow::Result<TypeHint> {
        use self::TypeHint::*;

        Ok(match *self {
            Self::Initial => initial_type.unwrap_or(default_hint),
            Self::Last => last_type.unwrap_or(default_hint),
            Self::Value => value_type.unwrap_or(default_hint),
            Self::Deref { .. } => default_hint,
            Self::AddressOf { .. } => Explicit(Type::Pointer),
            Self::Binary {
                op,
                ref lhs,
                ref rhs,
            } => {
                let lhs = lhs.type_of(initial_type, last_type, value_type, default_hint)?;
                let rhs = rhs.type_of(initial_type, last_type, value_type, default_hint)?;

                match (lhs, rhs) {
                    (Explicit(lhs), Explicit(rhs)) => {
                        if lhs != rhs {
                            bail!("incompatible types in expression: {} {} {}", lhs, op, rhs)
                        }

                        Explicit(lhs)
                    }
                    (Explicit(ty), _) | (_, Explicit(ty)) => Explicit(ty),
                    (Implicit(ty), _) | (_, Implicit(ty)) => Implicit(ty),
                    _ => default_hint,
                }
            }
            Self::Cast { cast_type, .. } => Explicit(cast_type),
            Self::Number {
                type_hint: Some(type_hint),
                ..
            } => Explicit(type_hint),
            Self::Number {
                type_hint: None, ..
            } => default_hint.or_implicit(Type::U32),
            Self::Decimal {
                type_hint: Some(type_hint),
                ..
            } => Explicit(type_hint),
            Self::Decimal {
                type_hint: None, ..
            } => default_hint.or_implicit(Type::F32),
            Self::String { .. } => default_hint.or_implicit(Type::String(Default::default())),
            Self::Bytes { ref value } => default_hint.or_implicit(Type::Bytes(Some(value.len()))),
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
                    other.type_of(NoHint, NoHint, NoHint, NoHint)?
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
            Self::Cast {
                ref expr,
                cast_type: inner_type,
            } => match &**expr {
                Self::Value => Explicit(inner_type),
                ref other => other.value_type_of(Explicit(inner_type))?,
            },
            _ => NoHint,
        })
    }

    /// Evaluate the expression to return a value.
    pub fn type_check<'a>(
        &'a self,
        initial_type: Type,
        last_type: Type,
        value_type: Type,
        expr_type: Type,
    ) -> anyhow::Result<TypedValueExpr<'a>> {
        use self::TypeHint::*;

        Ok(match *self {
            Self::Value => TypedValueExpr::Value(expr_type),
            Self::Initial => TypedValueExpr::Initial(expr_type),
            Self::Last => TypedValueExpr::Last(expr_type),
            Self::Number { ref value, .. } => TypedValueExpr::Number(expr_type, value),
            Self::Decimal { ref value, .. } => TypedValueExpr::Decimal(expr_type, value),
            Self::String { ref value } => TypedValueExpr::String(expr_type, value),
            Self::Bytes { ref value } => TypedValueExpr::Bytes(expr_type, value),
            Self::Deref { ref value } => {
                let value = value.type_check(initial_type, last_type, value_type, Type::Pointer)?;
                TypedValueExpr::Deref(expr_type, Box::new(value))
            }
            Self::AddressOf { ref value } => {
                let value = value.type_check(initial_type, last_type, value_type, Type::Pointer)?;
                TypedValueExpr::AddressOf(expr_type, Box::new(value))
            }
            Self::Cast {
                ref expr,
                cast_type,
            } => {
                let inner_type = expr
                    .type_of(
                        Explicit(initial_type),
                        Explicit(last_type),
                        Explicit(value_type),
                        Explicit(cast_type),
                    )?
                    .ok_or_else(|| anyhow!("cannot determine type of expression: {}", expr))?;

                let expr = expr.type_check(initial_type, last_type, value_type, inner_type)?;
                TypedValueExpr::Cast(expr_type, Box::new(expr))
            }
            Self::Binary {
                op,
                ref lhs,
                ref rhs,
            } => {
                let lhs = lhs.type_check(initial_type, last_type, value_type, expr_type)?;
                let rhs = rhs.type_check(initial_type, last_type, value_type, expr_type)?;
                TypedValueExpr::Binary(expr_type, op, Box::new(lhs), Box::new(rhs))
            }
        })
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
            Self::Deref { value } => match **value {
                ref binary @ Self::Binary { .. } => write!(fmt, "*({})", binary),
                ref other => write!(fmt, "*{}", other),
            },
            Self::AddressOf { value } => match **value {
                ref binary @ Self::Binary { .. } => write!(fmt, "&({})", binary),
                ref other => write!(fmt, "&{}", other),
            },
            Self::Binary { op, lhs, rhs } => write!(fmt, "({}) {} ({})", lhs, op, rhs),
            Self::Number {
                value,
                type_hint: None,
            } => write!(fmt, "{}", value),
            Self::Number {
                value,
                type_hint: Some(ty),
            } => write!(fmt, "{}{}", value, ty),
            Self::Decimal {
                value,
                type_hint: None,
            } => write!(fmt, "{}", value),
            Self::Decimal {
                value,
                type_hint: Some(ty),
            } => write!(fmt, "{}{}", value, ty),
            Self::String { value, .. } => write!(fmt, "{}", EscapeString(value)),
            Self::Bytes { value } => write!(fmt, "{}", Hex(value)),
            Self::Cast { expr, cast_type } => match **expr {
                ref expr @ Self::Binary { .. } => write!(fmt, "({}) as {}", expr, cast_type),
                ref expr => write!(fmt, "{} as {}", expr, cast_type),
            },
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
