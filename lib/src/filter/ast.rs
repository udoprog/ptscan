use crate::{filter, value, Type};
use num_bigint::BigInt;

#[derive(Debug)]
pub enum Value {
    Number(BigInt),
    String(String),
}

#[derive(Debug)]
pub enum Expression {
    /// A value that stays the same.
    Same,
    /// A value that has changed.
    Changed,
    /// A value that has increased.
    Inc,
    /// A value that has decreased.
    Dec,
    /// Test that the value equals the expected value.
    Eq(Value),
    /// Test that the value is not equal to the given value.
    Neq(Value),
    /// Test that a value is less than or equal to another value.
    Lte(Value),
    /// Test that a value is greater than or equal to another value.
    Gte(Value),
    /// Test that a value is less than another value.
    Lt(Value),
    /// Test that a value is greater than another value.
    Gt(Value),
    /// Multiple expressions and:ed together.
    And(Vec<Expression>),
    /// Multiple expressions or:ed together.
    Or(Vec<Expression>),
}

impl Expression {
    /// Convert expression into a matcher.
    pub fn into_matcher(self, ty: Type) -> anyhow::Result<Box<dyn filter::Matcher>> {
        use self::Expression::*;

        let matcher: Box<dyn filter::Matcher> = match self {
            Same => Box::new(filter::Same),
            Changed => Box::new(filter::Changed),
            Inc => Box::new(filter::Inc),
            Dec => Box::new(filter::Dec),
            Eq(value) => {
                let value = value::Value::from_ast(value, ty)?;
                Box::new(filter::Eq(value))
            }
            Neq(value) => {
                let value = value::Value::from_ast(value, ty)?;
                Box::new(filter::Neq(value))
            }
            Lte(value) => {
                let value = value::Value::from_ast(value, ty)?;
                Box::new(filter::Lte(value))
            }
            Gte(value) => {
                let value = value::Value::from_ast(value, ty)?;
                Box::new(filter::Gte(value))
            }
            Lt(value) => {
                let value = value::Value::from_ast(value, ty)?;
                Box::new(filter::Lt(value))
            }
            Gt(value) => {
                let value = value::Value::from_ast(value, ty)?;
                Box::new(filter::Gt(value))
            }
            And(expressions) => {
                let filters = expressions
                    .into_iter()
                    .map(|e| e.into_matcher(ty))
                    .collect::<Result<Vec<_>, _>>()?;
                Box::new(filter::All::new(filters))
            }
            Or(expressions) => {
                let filters = expressions
                    .into_iter()
                    .map(|e| e.into_matcher(ty))
                    .collect::<Result<Vec<_>, _>>()?;
                Box::new(filter::Any::new(filters))
            }
        };

        Ok(matcher)
    }
}
