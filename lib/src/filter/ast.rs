use crate::{filter, scan::Value};

#[derive(Debug)]
pub enum Expression {
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
    /// Convert an expression into a filter.
    pub fn into_filter(self) -> Result<Box<dyn filter::Filter>, failure::Error> {
        use self::Expression::*;

        let p: Box<dyn filter::Filter> = match self {
            Eq(value) => Box::new(filter::Eq(value)),
            Neq(value) => Box::new(filter::Neq(value)),
            Lte(value) => Box::new(filter::Lte(value)),
            Gte(value) => Box::new(filter::Gte(value)),
            Lt(value) => Box::new(filter::Lt(value)),
            Gt(value) => Box::new(filter::Gt(value)),
            And(expressions) => {
                let filters = expressions
                    .into_iter()
                    .map(Expression::into_filter)
                    .collect::<Result<Vec<_>, _>>()?;
                Box::new(filter::All::new(filters)?)
            }
            Or(expressions) => {
                let filters = expressions
                    .into_iter()
                    .map(Expression::into_filter)
                    .collect::<Result<Vec<_>, _>>()?;
                Box::new(filter::Any::new(filters)?)
            }
        };

        Ok(p)
    }
}