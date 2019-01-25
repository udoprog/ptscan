use crate::{predicate, scan::Value};

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
    /// Convert an expression into a predicate.
    pub fn into_predicate(self) -> Result<Box<dyn predicate::Predicate>, failure::Error> {
        use self::Expression::*;

        let p: Box<dyn predicate::Predicate> = match self {
            Eq(value) => Box::new(predicate::Eq(value)),
            Neq(value) => Box::new(predicate::Not(predicate::Eq(value))),
            Lte(value) => Box::new(predicate::Lte(value)),
            Gte(value) => Box::new(predicate::Gte(value)),
            Lt(value) => Box::new(predicate::Lt(value)),
            Gt(value) => Box::new(predicate::Gt(value)),
            And(expressions) => {
                let predicates = expressions
                    .into_iter()
                    .map(Expression::into_predicate)
                    .collect::<Result<Vec<_>, _>>()?;
                Box::new(predicate::All::new(predicates)?)
            }
            Or(expressions) => {
                let predicates = expressions
                    .into_iter()
                    .map(Expression::into_predicate)
                    .collect::<Result<Vec<_>, _>>()?;
                Box::new(predicate::Any::new(predicates)?)
            }
        };

        Ok(p)
    }
}
