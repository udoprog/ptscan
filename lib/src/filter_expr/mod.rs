use crate::{
    error::Error, process::MemoryInformation, value, AddressProxy, AddressRange, ProcessInfo, Sign,
    Type, TypeHint, Value, ValueExpr,
};
use anyhow::bail;
use hashbrown::HashSet;
use std::fmt;

use self::ast::ValueTrait;
pub use self::filter_op::FilterOp;
pub use self::special::Special;

pub mod ast;
mod filter_op;
pub(crate) mod lexer;
pub mod special;
lalrpop_util::lalrpop_mod!(#[allow(clippy::all)] pub parser, "/filter_expr/parser.rs");

/// The result of a filter test.
#[derive(Debug, Clone, Copy)]
pub enum Test {
    True,
    False,
    Undefined,
}

impl Test {
    /// Invert the truth value of the test result.
    pub fn invert(self) -> Self {
        match self {
            Self::True => Self::False,
            Self::False => Self::True,
            Self::Undefined => Self::Undefined,
        }
    }
}

impl From<bool> for Test {
    fn from(value: bool) -> Test {
        if value {
            Test::True
        } else {
            Test::False
        }
    }
}

#[derive(Debug, Clone)]
pub enum FilterExpr {
    Binary(Binary),
    All(All),
    Any(Any),
    IsPointer(IsPointer),
    IsType(IsType),
    IsNan(IsNan),
    Not(Not),
    Regex(Regex),
}

impl FilterExpr {
    /// Parse a a string into a filter.
    pub fn parse(input: &str, process: &impl ProcessInfo) -> anyhow::Result<Self> {
        let expr = parser::FilterExprParser::new().parse(lexer::Lexer::new(input))?;
        Ok(expr.into_filter(process)?)
    }

    pub fn pointer(expr: ValueExpr, process: &impl ProcessInfo) -> anyhow::Result<Self> {
        Ok(Self::IsPointer(IsPointer::new(expr, process)?))
    }

    pub fn value_type_of(&self, cast_type: TypeHint) -> anyhow::Result<TypeHint> {
        match self {
            Self::Binary(m) => m.value_type_of(cast_type),
            Self::All(m) => m.value_type_of(cast_type),
            Self::Any(m) => m.value_type_of(cast_type),
            Self::IsPointer(m) => m.value_type_of(cast_type),
            Self::IsType(m) => m.value_type_of(cast_type),
            Self::IsNan(m) => m.value_type_of(cast_type),
            Self::Not(m) => m.value_type_of(cast_type),
            Self::Regex(m) => m.value_type_of(cast_type),
        }
    }

    /// Test the specified memory region.
    pub fn test(
        &self,
        initial: &Value,
        last: &Value,
        value_type: Type,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        match self {
            Self::Binary(m) => m.test(initial, last, value_type, proxy),
            Self::All(m) => m.test(initial, last, value_type, proxy),
            Self::Any(m) => m.test(initial, last, value_type, proxy),
            Self::IsPointer(m) => m.test(initial, last, value_type, proxy),
            Self::IsType(m) => m.test(initial, last, value_type, proxy),
            Self::IsNan(m) => m.test(initial, last, value_type, proxy),
            Self::Not(m) => m.test(initial, last, value_type, proxy),
            Self::Regex(m) => m.test(initial, last, value_type, proxy),
        }
    }

    /// Construct a special filter.
    pub fn special(
        &self,
        process: &impl ProcessInfo,
        value_type: Type,
    ) -> anyhow::Result<Option<Special>> {
        match self {
            Self::Binary(m) => m.special(process, value_type),
            Self::All(m) => m.special(process, value_type),
            Self::Any(m) => m.special(process, value_type),
            Self::IsPointer(..) => Ok(Some(Special::NonZero(value_type.size(process)))),
            Self::IsType(..) => Ok(None),
            Self::IsNan(is_nan) => is_nan.special(process, value_type),
            Self::Not(m) => m.special(process, value_type),
            Self::Regex(m) => m.special(),
        }
    }
}

impl fmt::Display for FilterExpr {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary(m) => m.fmt(fmt),
            Self::All(m) => m.fmt(fmt),
            Self::Any(m) => m.fmt(fmt),
            Self::IsPointer(m) => m.fmt(fmt),
            Self::IsType(m) => m.fmt(fmt),
            Self::IsNan(m) => m.fmt(fmt),
            Self::Not(m) => m.fmt(fmt),
            Self::Regex(m) => m.fmt(fmt),
        }
    }
}

/// Only match values which are exactly equal.
#[derive(Debug, Clone)]
pub struct Binary(FilterOp, ValueExpr, ValueExpr);

impl Binary {
    fn value_type_of(&self, cast_type: TypeHint) -> anyhow::Result<TypeHint> {
        use self::TypeHint::*;

        // comparison including value.
        let Binary(op, lhs, rhs) = self;

        let (lhs, rhs) = match (lhs, rhs) {
            (ValueExpr::Value, rhs) => {
                let lhs = lhs.value_type_of(cast_type)?;
                let rhs = rhs.type_of(NoHint, NoHint, cast_type, NoHint)?;
                (lhs, rhs)
            }
            (lhs, ValueExpr::Value) => {
                let lhs = lhs.type_of(NoHint, NoHint, cast_type, NoHint)?;
                let rhs = rhs.value_type_of(cast_type)?;
                (lhs, rhs)
            }
            (lhs, rhs) => {
                let lhs = lhs.value_type_of(cast_type)?;
                let rhs = rhs.value_type_of(cast_type)?;
                (lhs, rhs)
            }
        };

        // explicit value type
        Ok(match (lhs, rhs) {
            (Explicit(lhs), Explicit(rhs)) => {
                if lhs != rhs {
                    bail!("incompatible value in expression: {} {} {}", lhs, op, rhs)
                }

                Explicit(lhs)
            }
            (result, NoHint) | (NoHint, result) => result,
            _ => NoHint,
        })
    }

    fn test(
        &self,
        initial: &Value,
        last: &Value,
        value_type: Type,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        use self::FilterOp::*;
        use self::TypeHint::*;

        macro_rules! binary {
            ($expr_a:expr, $expr_b:expr, $a:ident $op:tt $b:ident) => {
                match ($expr_a, $expr_b) {
                    (Value::None(..), _) => return Ok(Test::Undefined),
                    (_, Value::None(..)) => return Ok(Test::Undefined),
                    (Value::Pointer($a), Value::Pointer($b)) => $a $op $b,
                    (Value::U8($a), Value::U8($b)) => $a $op $b,
                    (Value::I8($a), Value::I8($b)) => $a $op $b,
                    (Value::U16($a), Value::U16($b)) => $a $op $b,
                    (Value::I16($a), Value::I16($b)) => $a $op $b,
                    (Value::U32($a), Value::U32($b)) => $a $op $b,
                    (Value::I32($a), Value::I32($b)) => $a $op $b,
                    (Value::U64($a), Value::U64($b)) => $a $op $b,
                    (Value::I64($a), Value::I64($b)) => $a $op $b,
                    (Value::U128($a), Value::U128($b)) => $a $op $b,
                    (Value::I128($a), Value::I128($b)) => $a $op $b,
                    (Value::String(_, $a), Value::String(_, $b)) => $a $op $b,
                    (Value::Bytes($a), Value::Bytes($b)) => $a $op $b,
                    _ => false,
                }
            };
        }

        let Binary(op, lhs, rhs) = self;

        let initial_type = initial.ty();
        let last_type = last.ty();

        let lhs_type = lhs.type_of(
            Explicit(initial_type),
            Explicit(last_type),
            Explicit(value_type),
            TypeHint::NoHint,
        )?;

        let rhs_type = rhs.type_of(
            Explicit(initial_type),
            Explicit(last_type),
            Explicit(value_type),
            TypeHint::NoHint,
        )?;

        let expr_type = match (lhs_type, rhs_type) {
            (Explicit(lhs), Explicit(rhs)) => {
                if lhs != rhs {
                    bail!("incompatible types in expression: {} {} {}", lhs, op, rhs)
                }

                lhs
            }
            (Explicit(ty), _) | (_, Explicit(ty)) => ty,
            (Implicit(ty), _) | (_, Implicit(ty)) => ty,
            _ => return Err(Error::BinaryTypeInference(self.clone()).into()),
        };

        let lhs = lhs.type_check(initial_type, last_type, value_type, expr_type)?;
        let rhs = rhs.type_check(initial_type, last_type, value_type, expr_type)?;

        let lhs = lhs.eval(initial, last, proxy)?;
        let rhs = rhs.eval(initial, last, proxy)?;

        // NB: we treat 'none' specially:
        // The only allowed match on none is to compare against something which is strictly not equal to it, like value != none.
        let result = match op {
            Eq => binary!(lhs, rhs, a == b),
            Neq => binary!(lhs, rhs, a != b),
            Lt => binary!(lhs, rhs, a < b),
            Lte => binary!(lhs, rhs, a <= b),
            Gt => binary!(lhs, rhs, a > b),
            Gte => binary!(lhs, rhs, a >= b),
            StartsWith => match lhs {
                Value::String(_, lhs) => match rhs {
                    Value::None(..) => return Ok(Test::Undefined),
                    Value::String(_, rhs) => lhs.starts_with(&rhs),
                    Value::Bytes(rhs) => lhs.as_bytes().starts_with(&rhs),
                    _ => false,
                },
                Value::Bytes(lhs) => match rhs {
                    Value::None(..) => return Ok(Test::Undefined),
                    Value::String(_, rhs) => lhs.starts_with(rhs.as_bytes()),
                    Value::Bytes(rhs) => lhs.starts_with(&rhs),
                    _ => false,
                },
                Value::None(..) => return Ok(Test::Undefined),
                _ => false,
            },
        };

        Ok(result.into())
    }

    fn special(
        &self,
        process: &impl ProcessInfo,
        value_type: Type,
    ) -> anyhow::Result<Option<Special>> {
        use self::FilterOp::*;
        use self::Sign::*;
        use self::ValueExpr::*;
        use self::ValueTrait::*;

        let self::Binary(op, lhs, rhs) = self;

        let exact = match (op, lhs.reduced(), rhs.reduced()) {
            (Eq, Number { value, .. }, Value) | (Eq, Value, Number { value, .. }) => {
                Some(value::Value::from_bigint(value_type, value)?)
            }
            (Eq, Decimal { value, .. }, Value) | (Eq, Value, Decimal { value, .. }) => {
                Some(value::Value::from_bigdecimal(value_type, value)?)
            }
            (Eq, String { encoding, value }, Value) | (Eq, Value, String { encoding, value }) => {
                Some(value::Value::String(*encoding, value.to_owned()))
            }
            (Eq, Bytes { value }, Value) | (Eq, Value, Bytes { value }) => {
                Some(value::Value::Bytes(value.clone()))
            }
            (StartsWith, Value, String { encoding, value }) => {
                Some(value::Value::String(*encoding, value.to_owned()))
            }
            (StartsWith, Value, Bytes { value }) => Some(value::Value::Bytes(value.clone())),
            _ => None,
        };

        if let Some(value) = exact {
            if let Some(width) = value.size(process) {
                let mut buf = vec![0u8; width];
                value.encode(process, &mut buf)?;

                if buf.iter().all(|c| *c == 0) {
                    return Ok(Some(Special::Zero(width)));
                } else {
                    return Ok(Some(Special::Bytes(buf)));
                }
            }
        }

        // expressions that only matches non-zeroed regions.
        let non_zero = match (op, lhs.traits(), rhs.traits()) {
            // any derefs must be done on a non-zero value.
            (_, (IsDeref, _), _) | (_, _, (IsDeref, _)) => true,
            // value equals something that is non-zero.
            (Eq, (IsValue, _), (NonZero, _)) | (Eq, (NonZero, _), (IsValue, _)) => true,
            // value does not equal to zero.
            (Neq, (IsValue, _), (Zero, _)) | (Neq, (Zero, _), (IsValue, _)) => true,
            // value is greater than non-zero positive.
            (Gte, (IsValue, _), (NonZero, Plus)) => true,
            // value is less than non-zero negative.
            (Lte, (IsValue, _), (NonZero, Minus)) => true,
            // less than zero.
            (Lt, (IsValue, _), (NonZero, Minus)) => true,
            (Lt, (IsValue, _), (Zero, _)) => true,
            // greater than zero.
            (Gt, (IsValue, _), (NonZero, Plus)) => true,
            (Gt, (IsValue, _), (Zero, _)) => true,
            _ => false,
        };

        if non_zero {
            return Ok(Some(Special::NonZero(value_type.size(process))));
        }

        Ok(None)
    }
}

impl fmt::Display for Binary {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{1} {0} {2}", self.0, self.1, self.2)
    }
}

/// Only matches when all nested filters match.
#[derive(Debug, Clone)]
pub struct All(Vec<FilterExpr>);

impl All {
    fn value_type_of(&self, cast_type: TypeHint) -> anyhow::Result<TypeHint> {
        collection_value_type_of(self, &self.0, cast_type)
    }

    pub fn new(filters: impl IntoIterator<Item = FilterExpr>) -> Self {
        All(filters.into_iter().collect())
    }

    pub fn special(
        &self,
        process: &impl ProcessInfo,
        value_type: Type,
    ) -> anyhow::Result<Option<Special>> {
        let mut all = Vec::new();

        for e in &self.0 {
            if let Some(special) = e.special(process, value_type)? {
                all.push(special);
            }
        }

        if all.len() > 1 {
            return Ok(Some(Special::All(all)));
        }

        Ok(all.into_iter().next())
    }

    fn test(
        &self,
        initial: &Value,
        last: &Value,
        value_type: Type,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        for m in &self.0 {
            match m.test(initial, last, value_type, proxy)? {
                Test::False => return Ok(Test::False),
                Test::Undefined => return Ok(Test::Undefined),
                _ => (),
            }
        }

        Ok(Test::True)
    }
}

impl fmt::Display for All {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut it = self.0.iter().peekable();

        while let Some(p) = it.next() {
            fmt::Display::fmt(p, fmt)?;

            if it.peek().is_some() {
                write!(fmt, " and ")?;
            }
        }

        Ok(())
    }
}

/// Only matches when any nested filter match.
#[derive(Debug, Clone)]
pub struct Any(Vec<FilterExpr>);

impl Any {
    fn value_type_of(&self, cast_type: TypeHint) -> anyhow::Result<TypeHint> {
        collection_value_type_of(self, &self.0, cast_type)
    }

    pub fn new(filters: impl IntoIterator<Item = FilterExpr>) -> Self {
        Any(filters.into_iter().collect())
    }

    pub fn special(
        &self,
        process: &impl ProcessInfo,
        value_type: Type,
    ) -> anyhow::Result<Option<Special>> {
        let mut any = Vec::new();

        for e in &self.0 {
            match e.special(process, value_type)? {
                Some(special) => any.push(special),
                None => return Ok(None),
            }
        }

        if any.len() > 1 {
            return Ok(Some(Special::Any(any)));
        }

        Ok(any.into_iter().next())
    }

    fn test(
        &self,
        initial: &Value,
        last: &Value,
        value_type: Type,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        for m in &self.0 {
            match m.test(initial, last, value_type, proxy)? {
                Test::True => return Ok(Test::True),
                Test::Undefined => return Ok(Test::Undefined),
                _ => (),
            }
        }

        Ok(Test::False)
    }
}

impl fmt::Display for Any {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut it = self.0.iter().peekable();

        while let Some(p) = it.next() {
            write!(fmt, "{}", p)?;

            if it.peek().is_some() {
                write!(fmt, " or ")?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct IsPointer {
    expr: ValueExpr,
    /// Sorted memory regions.
    memory_regions: Vec<MemoryInformation>,
}

impl IsPointer {
    pub fn new(expr: ValueExpr, process: &impl ProcessInfo) -> anyhow::Result<Self> {
        use crate::utils::IteratorExtension;

        let mut memory_regions = process
            .virtual_memory_regions()
            .only_relevant()
            .collect::<Result<Vec<_>, _>>()?;

        memory_regions.sort_by_key(|m| m.range.base);

        Ok(Self {
            expr,
            memory_regions,
        })
    }

    fn value_type_of(&self, _: TypeHint) -> anyhow::Result<TypeHint> {
        self.expr.value_type_of(TypeHint::Explicit(Type::Pointer))
    }

    pub fn test(
        &self,
        initial: &Value,
        last: &Value,
        value_type: Type,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        let initial_type = initial.ty();
        let last_type = last.ty();

        let expr_type = self
            .expr
            .type_of(
                TypeHint::Explicit(initial_type),
                TypeHint::Explicit(last_type),
                TypeHint::Explicit(value_type),
                TypeHint::NoHint,
            )?
            .ok_or_else(|| Error::TypeInference(self.expr.clone()))?;

        let value = self
            .expr
            .type_check(initial_type, last_type, value_type, expr_type)?
            .eval(initial, last, proxy)?;

        let address = match value {
            Value::Pointer(address) => address,
            _ => return Ok(Test::False),
        };

        if address.is_null() {
            return Ok(Test::False);
        }

        Ok(
            AddressRange::find_in_range(&self.memory_regions, |m| &m.range, address)
                .is_some()
                .into(),
        )
    }
}

impl fmt::Display for IsPointer {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "is pointer")
    }
}

#[derive(Debug, Clone)]
pub struct IsType {
    expr: ValueExpr,
    ty: Type,
}

impl IsType {
    pub fn new(expr: ValueExpr, ty: Type) -> anyhow::Result<Self> {
        Ok(Self { expr, ty })
    }

    fn value_type_of(&self, _: TypeHint) -> anyhow::Result<TypeHint> {
        self.expr.value_type_of(TypeHint::Explicit(self.ty))
    }

    pub fn test(
        &self,
        initial: &Value,
        last: &Value,
        value_type: Type,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        let initial_type = initial.ty();
        let last_type = last.ty();

        let expr_type = self
            .expr
            .type_of(
                TypeHint::Explicit(initial_type),
                TypeHint::Explicit(last_type),
                TypeHint::Explicit(value_type),
                TypeHint::NoHint,
            )?
            .ok_or_else(|| Error::TypeInference(self.expr.clone()))?;

        let value = self
            .expr
            .type_check(initial_type, last_type, value_type, expr_type)?
            .eval(initial, last, proxy)?;

        if value.is_none() {
            return Ok(Test::Undefined);
        }

        Ok((value.ty() == self.ty).into())
    }
}

impl fmt::Display for IsType {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "is {}", self.ty)
    }
}

#[derive(Debug, Clone)]
pub struct IsNan {
    expr: ValueExpr,
}

impl IsNan {
    pub fn new(expr: ValueExpr) -> anyhow::Result<Self> {
        Ok(Self { expr })
    }

    pub fn special(
        &self,
        process: &impl ProcessInfo,
        value_type: Type,
    ) -> anyhow::Result<Option<Special>> {
        Ok(Some(Special::NonZero(value_type.size(process))))
    }

    fn value_type_of(&self, _: TypeHint) -> anyhow::Result<TypeHint> {
        self.expr.value_type_of(TypeHint::Implicit(Type::F32))
    }

    pub fn test(
        &self,
        initial: &Value,
        last: &Value,
        value_type: Type,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        let initial_type = initial.ty();
        let last_type = last.ty();

        let expr_type = self
            .expr
            .type_of(
                TypeHint::Explicit(initial_type),
                TypeHint::Explicit(last_type),
                TypeHint::Explicit(value_type),
                TypeHint::NoHint,
            )?
            .ok_or_else(|| Error::TypeInference(self.expr.clone()))?;

        let value = self
            .expr
            .type_check(initial_type, last_type, value_type, expr_type)?
            .eval(initial, last, proxy)?;

        let value = match value {
            Value::F32(value) => value.is_nan(),
            Value::F64(value) => value.is_nan(),
            _ => false,
        };

        Ok(value.into())
    }
}

impl fmt::Display for IsNan {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "is nan")
    }
}

#[derive(Debug, Clone)]
pub struct Regex {
    expr: ValueExpr,
    regex: regex::bytes::Regex,
}

impl Regex {
    pub fn new(expr: ValueExpr, regex: regex::bytes::Regex) -> Self {
        Self { expr, regex }
    }

    fn value_type_of(&self, _: TypeHint) -> anyhow::Result<TypeHint> {
        self.expr
            .value_type_of(TypeHint::Implicit(Type::String(Default::default())))
    }

    pub fn test(
        &self,
        initial: &Value,
        last: &Value,
        value_type: Type,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        let initial_type = initial.ty();
        let last_type = last.ty();

        let expr_type = self
            .expr
            .type_of(
                TypeHint::Explicit(initial_type),
                TypeHint::Explicit(last_type),
                TypeHint::Explicit(value_type),
                TypeHint::NoHint,
            )?
            .ok_or_else(|| Error::TypeInference(self.expr.clone()))?;

        let value = self
            .expr
            .type_check(initial_type, last_type, value_type, expr_type)?
            .eval(initial, last, proxy)?;

        let bytes = match &value {
            Value::Bytes(bytes) => &bytes[..],
            Value::String(_, s, ..) => s.as_bytes(),
            Value::None(..) => return Ok(Test::Undefined),
            _ => return Ok(Test::False),
        };

        Ok(self.regex.is_match(bytes).into())
    }

    pub fn special(&self) -> anyhow::Result<Option<Special>> {
        if !self.regex.as_str().starts_with('^') && !self.regex.as_str().ends_with('$') {
            return Ok(Some(Special::Regex(self.regex.clone())));
        }

        let mut regex = self.regex.as_str();

        if regex.starts_with('^') {
            regex = &regex[1..];
        }

        if regex.ends_with('$') && !regex.ends_with("\\$") {
            regex = &regex[..regex.len() - 1];
        }

        if let Ok(regex) = regex::bytes::Regex::new(regex) {
            return Ok(Some(Special::Regex(regex)));
        }

        match self.expr.reduced() {
            ValueExpr::Value => Ok(Some(Special::NonZero(None))),
            _ => Ok(None),
        }
    }
}

impl fmt::Display for Regex {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "is none")
    }
}

#[derive(Debug, Clone)]
pub struct Not {
    filter: Box<FilterExpr>,
}

impl Not {
    fn test(
        &self,
        initial: &Value,
        last: &Value,
        value_type: Type,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        Ok(self.filter.test(initial, last, value_type, proxy)?.invert())
    }

    fn value_type_of(&self, cast_type: TypeHint) -> anyhow::Result<TypeHint> {
        self.filter.value_type_of(cast_type)
    }

    pub fn special(
        &self,
        process: &impl ProcessInfo,
        value_type: Type,
    ) -> anyhow::Result<Option<Special>> {
        // erase or fix specializations produced.
        let special = match self.filter.special(process, value_type)? {
            Some(special) => special,
            None => return Ok(None),
        };

        Ok(match special {
            Special::Bytes(bytes) => Some(Special::NotBytes(bytes)),
            Special::NotBytes(bytes) => Some(Special::Bytes(bytes)),
            // nb non-exact matches are unfortunately erased.
            // Consider pointer scanning. The specialization looks for non-zero
            // addresses and then check if those point to a valid memory region.
            //
            // To invert this check, we need to check _all_ addresses.
            _ => None,
        })
    }
}

impl fmt::Display for Not {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.filter {
            FilterExpr::IsPointer(..) => write!(fmt, "is not pointer"),
            FilterExpr::IsType(is_type) => write!(fmt, "is not {}", is_type.ty),
            FilterExpr::IsNan(..) => write!(fmt, "is not nan"),
            other => write!(fmt, "not {}", other),
        }
    }
}

/// Helper to figure out the value type of a collection of expressions.
fn collection_value_type_of(
    expr: &impl fmt::Display,
    exprs: &[FilterExpr],
    cast_type: TypeHint,
) -> anyhow::Result<TypeHint> {
    use self::TypeHint::*;

    let mut explicits = HashSet::new();
    let mut implicits = HashSet::new();

    for v in exprs {
        match v.value_type_of(cast_type)? {
            Explicit(ty) => {
                explicits.insert(ty);
            }
            Implicit(ty) => {
                implicits.insert(ty);
            }
            _ => (),
        }
    }

    if explicits.len() > 2 {
        bail!(
            "found multiple conflicting explicit types {:?} in expression: {}",
            explicits,
            expr
        );
    }

    if let Some(ty) = explicits.into_iter().next() {
        return Ok(Explicit(ty));
    }

    if implicits.len() > 2 {
        bail!(
            "found multiple conflicting implicit types {:?} in expression: {}",
            implicits,
            expr
        );
    }

    if let Some(ty) = implicits.into_iter().next() {
        return Ok(Implicit(ty));
    }

    Ok(NoHint)
}

#[cfg(test)]
mod tests {
    use super::{ast::FilterExpr as F, lexer, parser, FilterOp, Special};
    use crate::{
        error::Error, process::MemoryInformation, value_expr::ast::ValueExpr as V, Address,
        ProcessInfo, Type,
    };

    struct FakeProcess {
        pointer_width: usize,
    }

    impl FakeProcess {
        pub fn new() -> Self {
            Self { pointer_width: 8 }
        }
    }

    impl ProcessInfo for FakeProcess {
        type ByteOrder = byteorder::NativeEndian;

        fn pointer_width(&self) -> usize {
            self.pointer_width
        }

        fn virtual_query(&self, _: Address) -> Result<Option<MemoryInformation>, Error> {
            Ok(None)
        }
    }

    macro_rules! test {
        ($s:expr, expr = $expr:expr $(, special = ($special:pat, $value_type:expr))?) => {{
            let e = parse($s)?;
            assert_eq!($expr, e);
            $(
                let process = FakeProcess::new();
                let e = e.into_filter(&process)?;
                let special = e.special(&process, $value_type)?;

                match special {
                    $special  => (),
                    special => {
                        panic!("special doesn't match: {:?} != {} (expected)", special, stringify!($special));
                    }
                }
            )?
        }};
    }

    fn parse(input: &str) -> anyhow::Result<F> {
        Ok(parser::FilterExprParser::new().parse(lexer::Lexer::new(input))?)
    }

    #[test]
    fn test_basic_parsing() -> anyhow::Result<()> {
        let parser = parser::FilterExprParser::new();
        let a = parser.parse(lexer::Lexer::new("value == 1"))?;
        let b = parser.parse(lexer::Lexer::new("value == 1 and value == 2"))?;
        let c = parser.parse(lexer::Lexer::new("value == 1 and value == 2 or value == 3"))?;

        dbg!(a);
        dbg!(b);
        dbg!(c);

        Ok(())
    }

    #[test]
    fn test_equality() -> anyhow::Result<()> {
        use self::FilterOp::*;
        test! {
            "value == 42",
            expr = F::Binary(Eq, V::Value, V::Number(42.into(), None)),
            special = (Some(Special::Bytes(..)), Type::U32)
        };
        test! {
            "value != 0",
            expr = F::Binary(Neq, V::Value, V::Number(0.into(), None)),
            special = (Some(Special::NonZero(..)), Type::U32)
        };
        test! {
            "value < 0",
            expr = F::Binary(Lt, V::Value, V::Number(0.into(), None)),
            special = (Some(Special::NonZero(..)), Type::U32)
        };
        test! {
            "value > 0",
            expr = F::Binary(Gt, V::Value, V::Number(0.into(), None)),
            special = (Some(Special::NonZero(..)), Type::U32)
        };
        test! {
            "value >= 1",
            expr = F::Binary(Gte, V::Value, V::Number(1.into(), None)),
            special = (Some(Special::NonZero(..)), Type::U32)
        };
        test! {
            "value <= -1",
            expr = F::Binary(Lte, V::Value, V::Number((-1).into(), None)),
            special = (Some(Special::NonZero(..)), Type::U32)
        };
        test! {
            "value <= 0",
            expr = F::Binary(Lte, V::Value, V::Number(0.into(), None)),
            special = (None, Type::U32)
        };
        Ok(())
    }

    #[test]
    fn test_regex() -> anyhow::Result<()> {
        test! {
            "value ~ \"foobar\"",
            expr = F::Regex(V::Value, V::String(Default::default(), "foobar".into())),
            special = (Some(Special::Regex(..)), Type::String(Default::default()))
        };
        test! {
            "value ~ \"^foobar\"",
            expr = F::Regex(V::Value, V::String(Default::default(), "^foobar".into())),
            special = (Some(Special::Regex(..)), Type::String(Default::default()))
        };
        test! {
            "value ~ \"^foobar\\\\$\"",
            expr = F::Regex(V::Value, V::String(Default::default(), "^foobar\\$".into())),
            special = (Some(Special::Regex(..)), Type::String(Default::default()))
        };
        test! {
            "value !~ \"foobar\"",
            expr = F::Not(Box::new(F::Regex(V::Value, V::String(Default::default(), "foobar".into())))),
            special = (None, Type::String(Default::default()))
        };
        Ok(())
    }
}
