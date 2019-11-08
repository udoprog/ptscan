use self::ast::{Op, ValueTrait};
use crate::{
    process::{MemoryInformation, Process},
    value,
    value::Value,
    value_expr::ValueExpr,
    Address, AddressProxy, AddressRange, Type,
};
use anyhow::anyhow;
use num_bigint::Sign;
use std::fmt;

pub use self::special::Special;

pub mod ast;
pub(crate) mod lexer;
pub mod special;
lalrpop_util::lalrpop_mod!(pub parser, "/filter/parser.rs");

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
pub enum Filter {
    Binary(Binary),
    All(All),
    Any(Any),
    IsPointer(IsPointer),
    IsType(IsType),
    IsNan(IsNan),
    Not(Not),
    Regex(Regex),
}

impl Filter {
    /// Parse a a string into a filter.
    pub fn parse(input: &str, process: &Process) -> anyhow::Result<Self> {
        let expr = parser::OrParser::new().parse(lexer::Lexer::new(input))?;
        Ok(expr.into_filter(process)?)
    }

    pub fn pointer(expr: ValueExpr, process: &Process) -> anyhow::Result<Filter> {
        Ok(Filter::IsPointer(IsPointer::new(expr, process)?))
    }

    pub fn type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        match self {
            Self::Binary(m) => m.type_of(initial_type, last_type, value_type),
            Self::All(m) => m.type_of(initial_type, last_type, value_type),
            Self::Any(m) => m.type_of(initial_type, last_type, value_type),
            Self::IsPointer(m) => m.type_of(initial_type, last_type, value_type),
            Self::IsType(m) => m.type_of(initial_type, last_type, value_type),
            Self::IsNan(m) => m.type_of(initial_type, last_type, value_type),
            Self::Not(m) => m.type_of(initial_type, last_type, value_type),
            Self::Regex(m) => m.type_of(initial_type, last_type, value_type),
        }
    }

    pub fn value_type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        match self {
            Self::Binary(m) => m.value_type_of(initial_type, last_type, value_type),
            Self::All(m) => m.value_type_of(initial_type, last_type, value_type),
            Self::Any(m) => m.value_type_of(initial_type, last_type, value_type),
            Self::IsPointer(m) => m.value_type_of(initial_type, last_type, value_type),
            Self::IsType(m) => m.value_type_of(initial_type, last_type, value_type),
            Self::IsNan(m) => m.value_type_of(initial_type, last_type, value_type),
            Self::Not(m) => m.value_type_of(initial_type, last_type, value_type),
            Self::Regex(m) => m.value_type_of(initial_type, last_type, value_type),
        }
    }

    /// Test the specified memory region.
    pub fn test(
        &self,
        value_type: Type,
        initial: &Value,
        last: &Value,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        match self {
            Self::Binary(m) => m.test(value_type, initial, last, proxy),
            Self::All(m) => m.test(value_type, initial, last, proxy),
            Self::Any(m) => m.test(value_type, initial, last, proxy),
            Self::IsPointer(m) => m.test(value_type, initial, last, proxy),
            Self::IsType(m) => m.test(value_type, initial, last, proxy),
            Self::IsNan(m) => m.test(value_type, initial, last, proxy),
            Self::Not(m) => m.test(value_type, initial, last, proxy),
            Self::Regex(m) => m.test(value_type, initial, last, proxy),
        }
    }

    /// Construct a special filter.
    pub fn special(&self, process: &Process, value_type: Type) -> anyhow::Result<Option<Special>> {
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

impl fmt::Display for Filter {
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
pub struct Binary(Op, ValueExpr, ValueExpr);

impl Binary {
    fn type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        self.1
            .type_of(initial_type, last_type, value_type)
            .or_else(|| self.2.type_of(initial_type, last_type, value_type))
    }

    fn value_type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        self.1
            .value_type_of(initial_type, last_type, value_type)
            .or_else(|| self.2.value_type_of(initial_type, last_type, value_type))
    }

    fn test(
        &self,
        value_type: Type,
        initial: &Value,
        last: &Value,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        macro_rules! binary_numeric {
            ($expr:expr, $ty:ty, $a:ident $op:tt $b:ident) => {
                match $expr {
                    Value::None(..) => return Ok(Test::Undefined),
                    Value::Pointer(Address($b)) => $a $op ($b as $ty),
                    Value::U128($b) => $a $op ($b as $ty),
                    Value::U64($b) => $a $op ($b as $ty),
                    Value::U32($b) => $a $op ($b as $ty),
                    Value::U16($b) => $a $op ($b as $ty),
                    Value::U8($b) => $a $op ($b as $ty),
                    Value::I128($b) => $a $op ($b as $ty),
                    Value::I64($b) => $a $op ($b as $ty),
                    Value::I32($b) => $a $op ($b as $ty),
                    Value::I16($b) => $a $op ($b as $ty),
                    Value::I8($b) => $a $op ($b as $ty),
                    Value::F32($b) => $a $op ($b as $ty),
                    Value::F64($b) => $a $op ($b as $ty),
                    Value::String(..) => false,
                    Value::Bytes(..) => false,
                }
            }
        }

        macro_rules! binary {
            ($expr_a:expr, $expr_b:expr, $a:ident $op:tt $b:ident) => {
                match $expr_a {
                    Value::Pointer(Address($a)) => binary_numeric!($expr_b, u64, $a $op $b),
                    Value::U128($a) => binary_numeric!($expr_b, u128, $a $op $b),
                    Value::U64($a) => binary_numeric!($expr_b, u64, $a $op $b),
                    Value::U32($a) => binary_numeric!($expr_b, u32, $a $op $b),
                    Value::U16($a) => binary_numeric!($expr_b, u16, $a $op $b),
                    Value::U8($a) => binary_numeric!($expr_b, u8, $a $op $b),
                    Value::I128($a) => binary_numeric!($expr_b, i128, $a $op $b),
                    Value::I64($a) => binary_numeric!($expr_b, i64, $a $op $b),
                    Value::I32($a) => binary_numeric!($expr_b, i32, $a $op $b),
                    Value::I16($a) => binary_numeric!($expr_b, i16, $a $op $b),
                    Value::I8($a) => binary_numeric!($expr_b, i8, $a $op $b),
                    Value::F32($a) => binary_numeric!($expr_b, f32, $a $op $b),
                    Value::F64($a) => binary_numeric!($expr_b, f64, $a $op $b),
                    Value::String($a, ..) => match $expr_b {
                        Value::Bytes($b) => {
                            let len = usize::min($a.len(), $b.len());
                            let $a = &$a.as_bytes()[..len];
                            let $b = &$b[..len];
                            $a $op $b
                        },
                        Value::String($b, ..) => $a $op $b,
                        _ => false,
                    },
                    Value::Bytes($a) => match $expr_b {
                        Value::Bytes($b) => {
                            let len = usize::min($a.len(), $b.len());
                            let $a = &$a[..len];
                            let $b = &$b[..len];
                            $a $op $b
                        },
                        Value::String($b, ..) => {
                            let len = usize::min($a.len(), $b.len());
                            let $a = &$a[..len];
                            let $b = &$b.as_bytes()[..len];
                            $a $op $b
                        },
                        Value::None(..) => return Ok(Test::Undefined),
                        _ => false,
                    },
                    Value::None(..) => return Ok(Test::Undefined),
                }
            };
        }

        let Binary(op, lhs, rhs) = self;

        let expr_type = self
            .type_of(Some(initial.ty()), Some(last.ty()), Some(value_type))
            .ok_or_else(|| anyhow!("cannot determine type of binary expression: {}", self))?;

        let lhs = lhs.eval(expr_type, initial, last, proxy)?;
        let rhs = rhs.eval(expr_type, initial, last, proxy)?;

        // NB: we treat 'none' specially:
        // The only allowed match on none is to compare against something which is strictly not equal to it, like value != none.

        let result = match op {
            Op::Eq => binary!(lhs, rhs, a == b),
            Op::Neq => match (lhs, rhs) {
                (Value::None(..), other) | (other, Value::None(..)) if other.is_some() => true,
                (lhs, rhs) => binary!(lhs, rhs, a != b),
            },
            Op::Lt => binary!(lhs, rhs, a < b),
            Op::Lte => binary!(lhs, rhs, a <= b),
            Op::Gt => binary!(lhs, rhs, a > b),
            Op::Gte => binary!(lhs, rhs, a >= b),
            Op::StartsWith => match lhs {
                Value::String(lhs, ..) => match rhs {
                    Value::String(rhs, ..) => lhs.starts_with(&rhs),
                    Value::Bytes(rhs) => lhs.as_bytes().starts_with(&rhs),
                    _ => false,
                },
                Value::Bytes(lhs, ..) => match rhs {
                    Value::String(rhs, ..) => lhs.starts_with(rhs.as_bytes()),
                    Value::Bytes(rhs) => lhs.starts_with(&rhs),
                    _ => false,
                },
                Value::None(..) => return Ok(Test::Undefined),
                _ => false,
            },
        };

        Ok(result.into())
    }

    fn special(&self, process: &Process, value_type: Type) -> anyhow::Result<Option<Special>> {
        use self::Sign::*;
        use self::ValueExpr::*;
        use self::ValueTrait::*;

        let Binary(op, lhs, rhs) = self;

        let exact = match (op, lhs.reduced(), rhs.reduced()) {
            (Op::Eq, Number { value, .. }, Value) | (Op::Eq, Value, Number { value, .. }) => {
                Some(value::Value::from_bigint(value_type, value)?)
            }
            (Op::Eq, Decimal { value, .. }, Value) | (Op::Eq, Value, Decimal { value, .. }) => {
                Some(value::Value::from_bigdecimal(value_type, value)?)
            }
            (Op::Eq, String { value }, Value) | (Op::Eq, Value, String { value }) => {
                Some(value::Value::String(value.to_owned(), value.len()))
            }
            (Op::Eq, Bytes { value }, Value) | (Op::Eq, Value, Bytes { value }) => {
                Some(value::Value::Bytes(value.clone()))
            }
            (Op::StartsWith, Value, String { value }) => {
                Some(value::Value::String(value.to_owned(), value.len()))
            }
            (Op::StartsWith, Value, Bytes { value }) => Some(value::Value::Bytes(value.clone())),
            _ => None,
        };

        if let Some(value) = exact {
            let width = value.size(process);
            let mut buf = vec![0u8; width];
            value.encode(process, &mut buf)?;

            if buf.iter().all(|c| *c == 0) {
                return Ok(Some(Special::Zero(width)));
            } else {
                return Ok(Some(Special::Bytes(buf)));
            }
        }

        // expressions that only matches non-zeroed regions.
        let non_zero = match (op, lhs.traits(), rhs.traits()) {
            // any derefs must be done on a non-zero value.
            (_, (IsDeref, _), _) | (_, _, (IsDeref, _)) => true,
            // value equals something that is non-zero.
            (Op::Eq, (IsValue, _), (NonZero, _)) | (Op::Eq, (NonZero, _), (IsValue, _)) => true,
            // value does not equal to zero.
            (Op::Neq, (IsValue, _), (Zero, _)) | (Op::Neq, (Zero, _), (IsValue, _)) => true,
            // value is greater than non-zero positive.
            (Op::Gte, (IsValue, _), (NonZero, Plus)) => true,
            // value is less than non-zero negative.
            (Op::Lte, (IsValue, _), (NonZero, Minus)) => true,
            // less than zero.
            (Op::Lt, (IsValue, _), (NonZero, Minus)) => true,
            (Op::Lt, (IsValue, _), (Zero, _)) => true,
            // greater than zero.
            (Op::Gt, (IsValue, _), (NonZero, Plus)) => true,
            (Op::Gt, (IsValue, _), (Zero, _)) => true,
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
pub struct All(Vec<Filter>);

impl All {
    fn type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        self.0
            .iter()
            .flat_map(|m| m.type_of(initial_type, last_type, value_type))
            .next()
    }

    fn value_type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        self.0
            .iter()
            .flat_map(|m| m.value_type_of(initial_type, last_type, value_type))
            .next()
    }

    pub fn new(filters: impl IntoIterator<Item = Filter>) -> Self {
        All(filters.into_iter().collect())
    }

    pub fn special(&self, process: &Process, value_type: Type) -> anyhow::Result<Option<Special>> {
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
        value_type: Type,
        initial: &Value,
        last: &Value,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        for m in &self.0 {
            match m.test(value_type, initial, last, proxy)? {
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
pub struct Any(Vec<Filter>);

impl Any {
    fn type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        self.0
            .iter()
            .flat_map(|m| m.type_of(initial_type, last_type, value_type))
            .next()
    }

    fn value_type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        self.0
            .iter()
            .flat_map(|m| m.value_type_of(initial_type, last_type, value_type))
            .next()
    }

    pub fn new(filters: impl IntoIterator<Item = Filter>) -> Self {
        Any(filters.into_iter().collect())
    }

    pub fn special(&self, process: &Process, value_type: Type) -> anyhow::Result<Option<Special>> {
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
        value_type: Type,
        initial: &Value,
        last: &Value,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        for m in &self.0 {
            match m.test(value_type, initial, last, proxy)? {
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
    pub fn new(expr: ValueExpr, process: &Process) -> anyhow::Result<Self> {
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

    fn type_of(&self, _: Option<Type>, _: Option<Type>, _: Option<Type>) -> Option<Type> {
        Some(Type::Pointer)
    }

    fn value_type_of(&self, _: Option<Type>, _: Option<Type>, _: Option<Type>) -> Option<Type> {
        Some(Type::Pointer)
    }

    pub fn test(
        &self,
        value_type: Type,
        initial: &Value,
        last: &Value,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        let value = self.expr.eval(value_type, initial, last, proxy)?;

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

    fn type_of(&self, _: Option<Type>, _: Option<Type>, _: Option<Type>) -> Option<Type> {
        Some(self.ty)
    }

    fn value_type_of(&self, _: Option<Type>, _: Option<Type>, _: Option<Type>) -> Option<Type> {
        Some(self.ty)
    }

    pub fn test(
        &self,
        value_type: Type,
        initial: &Value,
        last: &Value,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        let value = self.expr.eval(value_type, initial, last, proxy)?;
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

    pub fn special(&self, process: &Process, value_type: Type) -> anyhow::Result<Option<Special>> {
        Ok(Some(Special::NonZero(value_type.size(process))))
    }

    fn type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        Some(
            self.expr
                .type_of(initial_type, last_type, value_type)
                .unwrap_or(Type::F32),
        )
    }

    fn value_type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        Some(
            self.expr
                .value_type_of(initial_type, last_type, value_type)
                .unwrap_or(Type::F32),
        )
    }

    pub fn test(
        &self,
        value_type: Type,
        initial: &Value,
        last: &Value,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        let value = self.expr.eval(value_type, initial, last, proxy)?;

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

    fn type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        self.expr.type_of(initial_type, last_type, value_type)
    }

    fn value_type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        self.expr.value_type_of(initial_type, last_type, value_type)
    }

    pub fn test(
        &self,
        value_type: Type,
        initial: &Value,
        last: &Value,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        let value = self.expr.eval(value_type, initial, last, proxy)?;

        let bytes = match &value {
            Value::Bytes(bytes) => &bytes[..],
            Value::String(s, ..) => s.as_bytes(),
            Value::None(..) => return Ok(Test::Undefined),
            _ => return Ok(Test::False),
        };

        Ok(self.regex.is_match(bytes).into())
    }

    pub fn special(&self) -> anyhow::Result<Option<Special>> {
        Ok(Some(Special::Regex(self.regex.clone())))
    }
}

impl fmt::Display for Regex {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "is none")
    }
}

#[derive(Debug, Clone)]
pub struct Not {
    filter: Box<Filter>,
}

impl Not {
    fn test(
        &self,
        value_type: Type,
        initial: &Value,
        last: &Value,
        proxy: &mut AddressProxy<'_>,
    ) -> anyhow::Result<Test> {
        Ok(self.filter.test(value_type, initial, last, proxy)?.invert())
    }

    fn type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        self.filter.type_of(initial_type, last_type, value_type)
    }

    fn value_type_of(
        &self,
        initial_type: Option<Type>,
        last_type: Option<Type>,
        value_type: Option<Type>,
    ) -> Option<Type> {
        self.filter
            .value_type_of(initial_type, last_type, value_type)
    }

    pub fn special(&self, process: &Process, value_type: Type) -> anyhow::Result<Option<Special>> {
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
            Filter::IsPointer(..) => write!(fmt, "is not pointer"),
            Filter::IsType(is_type) => write!(fmt, "is not {}", is_type.ty),
            Filter::IsNan(..) => write!(fmt, "is not nan"),
            other => write!(fmt, "not {}", other),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{lexer, parser};

    #[test]
    fn test_basic_parsing() -> anyhow::Result<()> {
        let parser = parser::OrParser::new();
        let a = parser.parse(lexer::Lexer::new("value == 1"))?;
        let b = parser.parse(lexer::Lexer::new("value == 1 and value == 2"))?;
        let c = parser.parse(lexer::Lexer::new("value == 1 and value == 2 or value == 3"))?;

        dbg!(a);
        dbg!(b);
        dbg!(c);

        Ok(())
    }
}
