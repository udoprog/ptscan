use self::ast::{EscapeString, Hex, Op, ValueTrait};
use crate::{
    pointer,
    process::{MemoryInformation, Process},
    value,
    value::Value,
    Address, AddressProxy, AddressRange, Offset, Type,
};
use anyhow::{anyhow, bail};
use num_bigint::{BigInt, Sign};
use serde::{Deserialize, Serialize};
use std::fmt;

pub mod ast;
mod lexer;
lalrpop_util::lalrpop_mod!(parser, "/filter/parser.rs");

const ZERO: [u8; 16] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

/// test if the given slice contains all zeros.
/// Hopefully this gets vectorized (please check!).
fn is_all_zeros(mut data: &[u8]) -> bool {
    while !data.is_empty() {
        let len = usize::min(data.len(), ZERO.len());

        if &data[..len] != &ZERO[..len] {
            return false;
        }

        data = &data[len..];
    }

    true
}

/// Find first nonzero byte in a slice.
fn find_first_nonzero(mut data: &[u8]) -> Option<usize> {
    let mut local = 0;

    while !data.is_empty() {
        let len = usize::min(data.len(), ZERO.len());

        if &data[..len] != &ZERO[..len] {
            break;
        }

        data = &data[len..];
        local += len;
    }

    if data.is_empty() {
        return None;
    }

    let index = match data.iter().position(|c| *c != 0) {
        Some(index) => index,
        None => return None,
    };

    Some(local + index)
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
    pub fn parse(input: &str, ty: Option<Type>, process: &Process) -> anyhow::Result<ValueExpr> {
        let expr = parser::ValueExprParser::new().parse(lexer::Lexer::new(input))?;

        let ty = ty
            .or_else(|| expr.type_from_hint())
            .ok_or_else(|| anyhow!("can't determine type of expression"))?;

        let value = expr.eval(ty, process)?;
        Ok(value)
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
            other => bail!("can't evaluate value yet: {}", other),
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
        }
    }
}

pub enum Special {
    Bytes(Vec<u8>),
    NotBytes(Vec<u8>),
    Zero(usize),
    NonZero(usize),
    /// All the inner specials must match.
    All(Vec<Special>),
    /// Any of the inner specials match.
    Any(Vec<Special>),
    /// Special seek for a regular expression.
    Regex(regex::bytes::Regex),
}

impl Special {
    pub fn test(&self, mut data: &[u8]) -> Option<usize> {
        match *self {
            Self::All(ref all) => all.iter().flat_map(|s| s.test(data)).max(),
            Self::Any(ref any) => any.iter().flat_map(|s| s.test(data)).min(),
            Self::Bytes(ref bytes) => {
                if bytes.is_empty() {
                    return None;
                }

                let first = bytes[0];
                let mut local = 0usize;

                while !data.is_empty() {
                    let index = match memchr::memchr(first, data) {
                        Some(index) => index,
                        None => {
                            return None;
                        }
                    };

                    data = &data[index..];
                    let len = usize::min(data.len(), bytes.len());

                    if &data[..len] == &bytes[..len] {
                        return Some(local + index);
                    }

                    local += index + 1;
                    data = &data[1..];
                }

                None
            }
            Self::NotBytes(..) => None,
            Self::NonZero(..) => find_first_nonzero(data),
            Self::Zero(width) => {
                if width == 0 {
                    return None;
                }

                let mut local = 0usize;

                while !data.is_empty() {
                    let index = match memchr::memchr(0, data) {
                        Some(index) => index,
                        None => {
                            return Some(local + data.len());
                        }
                    };

                    local += index;
                    data = &data[index..];

                    if data.len() < width {
                        return Some(local);
                    }

                    if is_all_zeros(&data[..width]) {
                        return Some(local);
                    }

                    let offset = match data[..width].iter().position(|c| *c != 0) {
                        Some(offset) => offset,
                        None => width,
                    };

                    local += offset;
                    data = &data[offset..];
                }

                None
            }
            Self::Regex(ref regex) => match regex.find(data) {
                Some(m) => Some(m.start()),
                None => None,
            },
        }
    }
}

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

#[derive(Debug)]
pub struct Filter {
    pub ty: Type,
    pub matcher: Matcher,
}

impl Filter {
    /// Construct a new, typed filter.
    pub fn new(ty: Type, matcher: Matcher) -> Self {
        Self { ty, matcher }
    }

    /// Parse the given filter.
    pub fn parse(filter: &str, ty: Option<Type>, process: &Process) -> anyhow::Result<Self> {
        let (ty, matcher) = Matcher::parse(filter, ty, process)?;
        Ok(Self::new(ty, matcher))
    }

    /// Construct a new pointer filter.
    pub fn pointer(ty: Type, expr: ValueExpr, process: &Process) -> anyhow::Result<Filter> {
        Ok(Filter {
            ty,
            matcher: Matcher::IsPointer(IsPointer::new(expr, process)?),
        })
    }

    /// Test the given bytes against this filter.
    pub fn test(&self, last: &Value, proxy: AddressProxy<'_>) -> anyhow::Result<Test> {
        Ok(self.matcher.test(self.ty, last, proxy)?)
    }

    pub fn special(&self, process: &Process) -> anyhow::Result<Option<Special>> {
        self.matcher.special(process, self.ty)
    }
}

#[derive(Debug, Clone)]
pub enum Matcher {
    Binary(Binary),
    All(All),
    Any(Any),
    IsPointer(IsPointer),
    IsNone(IsNone),
    Not(Not),
    Regex(Regex),
}

impl Matcher {
    /// Parse a a string into a matcher.
    pub fn parse(input: &str, ty: Option<Type>, process: &Process) -> anyhow::Result<(Type, Self)> {
        let expr = parser::OrParser::new().parse(lexer::Lexer::new(input))?;

        let ty = ty
            .or_else(|| expr.type_from_hint())
            .ok_or_else(|| anyhow!("can't determine type of expression"))?;

        Ok((ty, expr.into_matcher(ty, process)?))
    }

    /// Test the specified memory region.
    fn test(&self, ty: Type, last: &Value, proxy: AddressProxy<'_>) -> anyhow::Result<Test> {
        match self {
            Self::Binary(m) => m.test(ty, last, proxy),
            Self::All(m) => m.test(ty, last, proxy),
            Self::Any(m) => m.test(ty, last, proxy),
            Self::IsPointer(m) => m.test(ty, last, proxy),
            Self::IsNone(m) => m.test(ty, last, proxy),
            Self::Not(m) => m.test(ty, last, proxy),
            Self::Regex(m) => m.test(ty, last, proxy),
        }
    }

    /// Construct a special matcher.
    fn special(&self, process: &Process, ty: Type) -> anyhow::Result<Option<Special>> {
        match self {
            Self::Binary(m) => m.special(process, ty),
            Self::All(m) => m.special(process, ty),
            Self::Any(m) => m.special(process, ty),
            Self::IsPointer(..) => Ok(Some(Special::NonZero(ty.size(process)))),
            Self::IsNone(..) => Ok(None),
            Self::Not(m) => m.special(process, ty),
            Self::Regex(m) => m.special(),
        }
    }
}

impl fmt::Display for Matcher {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Binary(m) => m.fmt(fmt),
            Self::All(m) => m.fmt(fmt),
            Self::Any(m) => m.fmt(fmt),
            Self::IsPointer(m) => m.fmt(fmt),
            Self::IsNone(m) => m.fmt(fmt),
            Self::Not(m) => m.fmt(fmt),
            Self::Regex(m) => m.fmt(fmt),
        }
    }
}

/// Only match values which are exactly equal.
#[derive(Debug, Clone)]
pub struct Binary(Op, ValueExpr, ValueExpr);

impl Binary {
    fn test(&self, ty: Type, last: &Value, proxy: AddressProxy<'_>) -> anyhow::Result<Test> {
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

        let (_, lhs) = lhs.eval(ty, last, proxy)?;
        let (_, rhs) = rhs.eval(ty, last, proxy)?;

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

    fn special(&self, process: &Process, ty: Type) -> anyhow::Result<Option<Special>> {
        use self::Sign::*;
        use self::ValueExpr::*;
        use self::ValueTrait::*;

        let Binary(op, lhs, rhs) = self;

        let exact = match (op, lhs, rhs) {
            (Op::Eq, Number { value }, Value) | (Op::Eq, Value, Number { value }) => {
                Some(value::Value::from_bigint(ty, value)?)
            }
            (Op::Eq, String { value }, Value) | (Op::Eq, Value, String { value }) => {
                Some(value::Value::String(value.to_owned(), value.len()))
            }
            (Op::Eq, Bytes { value }, Value) | (Op::Eq, Value, Bytes { value }) => {
                Some(value::Value::Bytes(value.clone()))
            }
            (Op::StartsWith, Value, Number { value }) => {
                Some(value::Value::from_bigint(ty, value)?)
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
            return Ok(Some(Special::NonZero(ty.size(process))));
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
pub struct All(Vec<Matcher>);

impl All {
    pub fn new(filters: impl IntoIterator<Item = Matcher>) -> Self {
        All(filters.into_iter().collect())
    }

    pub fn special(&self, process: &Process, ty: Type) -> anyhow::Result<Option<Special>> {
        let mut all = Vec::new();

        for e in &self.0 {
            if let Some(special) = e.special(process, ty)? {
                all.push(special);
            }
        }

        if all.len() > 1 {
            return Ok(Some(Special::All(all)));
        }

        Ok(all.into_iter().next())
    }

    fn test(&self, ty: Type, last: &Value, proxy: AddressProxy<'_>) -> anyhow::Result<Test> {
        for m in &self.0 {
            match m.test(ty, last, proxy)? {
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
pub struct Any(Vec<Matcher>);

impl Any {
    pub fn new(filters: impl IntoIterator<Item = Matcher>) -> Self {
        Any(filters.into_iter().collect())
    }

    pub fn special(&self, process: &Process, ty: Type) -> anyhow::Result<Option<Special>> {
        let mut any = Vec::new();

        for e in &self.0 {
            match e.special(process, ty)? {
                Some(special) => any.push(special),
                None => return Ok(None),
            }
        }

        if any.len() > 1 {
            return Ok(Some(Special::Any(any)));
        }

        Ok(any.into_iter().next())
    }

    fn test(&self, ty: Type, last: &Value, proxy: AddressProxy<'_>) -> anyhow::Result<Test> {
        for m in &self.0 {
            match m.test(ty, last, proxy)? {
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

    pub fn test(&self, ty: Type, last: &Value, proxy: AddressProxy<'_>) -> anyhow::Result<Test> {
        let (_, value) = self.expr.eval(ty, last, proxy)?;

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
pub struct IsNone {
    expr: ValueExpr,
}

impl IsNone {
    pub fn new(expr: ValueExpr) -> anyhow::Result<Self> {
        Ok(Self { expr })
    }

    pub fn test(&self, ty: Type, last: &Value, proxy: AddressProxy<'_>) -> anyhow::Result<Test> {
        let (_, value) = self.expr.eval(ty, last, proxy)?;
        Ok(value.is_none().into())
    }
}

impl fmt::Display for IsNone {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "is none")
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

    pub fn test(&self, ty: Type, last: &Value, proxy: AddressProxy<'_>) -> anyhow::Result<Test> {
        let (_, value) = self.expr.eval(ty, last, proxy)?;

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
    matcher: Box<Matcher>,
}

impl Not {
    fn test(&self, ty: Type, last: &Value, proxy: AddressProxy<'_>) -> anyhow::Result<Test> {
        Ok(self.matcher.test(ty, last, proxy)?.invert())
    }

    pub fn special(&self, process: &Process, ty: Type) -> anyhow::Result<Option<Special>> {
        // erase or fix specializations produced.
        let special = match self.matcher.special(process, ty)? {
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
        match &*self.matcher {
            Matcher::IsPointer(..) => write!(fmt, "is not pointer"),
            other => write!(fmt, "not {}", other),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{find_first_nonzero, is_all_zeros, lexer, parser};
    use std::iter;

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

    #[test]
    fn test_find_first_nonzero() -> anyhow::Result<()> {
        assert_eq!(Some(4), find_first_nonzero(&[0, 0, 0, 0, 5, 0, 0]));
        assert_eq!(
            None,
            find_first_nonzero(&iter::repeat(0u8).take(127).collect::<Vec<_>>())
        );
        let test = iter::repeat(0u8)
            .take(126)
            .chain(iter::once(8))
            .collect::<Vec<_>>();
        assert_eq!(Some(126), find_first_nonzero(&test));
        assert_ne!(0, test[126]);
        Ok(())
    }

    #[test]
    fn test_is_all_zeros() -> anyhow::Result<()> {
        assert_eq!(false, is_all_zeros(&[0, 0, 0, 0, 5, 0, 0]));
        assert_eq!(
            true,
            is_all_zeros(&iter::repeat(0u8).take(127).collect::<Vec<_>>())
        );
        let test = iter::repeat(0u8)
            .take(126)
            .chain(iter::once(8))
            .collect::<Vec<_>>();
        assert_eq!(false, is_all_zeros(&test));
        Ok(())
    }
}
