//! Pins the behaviour of the filter and value expression grammars, independent
//! of the parser implementation. Only `parse_filter` and `parse_value` know
//! which parser is used.

use super::{ast::FilterExpr as F, parser, FilterOp};
use crate::{value_expr::ast::ValueExpr as V, value_expr::ValueOp, PointerWidth, Type};

fn parse_filter(input: &str) -> anyhow::Result<F> {
    Ok(parser::parse_filter(input)?)
}

fn parse_value(input: &str) -> anyhow::Result<V> {
    Ok(parser::parse_value(input)?)
}

#[track_caller]
fn filter(input: &str) -> F {
    match parse_filter(input) {
        Ok(expr) => expr,
        Err(e) => panic!("expected {:?} to parse as a filter: {}", input, e),
    }
}

#[track_caller]
fn value(input: &str) -> V {
    match parse_value(input) {
        Ok(expr) => expr,
        Err(e) => panic!("expected {:?} to parse as a value: {}", input, e),
    }
}

#[track_caller]
fn rejects_filter(input: &str) {
    if let Ok(expr) = parse_filter(input) {
        panic!("expected {:?} to be rejected, got {:?}", input, expr);
    }
}

#[track_caller]
fn rejects_value(input: &str) {
    if let Ok(expr) = parse_value(input) {
        panic!("expected {:?} to be rejected, got {:?}", input, expr);
    }
}

fn n(number: i64) -> V {
    V::Number(number.into(), None)
}

fn bin(op: ValueOp, lhs: V, rhs: V) -> V {
    V::Binary(op, Box::new(lhs), Box::new(rhs))
}

fn deref(expr: V) -> V {
    V::Deref(Box::new(expr))
}

fn addr(expr: V) -> V {
    V::AddressOf(Box::new(expr))
}

fn cast(expr: V, ty: Type) -> V {
    V::Cast(Box::new(expr), ty)
}

fn eq(lhs: V, rhs: V) -> F {
    F::Binary(FilterOp::Eq, lhs, rhs)
}

fn not(expr: F) -> F {
    F::Not(Box::new(expr))
}

fn ty(s: &str) -> Type {
    s.parse().unwrap()
}

#[test]
fn value_terms() {
    assert_eq!(value("value"), V::Value);
    assert_eq!(value("last"), V::Last);
    assert_eq!(value("initial"), V::Initial);
    assert_eq!(value("42"), n(42));
    assert_eq!(value("-42"), n(-42));
    assert_eq!(value("0x10"), n(16));
    assert_eq!(value("0xFFu8"), V::Number(255.into(), Some(Type::U8)));
    assert_eq!(value("-0x10"), n(-16));
    assert_eq!(value("42.5"), V::Decimal("42.5".parse().unwrap(), None));
    assert_eq!(
        value("-1.5f64"),
        V::Decimal("-1.5".parse().unwrap(), Some(Type::F64))
    );
    assert_eq!(value("\"ui_boot\""), V::String("ui_boot".into()));
    assert_eq!(value("{01 ab FF}"), V::Bytes(vec![0x01, 0xab, 0xff]));
    assert_eq!(value("{}"), V::Bytes(vec![]));
}

#[test]
fn number_suffixes() {
    let cases = [
        ("1u8", Type::U8),
        ("1i8", Type::I8),
        ("1u16", Type::U16),
        ("1i16", Type::I16),
        ("1u32", Type::U32),
        ("1i32", Type::I32),
        ("1u64", Type::U64),
        ("1i64", Type::I64),
        ("1u128", Type::U128),
        ("1i128", Type::I128),
        ("1f32", Type::F32),
        ("1f64", Type::F64),
    ];

    for (input, expected) in cases {
        assert_eq!(
            value(input),
            V::Number(1.into(), Some(expected)),
            "{}",
            input
        );
    }

    rejects_value("1u7");
    rejects_value("1x");
}

#[test]
fn hex_digits_swallow_float_suffixes() {
    // `f` is a hex digit, so a hex literal cannot carry an `f32`/`f64` suffix.
    assert_eq!(value("0x10f32"), n(0x10f32));
}

#[test]
fn string_escapes() {
    assert_eq!(
        value(r#""a\"b\\c\n\r\t\0""#),
        V::String("a\"b\\c\n\r\t\0".into())
    );
    assert_eq!(value(r#""Aé""#), V::String("A\u{e9}".into()));
    rejects_value(r#""abc"#);
    rejects_value(r#""\q""#);
    rejects_value(r#""\u00""#);
}

#[test]
fn bare_identifiers_are_strings() {
    // Anything that lexes as an identifier and is not a keyword or a type is a
    // string literal; keywords are case-sensitive.
    assert_eq!(value("foo"), V::String("foo".into()));
    assert_eq!(value("ui_boot-1/x"), V::String("ui_boot-1/x".into()));
    assert_eq!(value("VALUE"), V::String("VALUE".into()));
    assert_eq!(
        filter("value == foo"),
        eq(V::Value, V::String("foo".into()))
    );
}

#[test]
fn bytes_literals() {
    assert_eq!(value("{0102}"), V::Bytes(vec![1, 2]));
    rejects_value("{0}");
    rejects_value("{zz}");
}

#[test]
fn value_precedence() {
    use ValueOp::*;

    assert_eq!(value("1 + 2 * 3"), bin(Add, n(1), bin(Mul, n(2), n(3))));
    assert_eq!(value("1 * 2 + 3"), bin(Add, bin(Mul, n(1), n(2)), n(3)));
    assert_eq!(value("1 - 2 / 3"), bin(Sub, n(1), bin(Div, n(2), n(3))));
    assert_eq!(value("(1 + 2) * 3"), bin(Mul, bin(Add, n(1), n(2)), n(3)));
    assert_eq!(value("((value))"), V::Value);

    // `as` binds tighter than arithmetic, deref and address-of tighter still.
    assert_eq!(
        value("1 + value as u8"),
        bin(Add, n(1), cast(V::Value, Type::U8))
    );
    assert_eq!(
        value("value * 2 as u8"),
        bin(Mul, V::Value, cast(n(2), Type::U8))
    );
    assert_eq!(value("*value as u32"), cast(deref(V::Value), Type::U32));
    assert_eq!(
        value("(value + 1) as u32"),
        cast(bin(Add, V::Value, n(1)), Type::U32)
    );
    assert_eq!(value("*value + 1"), bin(Add, deref(V::Value), n(1)));
    assert_eq!(value("value * *value"), bin(Mul, V::Value, deref(V::Value)));
}

#[test]
fn value_associativity() {
    use ValueOp::*;

    assert_eq!(value("1 - 2 - 3"), bin(Sub, bin(Sub, n(1), n(2)), n(3)));
    assert_eq!(value("1 + 2 - 3"), bin(Sub, bin(Add, n(1), n(2)), n(3)));
    assert_eq!(value("8 / 4 / 2"), bin(Div, bin(Div, n(8), n(4)), n(2)));
    assert_eq!(value("8 * 4 / 2"), bin(Div, bin(Mul, n(8), n(4)), n(2)));
    assert_eq!(
        value("value as u8 as u16"),
        cast(cast(V::Value, Type::U8), Type::U16)
    );
}

#[test]
fn deref_and_address_of() {
    use ValueOp::*;

    assert_eq!(value("*value"), deref(V::Value));
    assert_eq!(value("**value"), deref(deref(V::Value)));
    assert_eq!(value("&value"), addr(V::Value));
    assert_eq!(value("*&value"), deref(addr(V::Value)));
    assert_eq!(
        value("*(&value + 0x40)"),
        deref(bin(Add, addr(V::Value), n(0x40)))
    );
    // Not every value has an address, but that is not a parse error.
    assert_eq!(value("&(value + 42)"), addr(bin(Add, V::Value, n(42))));
}

#[test]
fn minus_and_negative_literals() {
    use ValueOp::*;

    assert_eq!(value("value - 1"), bin(Sub, V::Value, n(1)));
    assert_eq!(value("value - -1"), bin(Sub, V::Value, n(-1)));
    // A `-` directly followed by a digit lexes as a negative literal, so this
    // is two adjacent terms.
    rejects_value("value -1");
    // `-` is an identifier character, so this is a single bare string.
    assert_eq!(value("value-1"), V::String("value-1".into()));
    // There is no unary minus on non-literals.
    rejects_value("-value");
}

#[test]
fn rejected_values() {
    rejects_value("");
    rejects_value("value +");
    rejects_value("+ value");
    rejects_value("value + + 1");
    rejects_value("(value");
    rejects_value("value)");
    rejects_value("()");
    rejects_value("value as");
    rejects_value("value as foo");
    rejects_value("u32");
    rejects_value("value value");
    rejects_value("value & value");
}

#[test]
fn comparison_operators() {
    let cases = [
        ("==", FilterOp::Eq),
        ("!=", FilterOp::Neq),
        ("<", FilterOp::Lt),
        ("<=", FilterOp::Lte),
        (">", FilterOp::Gt),
        (">=", FilterOp::Gte),
        ("^", FilterOp::StartsWith),
    ];

    for (op, expected) in cases {
        let input = format!("value {} 1", op);
        assert_eq!(
            filter(&input),
            F::Binary(expected, V::Value, n(1)),
            "{}",
            input
        );
        let input = format!("value{}1", op);
        assert_eq!(
            filter(&input),
            F::Binary(expected, V::Value, n(1)),
            "{}",
            input
        );
    }

    assert_eq!(
        filter("value + 1 == last * 2"),
        eq(
            bin(ValueOp::Add, V::Value, n(1)),
            bin(ValueOp::Mul, V::Last, n(2))
        )
    );
    assert_eq!(
        filter("value as u128 == 1"),
        eq(cast(V::Value, Type::U128), n(1))
    );
    assert_eq!(filter("(value == 1)"), eq(V::Value, n(1)));
}

#[test]
fn regex_filters() {
    assert_eq!(
        filter("value ~ \"^foo\""),
        F::Regex(V::Value, V::String("^foo".into()))
    );
    assert_eq!(
        filter("value !~ \"foo\""),
        not(F::Regex(V::Value, V::String("foo".into())))
    );
    // The pattern is any value expression; only conversion rejects non-strings.
    assert_eq!(filter("value ~ 1"), F::Regex(V::Value, n(1)));
}

#[test]
fn is_filters() {
    assert_eq!(filter("value is u32"), F::IsType(V::Value, Type::U32));
    assert_eq!(
        filter("value is pointer"),
        F::IsType(V::Value, Type::Pointer(PointerWidth::Pointer64))
    );
    assert_eq!(filter("value is none"), F::IsType(V::Value, Type::None));
    assert_eq!(
        filter("value is not none"),
        not(F::IsType(V::Value, Type::None))
    );
    assert_eq!(filter("value is string"), F::IsType(V::Value, ty("string")));
    assert_eq!(
        filter("value is string/utf-16"),
        F::IsType(V::Value, ty("string/utf-16"))
    );
    assert_eq!(
        filter("value is bytes/4"),
        F::IsType(V::Value, Type::Bytes(Some(4)))
    );
    assert_eq!(
        filter("value is bytes"),
        F::IsType(V::Value, Type::Bytes(None))
    );
    assert_eq!(filter("value is nan"), F::IsNan(V::Value));
    assert_eq!(filter("value is not nan"), not(F::IsNan(V::Value)));
    assert_eq!(
        filter("*value + 1 is pointer"),
        F::IsType(
            bin(ValueOp::Add, deref(V::Value), n(1)),
            Type::Pointer(PointerWidth::Pointer64)
        )
    );

    rejects_filter("value is");
    rejects_filter("value is not");
    rejects_filter("value is foo");
    rejects_filter("value is not not u32");
    rejects_filter("value is string/nope");
}

#[test]
fn and_or_precedence() {
    let a = || eq(V::Value, n(1));
    let b = || eq(V::Value, n(2));
    let c = || eq(V::Value, n(3));

    // Chains are flattened into a single node.
    assert_eq!(filter("value == 1 and value == 2"), F::And(vec![a(), b()]));
    assert_eq!(
        filter("value == 1 and value == 2 and value == 3"),
        F::And(vec![a(), b(), c()])
    );
    assert_eq!(
        filter("value == 1 or value == 2 or value == 3"),
        F::Or(vec![a(), b(), c()])
    );

    // `and` binds tighter than `or`.
    assert_eq!(
        filter("value == 1 or value == 2 and value == 3"),
        F::Or(vec![a(), F::And(vec![b(), c()])])
    );
    assert_eq!(
        filter("value == 1 and value == 2 or value == 3"),
        F::Or(vec![F::And(vec![a(), b()]), c()])
    );

    // Parentheses group, and grouped chains are not flattened into the parent.
    assert_eq!(
        filter("(value == 1 or value == 2) and value == 3"),
        F::And(vec![F::Or(vec![a(), b()]), c()])
    );
    assert_eq!(
        filter("(value == 1 and value == 2) and value == 3"),
        F::And(vec![F::And(vec![a(), b()]), c()])
    );
    assert_eq!(filter("((value == 1))"), a());
}

#[test]
fn rejected_filters() {
    rejects_filter("");
    rejects_filter("value");
    rejects_filter("value ==");
    rejects_filter("== 1");
    rejects_filter("value == 1 == 2");
    rejects_filter("value < 1 < 2");
    rejects_filter("value == 1 and");
    rejects_filter("and value == 1");
    rejects_filter("value == 1 or");
    rejects_filter("value == 1 value == 2");
    rejects_filter("(value == 1");
    rejects_filter("value == 1)");
    rejects_filter("value == 1 && value == 2");
    rejects_filter("value == 1 || value == 2");
    rejects_filter("not value == 1");
    rejects_filter("value ~");
    rejects_filter("value = 1");
    rejects_filter("value === 1");
    rejects_filter("u32 == 1");
}

#[test]
fn all_whitespace_separates() {
    assert_eq!(filter("  value   ==  1  "), eq(V::Value, n(1)));
    assert_eq!(filter("value\t== 1"), eq(V::Value, n(1)));
    assert_eq!(filter("value ==\n1"), eq(V::Value, n(1)));
    assert_eq!(
        filter("\r\nvalue == 1\n\tand\tvalue == 2\n"),
        F::And(vec![eq(V::Value, n(1)), eq(V::Value, n(2))])
    );
    assert_eq!(value("{01\tab\nFF}"), V::Bytes(vec![0x01, 0xab, 0xff]));
}

fn error(input: &str) -> String {
    match parse_filter(input) {
        Ok(expr) => panic!("expected {:?} to be rejected, got {:?}", input, expr),
        Err(e) => e.to_string(),
    }
}

#[test]
fn errors_name_what_was_expected_and_where() {
    assert_eq!(
        error(""),
        "expected value, found end of input at position 0"
    );
    assert_eq!(
        error("value"),
        "expected `is` or a comparison operator, found end of input at position 5"
    );
    assert_eq!(
        error("value == 1 == 2"),
        "expected `and`, `or` or end of input, found `==` at position 11"
    );
    assert_eq!(
        error("value is foo"),
        "expected type, `not` or `nan`, found `foo` at position 9"
    );
    assert_eq!(
        error("value is not \"x\""),
        "expected type or `nan`, found `\"x\"` at position 13"
    );
    assert_eq!(
        error("value as 1 == 1"),
        "expected type, found `1` at position 9"
    );
    assert_eq!(
        error("(value == 1"),
        "expected `and`, `or` or `)`, found end of input at position 11"
    );
    assert_eq!(
        error("(value + 1 == 2"),
        "expected `and`, `or` or `)`, found end of input at position 15"
    );
    assert_eq!(
        error("(value) =="),
        "expected value, found end of input at position 10"
    );
    assert_eq!(
        error("value == 1 && value == 2"),
        "expected `and`, `or` or end of input, found `&` at position 11"
    );
    assert_eq!(error("value @ 1"), "unexpected character `@` at position 6");
    assert_eq!(
        parse_value("value value").unwrap_err().to_string(),
        "expected `+`, `-`, `*`, `/`, `as` or end of input, found `value` at position 6"
    );
}

#[test]
fn grouped_values_in_filters() {
    use ValueOp::*;

    assert_eq!(filter("(value) == 1"), eq(V::Value, n(1)));
    assert_eq!(
        filter("(value + 1) * 2 == 4"),
        eq(bin(Mul, bin(Add, V::Value, n(1)), n(2)), n(4))
    );
    assert_eq!(
        filter("((value)) is u8 and (value == 1)"),
        F::And(vec![F::IsType(V::Value, Type::U8), eq(V::Value, n(1))])
    );
}

#[test]
fn deep_nesting_is_rejected() {
    let input = format!("{}value{} == 1", "(".repeat(10_000), ")".repeat(10_000));
    assert!(error(&input).starts_with("expression is nested too deeply"));
    let input = format!("{}value == 1", "*".repeat(10_000));
    assert!(error(&input).starts_with("expression is nested too deeply"));
}
