//! Pins the behaviour of the portable pointer grammar through
//! `PortablePointer::parse`, independent of the parser implementation.

use super::{PortableBase, PortablePointer};
use crate::{Address, Offset, Sign, Size};

#[track_caller]
fn parse(input: &str) -> PortablePointer {
    match PortablePointer::parse(input) {
        Ok(pointer) => pointer,
        Err(e) => panic!("expected {:?} to parse: {}", input, e),
    }
}

#[track_caller]
fn rejects(input: &str) {
    if let Ok(pointer) = PortablePointer::parse(input) {
        panic!("expected {:?} to be rejected, got {:?}", input, pointer);
    }
}

#[track_caller]
fn error(input: &str) -> String {
    match PortablePointer::parse(input) {
        Ok(pointer) => panic!("expected {:?} to be rejected, got {:?}", input, pointer),
        Err(e) => e.to_string(),
    }
}

fn off(offset: i64) -> Offset {
    match offset {
        0 => Offset::zero(),
        o if o < 0 => Offset::new(Sign::Minus, Size::new(o.unsigned_abs())),
        o => Offset::new(Sign::Plus, Size::new(o as u64)),
    }
}

fn address(address: u64, offsets: &[i64]) -> PortablePointer {
    PortablePointer::new(
        PortableBase::Address {
            address: Address::new(address),
        },
        offsets.iter().copied().map(off),
    )
}

fn module(name: &str, offset: i64, offsets: &[i64]) -> PortablePointer {
    PortablePointer::new(
        PortableBase::Module {
            name: name.into(),
            offset: off(offset),
        },
        offsets.iter().copied().map(off),
    )
}

#[test]
fn address_bases() {
    assert_eq!(parse("0xABCDEF"), address(0xABCDEF, &[]));
    assert_eq!(parse("0xabcdef"), address(0xABCDEF, &[]));
    assert_eq!(parse("0x0"), address(0, &[]));
    // Base offsets are folded into the address.
    assert_eq!(parse("0xA + 0x10 - 0x2"), address(0x18, &[]));
}

#[test]
fn module_bases() {
    assert_eq!(parse("\"Steam.exe\""), module("Steam.exe", 0, &[]));
    assert_eq!(parse("\"Steam.exe\" + 0x0F"), module("Steam.exe", 0xF, &[]));
    assert_eq!(
        parse("\"Steam.exe\" - 0xF0F"),
        module("Steam.exe", -0xF0F, &[])
    );
    assert_eq!(
        parse("\"Steam.exe\" + 0x10 - 0x4 + 0x1"),
        module("Steam.exe", 0xD, &[])
    );
    assert_eq!(parse("\"a b\\\"c\\\\\""), module("a b\"c\\", 0, &[]));
}

#[test]
fn unquoted_module_names_end_at_space_or_offset() {
    assert_eq!(parse("Steam.exe"), module("Steam.exe", 0, &[]));
    assert_eq!(parse("Steam.exe + 0x0F"), module("Steam.exe", 0xF, &[]));
    assert_eq!(parse("Steam.exe+0x0F"), module("Steam.exe", 0xF, &[]));
    assert_eq!(parse("Steam.exe-0x0F"), module("Steam.exe", -0xF, &[]));
    assert_eq!(parse("Steam.exe->0x8"), module("Steam.exe", 0, &[0x8]));
    assert_eq!(
        parse("\"my-game.exe\"+0x1"),
        module("my-game.exe", 0x1, &[])
    );
    assert_eq!(parse("my-game.exe+0x1"), module("my-game.exe", 0x1, &[]));
    assert_eq!(parse("my-game.exe-0x1"), module("my-game.exe", -0x1, &[]));
    assert_eq!(parse("my-game.exe->0x8"), module("my-game.exe", 0, &[0x8]));
}

#[test]
fn unclosed_quoted_name_is_rejected() {
    rejects("\"Steam.exe");
    rejects("\"Steam.exe + 0x0F");
    rejects("\"");
}

#[test]
fn deref_offsets() {
    assert_eq!(parse("0x10 -> 0x8"), address(0x10, &[0x8]));
    assert_eq!(
        parse("\"Steam.exe\" + 0x0F -> -0xFFAA"),
        module("Steam.exe", 0xF, &[-0xFFAA])
    );
    assert_eq!(
        parse("\"Steam.exe\" -> +0xFFAA -> 0x0 -> - 0x8"),
        module("Steam.exe", 0, &[0xFFAA, 0, -0x8])
    );
}

#[test]
fn hex_numbers_take_no_sign() {
    rejects("0x-10");
    rejects("0x10 -> 0x-8");
    rejects("0x+10");
    assert_eq!(parse("0x10+0x2"), address(0x12, &[]));
    assert_eq!(parse("0x10-0x2"), address(0xE, &[]));
    assert_eq!(parse("0x10->0x8"), address(0x10, &[0x8]));
    assert_eq!(parse("0x10->-0x8"), address(0x10, &[-0x8]));
}

#[test]
fn rejected_pointers() {
    rejects("");
    rejects("0x");
    rejects("0xG");
    rejects("-> 0x10");
    rejects("0x10 ->");
    rejects("0x10 -> \"a\"");
    rejects("0x10 0x20");
    rejects("\"a\" + \"b\"");
    rejects("\"a\" \"b\"");
    rejects("0x10 +");
    rejects("\"\\q\"");
    rejects("0x10 -> 0x8 + 0x1");
}

#[test]
fn errors_name_what_was_expected_and_where() {
    assert_eq!(
        error(""),
        "expected hex address or module name, found end of input at position 0"
    );
    assert_eq!(
        error("0x10 ->"),
        "expected `+`, `-` or hex number, found end of input at position 7"
    );
    assert_eq!(
        error("0x10 0x20"),
        "expected `+`, `-`, `->` or end of input, found hex number at position 5"
    );
    assert_eq!(
        error("0x10 -> 0x8 + 0x1"),
        "expected `->` or end of input, found `+` at position 12"
    );
    assert_eq!(
        error("\"a\" + \"b\""),
        "expected hex number, found module name at position 6"
    );
    assert_eq!(
        error("0x-10"),
        "expected hex digits after `0x` at position 2"
    );
    assert_eq!(
        error("0x1 + \"ab"),
        "unclosed quoted module name at position 6"
    );
}

#[test]
fn display_round_trips() {
    for input in [
        "0xABCDEF",
        "\"Steam.exe\" + 0x0F",
        "\"Steam.exe\" - 0xF0F -> -0xFFAA",
        "\"Steam.exe\" -> 0xFFAA -> 0x0 -> -0x8",
        "\"a b\\\"c\"",
    ] {
        let pointer = parse(input);
        assert_eq!(parse(&pointer.to_string()), pointer, "{}", input);
    }
}
