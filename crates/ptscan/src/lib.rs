//! [<img alt="github" src="https://img.shields.io/badge/github-udoprog/ptscan-8da0cb?style=for-the-badge&logo=github" height="20">](https://github.com/udoprog/ptscan)
//! [<img alt="crates.io" src="https://img.shields.io/crates/v/ptscan.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/ptscan)
//! [<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-ptscan-66c2a5?style=for-the-badge&logoColor=white&logo=data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDUxMiA1MTIiPjxwYXRoIGZpbGw9IiNmNWY1ZjUiIGQ9Ik00ODguNiAyNTAuMkwzOTIgMjE0VjEwNS41YzAtMTUtOS4zLTI4LjQtMjMuNC0zMy43bC0xMDAtMzcuNWMtOC4xLTMuMS0xNy4xLTMuMS0yNS4zIDBsLTEwMCAzNy41Yy0xNC4xIDUuMy0yMy40IDE4LjctMjMuNCAzMy43VjIxNGwtOTYuNiAzNi4yQzkuMyAyNTUuNSAwIDI2OC45IDAgMjgzLjlWMzk0YzAgMTMuNiA3LjcgMjYuMSAxOS45IDMyLjJsMTAwIDUwYzEwLjEgNS4xIDIyLjEgNS4xIDMyLjIgMGwxMDMuOS01MiAxMDMuOSA1MmMxMC4xIDUuMSAyMi4xIDUuMSAzMi4yIDBsMTAwLTUwYzEyLjItNi4xIDE5LjktMTguNiAxOS45LTMyLjJWMjgzLjljMC0xNS05LjMtMjguNC0yMy40LTMzLjd6TTM1OCAyMTQuOGwtODUgMzEuOXYtNjguMmw4NS0zN3Y3My4zek0xNTQgMTA0LjFsMTAyLTM4LjIgMTAyIDM4LjJ2LjZsLTEwMiA0MS40LTEwMi00MS40di0uNnptODQgMjkxLjFsLTg1IDQyLjV2LTc5LjFsODUtMzguOHY3NS40em0wLTExMmwtMTAyIDQxLjQtMTAyLTQxLjR2LS42bDEwMi0zOC4yIDEwMiAzOC4ydi42em0yNDAgMTEybC04NSA0Mi41di03OS4xbDg1LTM4Ljh2NzUuNHptMC0xMTJsLTEwMiA0MS40LTEwMi00MS40di0uNmwxMDItMzguMiAxMDIgMzguMnYuNnoiPjwvcGF0aD48L3N2Zz4K" height="20">](https://docs.rs/ptscan)
//! [<img alt="build status" src="https://img.shields.io/github/workflow/status/udoprog/ptscan/CI/main?style=for-the-badge" height="20">](https://github.com/udoprog/ptscan/actions?query=branch%3Amain)
//!
//! A memory scanner for Windows, written in Rust.
//!
//! ![Main Window](https://raw.githubusercontent.com/udoprog/ptscan/main/gfx/main_window.png)
//!
//! <br>
//!
//! ## Usage
//!
//! ptscan is a cli tool which allows you to scan and dissect memory using _filters_.
//!
//! These filters are used in commands such as `scan` and `watch`.
//!
//! <br>
//!
//! ## Types
//!
//! All expressions support the following types:
//!
//! * `none` - The special `none` type. Any value of this type is _undefined_. See the section below for more details.
//! * `pointer` -  A pointer value, who's size depends on the process being attached to.
//! * `u8` - An unsigned 8-bit number.
//! * `i8` - A signed 8-bit number.
//! * `u16` - An unsigned 16-bit number.
//! * `i16` - A signed 16-bit number.
//! * `u32` - An unsigned 32-bit number.
//! * `i32` - A signed 32-bit number.
//! * `u64` - An unsigned 64-bit number.
//! * `i64` - A signed 64-bit number.
//! * `u128` - An unsigned 128-bit number.
//! * `i128` - A signed 128-bit number.
//! * `f32` - A 32-bit floating point number.
//! * `f64` - A 64-bit floating point number.
//! * `string` - A null-terminated string with the default encoding (`utf-8`).
//! * `string/<encoding>` - A null-terminated string with the specified `<encoding>`, as per [the whatwg Encoding standard](https://encoding.spec.whatwg.org/#names-and-labels).
//! * `bytes` - An unsized byte array.
//! * `bytes/<len>` - A byte array of length `<len>`.
//!
//! <br>
//!
//! #### The `none` type
//!
//! The `none` type is a special type which any value can assume.
//!
//! Any comparison (`==`, `!=`, `<`, `>`, ..) _except_ `value is none` or `value is not none` is `false`.
//!
//! Any expression (`+`, `-`, `*`, `/`) involving a value of the `none` type results in another `none` type.
//! For example, `*value + 1` would be `none` if `*value` is not a valid pointer.
//!
//! A value of type `none` still retains the old type information to make sure it can be successfully refresh if needed.
//! Therefore you'll see things like:
//!
//! ```
//! none(u128)
//! ```
//!
//! Which means that an expression which was expected to evaluate to a `u128` value, evaluated to `none`.
//!
//! <br>
//!
//! ## Value Expressions
//!
//! Value expressions resolve to a specific _value_ in memory.
//! The take the following forms (the earlier it is listed, the higher its precedence):
//!
//! * `value` - The current as-we-are-scanning value of the memory location.
//! * `initial` - The initial value of the memory location, from the initial scan.
//! * `last` - The last value of the memory location, from the previous scan.
//! * `<number>` - A whole number literal. Default type is `u32`.
//!   * Example: `42`
//! * `<decimal>` - A decimal number literal. Default type is `f32`.
//!   * Example: `42.42`
//! * `<string>` - A string literal. Default type is `string/utf-8`.
//!   * Example: `"ui_boot"`
//! * `(<value>)` - Override default precedence.
//! * `*<value>` - Dereference the given value. This treats it as an address and follows the pointer.
//!   * Example: `*value`
//! * `&<value>` - Take the address of the given value. _not every value has an address_, like `&(value + 42)` is not valid.
//!   * Example: `&value`
//!   * Example: `*(&value + 0x40)`
//! * `<value> as <ty>` - Explicitly treat the value of `<value>` as the type `<ty>`.
//!   * Example: `value as u64`
//!   * Example: `value as u128 == 1`
//! * `<value> * <value>` - Multiply two values from each other.
//!   * Example: `value * 42`
//! * `<value> / <value>` - Divide two values.
//!   * Example: `value / 42`
//! * `<value> + <value>` - Add two values together.
//!   * Example: `value + 42`
//! * `<value> - <value>` - Subtract two values from each other.
//!   * Example: `value - 42`
//!
//! <br>
//!
//! ## Filters
//!
//! <br>
//!
//! #### Regular expressions `<a> ~ <b>`
//!
//! Test if `<a>` matches the regular expression specified in `<b>`.
//!
//! <br>
//!
//! #### Equality `<a> == <b>`
//!
//! Checks that `<a>` is equal to value `<b>`.
//!
//! For the initial scan, this allows for the following optimization:
//! * `value == 42` - scan in batches for the exact memory pattern of `42`
//!
//! <br>
//!
//! #### Non-equality `<a> != <b>`
//!
//! Checks that `<a>` is _not_ equal to value `<b>`.
//!
//! For the initial scan, this allows for the following optimization:
//! * `value != 0` - scans for non-zero memory addresses.
//!
//! <br>
//!
//! #### Less than `<a> < <b>`
//!
//! Checks that `<a>` is less than value `<b>`.
//!
//! For the initial scan, this allows for the following optimization:
//! * `value < 0` - scans for non-zero memory addresses.
//!
//! <br>
//!
//! #### Less than or equal `<a> <= <b>`
//!
//! Checks that `<a>` is less or equal to the value `<b>`.
//!
//! For the initial scan, this allows for the following optimization:
//! * `value <= 1` - scans for non-zero memory addresses.
//!
//! <br>
//!
//! #### Greater than `<a> > <b>`
//!
//! Checks that `<a>` is greater than the value `<b>`.
//!
//! For the initial scan, this allows for the following optimization:
//! * `value > 0` - scans for non-zero memory addresses.
//!
//! <br>
//!
//! #### Greater than or equal `<a> >= <b>`
//!
//! Checks that `<a>` is greater than or equal to the value `<b>`.
//!
//! For the initial scan, this allows for the following optimization:
//! * `value >= 1` - scans for non-zero memory addresses.
//!
//! <br>
//!
//! ## License
//!
//! This project bundles Adwaita icons parts of the GNOME Project under the Creative
//! Commons Attribution-Share Alike 3.0 license (see [licenses]).
//!
//! You can find them at http://www.gnome.org

use winapi::shared::minwindef::DWORD;

#[macro_use]
pub mod utils;
mod address;
mod address_range;
mod addresses;
mod alignment;
mod bitset;
mod encoding;
mod error;
mod filter_expr;
mod module;
mod offset;
mod pointer;
mod pointer_scan;
mod process;
mod process_handle;
mod progress_reporter;
mod raw_serde;
mod scanner;
mod sign;
mod size;
mod system;
mod thread;
mod token;
mod ty;
mod type_hint;
mod value;
mod value_expr;
mod values;

pub type ProcessId = DWORD;
pub type ThreadId = DWORD;
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// The endianness of a number.
#[derive(Debug, Clone, Copy)]
pub enum Endianness {
    LittleEndian,
    BigEndian,
}

#[derive(Debug, Clone, Copy)]
pub enum Cached<T> {
    Some(T),
    None,
}

pub use self::address::Address;
pub use self::address_range::AddressRange;
pub use self::addresses::Addresses;
pub use self::alignment::{Aligned, Alignment, Unaligned};
pub use self::bitset::BitSet;
pub use self::encoding::Encoding;
pub use self::error::Error;
pub use self::filter_expr::{FilterExpr, Special, Test, TypedFilterExpr, ValueInfo};
pub use self::module::Module;
pub use self::offset::Offset;
pub use self::pointer::{
    Base, FollowablePointer, Pointer, PointerInfo, PortableBase, PortablePointer,
};
pub use self::pointer_scan::{
    PointerScan, PointerScanBackreferenceProgress, PointerScanInitialProgress,
};
pub use self::process::{
    MemoryInformation, MemoryReader, MemoryState, MemoryType, PointerWidth, Process, ProcessInfo,
};
pub use self::process_handle::{
    AddressProxy, BufferProxy, InitialScanConfig, Location, ModuleInfo, ModulesState,
    ProcessHandle, ProcessName, ProcessThread, Proxy, ScanProgress, ValueHolder,
};
pub use self::raw_serde::{RawDeserialize, RawSerialize};
pub use self::scanner::{DefaultScanner, Scanner};
pub use self::sign::Sign;
pub use self::size::Size;
pub use self::system::processes;
pub use self::thread::Thread;
pub use self::token::Token;
pub use self::ty::Type;
pub use self::type_hint::{TypeHint, TypeSolveError};
pub use self::utils::IteratorExtension;
pub use self::value::{Value, ValueRef};
pub use self::value_expr::{TypedValueExpr, ValueExpr};
pub use self::values::Values;
