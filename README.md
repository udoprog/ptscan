# ptscan

A memory scanner for Windows, written in Rust.

## Usage

ptscan is a cli tool which allows you to scan and dissect memory using _filters_.

These filters are used in commands such as `scan` and `watch`.

## Types

All expressions support the following types:

* `none` - The special `none` type. Any value of this type is _undefined_. See the section below for more details.
* `pointer` -  A pointer value, who's size depends on the process being attached to.
* `u8` - An unsigned 8-bit number.
* `i8` - A signed 8-bit number.
* `u16` - An unsigned 16-bit number.
* `i16` - A signed 16-bit number.
* `u32` - An unsigned 32-bit number.
* `i32` - A signed 32-bit number.
* `u64` - An unsigned 64-bit number.
* `i64` - A signed 64-bit number.
* `u128` - An unsigned 128-bit number.
* `i128` - A signed 128-bit number.
* `f32` - A 32-bit floating point number.
* `f64` - A 64-bit floating point number.
* `string` - A null-terminated string with the default encoding (`utf-8`).
* `string/<encoding>` - A null-terminated string with the specified `<encoding>`, as per [the whatwg Encoding standard](https://encoding.spec.whatwg.org/#names-and-labels).
* `bytes` - An unsized byte array.
* `bytes/<len>` - A byte array of length `<len>`.

#### The `none` type

The `none` type is a special type which any value can assume.

Any comparison (`==`, `!=`, `<`, `>`, ..) _except_ `value is none` or `value is not none` is `false`.

Any expression (`+`, `-`, `*`, `/`) involving a value of the `none` type results in another `none` type.
For example, `*value + 1` would be `none` if `*value` is not a valid pointer.

A value of type `none` still retains the old type information to make sure it can be successfully refresh if needed.
Therefore you'll see things like:

```
none(u128)
```

Which means that an expression which was expected to evaluate to a `u128` value, evaluated to `none`.

## Value Expressions

Value expressions resolve to a specific _value_ in memory.
The take the following forms (the earlier it is listed, the higher its precedence):

* `value` - The current as-we-are-scanning value of the memory location.
* `initial` - The initial value of the memory location, from the initial scan.
* `last` - The last value of the memory location, from the previous scan.
* `<number>` - A whole number literal. Default type is `u32`.
  * Example: `42`
* `<decimal>` - A decimal number literal. Default type is `f32`.
  * Example: `42.42`
* `<string>` - A string literal. Default type is `string/utf-8`.
  * Example: `"ui_boot"`
* `(<value>)` - Override default precedence.
* `*<value>` - Dereference the given value. This treats it as an address and follows the pointer.
  * Example: `*value`
* `&<value>` - Take the address of the given value. _not every value has an address_, like `&(value + 42)` is not valid.
  * Example: `&value`
  * Example: `*(&value + 0x40)`
* `<value> as <ty>` - Explicitly treat the value of `<value>` as the type `<ty>`.
  * Example: `value as u64`
  * Example: `value as u128 == 1`
* `<value> * <value>` - Multiply two values from each other.
  * Example: `value * 42`
* `<value> / <value>` - Divide two values.
  * Example: `value / 42`
* `<value> + <value>` - Add two values together.
  * Example: `value + 42`
* `<value> - <value>` - Subtract two values from each other.
  * Example: `value - 42`

## Filters

#### Equality `<a> == <b>`

Checks that `<a>` is equal to value `<b>`.

For the initial scan, this allows for the following optimization:
* `value == 42` - scan in batches for the exact memory pattern of `42`

#### Non-equality `<a> != <b>`

Checks that `<a>` is _not_ equal to value `<b>`.

For the initial scan, this allows for the following optimization:
* `value != 0` - scans for non-zero memory addresses.

#### Less than `<a> < <b>`

Checks that `<a>` is less than value `<b>`.

For the initial scan, this allows for the following optimization:
* `value < 0` - scans for non-zero memory addresses.

#### Less than or equal `<a> <= <b>`

Checks that `<a>` is less or equal to the value `<b>`.

For the initial scan, this allows for the following optimization:
* `value <= 1` - scans for non-zero memory addresses.

#### Greater than `<a> > <b>`

Checks that `<a>` is greater than the value `<b>`.

For the initial scan, this allows for the following optimization:
* `value > 0` - scans for non-zero memory addresses.

#### Greater than or equal `<a> >= <b>`

Checks that `<a>` is greater than or equal to the value `<b>`.

For the initial scan, this allows for the following optimization:
* `value >= 1` - scans for non-zero memory addresses.