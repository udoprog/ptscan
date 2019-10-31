# <predicate>

`<predicate>` can be one of:

* `<predicate> and <predicate>` - find address which matches both of the specified predicates.
* `<predicate> or <predicate>` - find address which matches any of the specified predicates.
* `value == <value>` - find address with value equal to `<value>`.
* `value > <value>` - find address with value greater-than `<value>`.
* `value >= <value>` - find address with value greater-than or equal to `<value>`.
* `value < <value>` - find address with value less-than `<value>`.
* `value <= <value>` - find address with value less-than or equal to `<value>`.
* `dec` - find an address with a decreased value since last scan.
* `inc` - find an address with an increased value since last scan.
* `changed` - find values which have changed since the last scan.
* `changed` - find values which are the same since last scan.
* `value is pointer` - scan for a pointer (see \What is a pointer?\).

# `<value>`

A `<value>` is a literal value. It can take the following forms:

* `0xFFFFFFFF[type]`
* `1000000[type]`

Where `[type]` is an optional suffix indicating the type of the value.
`[type]` can be one of: `u128`, `i128`, `u64`, `i64`, `u32`, `i32`, `u16`, `i16`, `u8`, `i8`.

# What is a pointer?

A pointer is a memory address which:

* has the width of a pointer (typically 4 or 8 bytes).
* points to a memory address that is accessible (i.e. not a stack guard).