# Commands

* `help` - print this help message
* `exit` - exit the application
* `scans [list]` - list active scans
* `scans new <name>` - create and switch to the scanner named `<name>`.
* `scans del <name>` - delete the scan named `<name>`.
* `del <address>` - delete scan result matching `<address>`.
* `print, p [limit]` - print the current scan, limiting the number of results to `[limit]`.
* `process, ps` - print information about the current process
* `scan <predicate>` - scan using the given `<predicate>`
* `set <address> <value>` - set the value of the given memory location

# <predicate>

`<predicate>` can be one of:

* `eq <value>` - find address with value equal to `<value>`.
* `gt <value>` - find address with value greater-than `<value>`.
* `gte <value>` - find address with value greater-than or equal to `<value>`.
* `lt <value>` - find address with value less-than `<value>`.
* `lte <value>` - find address with value less-than or equal to `<value>`.
* `dec`, `smaller` - find an address with a decreased value since last scan.
* `inc`, `bigger` - find an address with an increased value since last scan.
* `changed` - find an address with an increased value since last scan.
* `pointer` - scan for a pointer (see \What is a pointer?\).

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