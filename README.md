# ptscan

[<img alt="github" src="https://img.shields.io/badge/github-udoprog/ptscan-8da0cb?style=for-the-badge&logo=github" height="20">](https://github.com/udoprog/ptscan)
[<img alt="crates.io" src="https://img.shields.io/crates/v/ptscan.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/ptscan)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-ptscan-66c2a5?style=for-the-badge&logoColor=white&logo=data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDUxMiA1MTIiPjxwYXRoIGZpbGw9IiNmNWY1ZjUiIGQ9Ik00ODguNiAyNTAuMkwzOTIgMjE0VjEwNS41YzAtMTUtOS4zLTI4LjQtMjMuNC0zMy43bC0xMDAtMzcuNWMtOC4xLTMuMS0xNy4xLTMuMS0yNS4zIDBsLTEwMCAzNy41Yy0xNC4xIDUuMy0yMy40IDE4LjctMjMuNCAzMy43VjIxNGwtOTYuNiAzNi4yQzkuMyAyNTUuNSAwIDI2OC45IDAgMjgzLjlWMzk0YzAgMTMuNiA3LjcgMjYuMSAxOS45IDMyLjJsMTAwIDUwYzEwLjEgNS4xIDIyLjEgNS4xIDMyLjIgMGwxMDMuOS01MiAxMDMuOSA1MmMxMC4xIDUuMSAyMi4xIDUuMSAzMi4yIDBsMTAwLTUwYzEyLjItNi4xIDE5LjktMTguNiAxOS45LTMyLjJWMjgzLjljMC0xNS05LjMtMjguNC0yMy40LTMzLjd6TTM1OCAyMTQuOGwtODUgMzEuOXYtNjguMmw4NS0zN3Y3My4zek0xNTQgMTA0LjFsMTAyLTM4LjIgMTAyIDM4LjJ2LjZsLTEwMiA0MS40LTEwMi00MS40di0uNnptODQgMjkxLjFsLTg1IDQyLjV2LTc5LjFsODUtMzguOHY3NS40em0wLTExMmwtMTAyIDQxLjQtMTAyLTQxLjR2LS42bDEwMi0zOC4yIDEwMiAzOC4ydi42em0yNDAgMTEybC04NSA0Mi41di03OS4xbDg1LTM4Ljh2NzUuNHptMC0xMTJsLTEwMiA0MS40LTEwMi00MS40di0uNmwxMDItMzguMiAxMDIgMzguMnYuNnoiPjwvcGF0aD48L3N2Zz4K" height="20">](https://docs.rs/ptscan)
[<img alt="build status" src="https://img.shields.io/github/actions/workflow/status/udoprog/ptscan/ci.yml?branch=main&style=for-the-badge" height="20">](https://github.com/udoprog/ptscan/actions?query=branch%3Amain)

ptscan finds the memory addresses behind the numbers in a running program, the
way Cheat Engine does, from a web UI in your browser on Windows and Linux. Attach
to a process, scan for a value, change it in the program, narrow the results
until one address is left, then watch it or write to it. When the program moves
it around between runs, a pointer scan finds a stable path back to it.

* **Typed scans.** Integers, floats, pointers, strings in any encoding and
  byte arrays, and the expression language below to narrow them with
  comparisons, arithmetic, dereferences and regular expressions.
* **Pointer scans.** Resolve addresses that move into module-relative pointer
  paths which survive restarts.
* **Fast.** Scans run in parallel over the whole address space, with the exact
  value patterns of initial scans searched in batches.
* **A browser UI.** `ptscan` serves a local web UI, so there is nothing
  to attach a debugger-style window to, and no GUI toolkit to install.
* **Written in Rust.** One native binary per platform, with a library you can
  embed in your own tools.

<br>

## Install

On Linux (`x86_64`), install the latest release into `~/.local/bin` (set
`PTSCAN_INSTALL_DIR` to change it):

```sh
curl -fsSL https://udoprog.github.io/ptscan/install.sh | sh
```

On Windows, download the zip from the
[GitHub releases](https://github.com/udoprog/ptscan/releases).

To update an existing installation to the latest release:

```sh
ptscan update
```

<br>

## Usage

`ptscan` serves the web UI and its websocket API (at `/ws`) on
`127.0.0.1:7431`, and opens the UI in the default browser on launch:

```sh
ptscan [--bind ADDR] [--db PATH] [--no-browser] [--no-tray]
```

Run `ptscan update` to replace the binary with the latest GitHub release.

Settings are kept in an SQLite database, created on first start:
`$XDG_DATA_HOME/ptscan/ptscan.db` (or `~/.local/share/ptscan/ptscan.db`) on
Linux and `%LOCALAPPDATA%\ptscan\ptscan.db` on Windows. `--db` uses another
file. Scan results are never stored; scanning again rebuilds them.

Work is kept in _sessions_: named workspaces holding the process they
target, the scan form, the pointer-scan settings and the watches of the
scratch list, saved as they change.
The session that was open last is opened again at start-up, and a
`Default` one is created when there are none. A session remembers its
process by name, so opening it does not attach; the UI offers to attach to
a running process of that name instead. Switching sessions detaches and
drops the scan and pointer-scan results.

On Windows it also shows a notification area icon whose menu opens the UI,
shows the attached process and quits the server; `--no-tray` leaves it out.

Scans and watches select values using _filters_, the expression language
described below.

On Linux, reading another process's memory needs ptrace access to it, such
as `kernel.yama.ptrace_scope = 0` or `CAP_SYS_PTRACE`. See
[Linux](https://github.com/udoprog/ptscan#linux) for details.

<br>

## Building

The server embeds the web UI built by [trug](https://github.com/udoprog/trug), so build
the UI first (it needs the `wasm32-unknown-unknown` target):

```sh
trug build --release
cargo build --release -p ptscan
```

Without the first step the server still builds, but serves a page saying the
UI is missing. Releases are packaged for Linux (`.tar.gz`) and Windows
(`.zip`) by [kick](https://github.com/udoprog/kick).

For development, run the server and serve the UI with `trug serve`, which
proxies `/ws` to the server:

```sh
cargo run -p ptscan -- --no-browser --dev-origin http://127.0.0.1:8080
trug serve
```

<br>

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

<br>

#### The `none` type

The `none` type is a special type which any value can assume.

Any comparison (`==`, `!=`, `<`, `>`, ..) _except_ `value is none` or `value is not none` is `false`.

Any expression (`+`, `-`, `*`, `/`) involving a value of the `none` type results in another `none` type.
For example, `*value + 1` would be `none` if `*value` is not a valid pointer.

A value of type `none` still retains the old type information to make sure it can be successfully refresh if needed.
Therefore you'll see things like:

```text
none(u128)
```

Which means that an expression which was expected to evaluate to a `u128` value, evaluated to `none`.

<br>

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

<br>

## Filters

<br>

#### Regular expressions `<a> ~ <b>`

Test if `<a>` matches the regular expression specified in `<b>`.

<br>

#### Equality `<a> == <b>`

Checks that `<a>` is equal to value `<b>`.

For the initial scan, this allows for the following optimization:
* `value == 42` - scan in batches for the exact memory pattern of `42`

<br>

#### Non-equality `<a> != <b>`

Checks that `<a>` is _not_ equal to value `<b>`.

For the initial scan, this allows for the following optimization:
* `value != 0` - scans for non-zero memory addresses.

<br>

#### Less than `<a> < <b>`

Checks that `<a>` is less than value `<b>`.

For the initial scan, this allows for the following optimization:
* `value < 0` - scans for non-zero memory addresses.

<br>

#### Less than or equal `<a> <= <b>`

Checks that `<a>` is less or equal to the value `<b>`.

For the initial scan, this allows for the following optimization:
* `value <= 1` - scans for non-zero memory addresses.

<br>

#### Greater than `<a> > <b>`

Checks that `<a>` is greater than the value `<b>`.

For the initial scan, this allows for the following optimization:
* `value > 0` - scans for non-zero memory addresses.

<br>

#### Greater than or equal `<a> >= <b>`

Checks that `<a>` is greater than or equal to the value `<b>`.

For the initial scan, this allows for the following optimization:
* `value >= 1` - scans for non-zero memory addresses.

<br>

## Linux

On Linux, ptscan reads process memory with `process_vm_readv`, falling back to
`/proc/<pid>/mem`. Both need ptrace access to the target process:

* With `kernel.yama.ptrace_scope = 0`, any process owned by the same user can be
  read. Set it with `sudo sysctl kernel.yama.ptrace_scope=0`, and put
  `kernel.yama.ptrace_scope = 0` in a file under `/etc/sysctl.d/` to keep it
  across reboots.
* With the common default of `1`, only processes started by ptscan can be read.
* Processes owned by other users, or any process under `ptrace_scope = 2`, need
  `CAP_SYS_PTRACE`: run ptscan as root or grant it with
  `sudo setcap cap_sys_ptrace=ep /path/to/ptscan`.
* `ptrace_scope = 3` forbids reading other processes entirely.

<br>

## License

This project bundles Adwaita icons parts of the GNOME Project under the Creative
Commons Attribution-Share Alike 3.0 license (see [licenses](https://github.com/udoprog/ptscan/tree/main/licenses)).

You can find them at <https://www.gnome.org>.
