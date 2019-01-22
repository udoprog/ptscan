# ptscan

A pointer scanner for Windows application written in Rust.

## Usage

Note: can currently only (badly) scan for a u32 value with `0xDEADBEEF`:

```bash
cargo run --release -- --process <name>
```