//! [<img alt="github" src="https://img.shields.io/badge/github-udoprog/ptscan-8da0cb?style=for-the-badge&logo=github" height="20">](https://github.com/udoprog/ptscan)
//! [<img alt="crates.io" src="https://img.shields.io/crates/v/ptscan-gui.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/ptscan-gui)
//! [<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-ptscan--gui-66c2a5?style=for-the-badge&logoColor=white&logo=data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDUxMiA1MTIiPjxwYXRoIGZpbGw9IiNmNWY1ZjUiIGQ9Ik00ODguNiAyNTAuMkwzOTIgMjE0VjEwNS41YzAtMTUtOS4zLTI4LjQtMjMuNC0zMy43bC0xMDAtMzcuNWMtOC4xLTMuMS0xNy4xLTMuMS0yNS4zIDBsLTEwMCAzNy41Yy0xNC4xIDUuMy0yMy40IDE4LjctMjMuNCAzMy43VjIxNGwtOTYuNiAzNi4yQzkuMyAyNTUuNSAwIDI2OC45IDAgMjgzLjlWMzk0YzAgMTMuNiA3LjcgMjYuMSAxOS45IDMyLjJsMTAwIDUwYzEwLjEgNS4xIDIyLjEgNS4xIDMyLjIgMGwxMDMuOS01MiAxMDMuOSA1MmMxMC4xIDUuMSAyMi4xIDUuMSAzMi4yIDBsMTAwLTUwYzEyLjItNi4xIDE5LjktMTguNiAxOS45LTMyLjJWMjgzLjljMC0xNS05LjMtMjguNC0yMy40LTMzLjd6TTM1OCAyMTQuOGwtODUgMzEuOXYtNjguMmw4NS0zN3Y3My4zek0xNTQgMTA0LjFsMTAyLTM4LjIgMTAyIDM4LjJ2LjZsLTEwMiA0MS40LTEwMi00MS40di0uNnptODQgMjkxLjFsLTg1IDQyLjV2LTc5LjFsODUtMzguOHY3NS40em0wLTExMmwtMTAyIDQxLjQtMTAyLTQxLjR2LS42bDEwMi0zOC4yIDEwMiAzOC4ydi42em0yNDAgMTEybC04NSA0Mi41di03OS4xbDg1LTM4Ljh2NzUuNHptMC0xMTJsLTEwMiA0MS40LTEwMi00MS40di0uNmwxMDItMzguMiAxMDIgMzguMnYuNnoiPjwvcGF0aD48L3N2Zz4K" height="20">](https://docs.rs/ptscan-gui)
//!
//! This project is a work-in-progress GUI frontend for ptscan.
//!
//! It requires GTK 3.22 or higher, and is built using [gtk-rs](https://gtk-rs.org).
//!
//! ![Main Window](https://raw.githubusercontent.com/udoprog/ptscan/main/gfx/main_window.png)

#![feature(backtrace)]

#[macro_use]
mod macros;
mod clipboard;
mod error_handler;
mod memory;
pub mod prelude;
pub mod scan;
mod settings;
mod signal;
pub mod task;
pub mod ui;

pub use self::clipboard::{Clipboard, ClipboardBuffer, ClipboardBufferRef, ClipboardHandle};
pub use self::error_handler::ErrorHandler;
pub use self::memory::{CurrentScanResult, MemoryInfo, MemoryKind};
pub use self::scan::{Scan, ScanResult};
pub use self::settings::Settings;
