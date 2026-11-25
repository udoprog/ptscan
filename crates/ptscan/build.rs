use std::env;
use std::path::Path;
use std::process::ExitCode;

fn main() -> ExitCode {
    let dist = Path::new("../../target/dist");
    println!("cargo:rerun-if-changed={}", dist.display());

    if let Ok(entries) = std::fs::read_dir(dist) {
        for entry in entries.flatten() {
            println!("cargo:rerun-if-changed={}", entry.path().display());
        }
    }

    // Debug builds, tests and lints may run without the UI and serve a
    // placeholder page; a release build without it is never useful.
    if env::var("PROFILE").is_ok_and(|p| p == "release") && !dist.join("index.html").is_file() {
        eprintln!(
            "error: the ptscan web UI is not built. Run `trug build --release` in the \
             repository root (it needs the wasm32-unknown-unknown target), then build ptscan again."
        );
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
