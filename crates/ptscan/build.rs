use std::path::Path;

fn main() {
    let dist = Path::new("../../target/dist");
    println!("cargo:rerun-if-changed={}", dist.display());

    if let Ok(entries) = std::fs::read_dir(dist) {
        for entry in entries.flatten() {
            println!("cargo:rerun-if-changed={}", entry.path().display());
        }
    }
}
