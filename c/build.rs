use std::{env, error, path::PathBuf};

fn main() -> Result<(), Box<error::Error>> {
    let crate_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR")?);

    cbindgen::Builder::new()
        .with_crate(&crate_dir)
        .with_config(cbindgen::Config::from_file(
            crate_dir.join("cbindgen.toml"),
        )?)
        .generate()?
        .write_to_file(crate_dir.join("include").join("ptscan.hpp"));

    Ok(())
}
