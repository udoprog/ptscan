use std::env;
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::process::Command;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn version() -> Result<String> {
    use std::fs;
    use toml::Value;

    let value = fs::read_to_string(Path::new("crates").join("ptscan-gui").join("Cargo.toml"))?;
    let value: Value = toml::from_str(&value)?;

    let version = value
        .get("package")
        .and_then(|v| v.get("version"))
        .and_then(|v| v.as_str())
        .ok_or_else(|| "missing `[package] version = \"..\"` in manifest")?;

    Ok(version.to_owned())
}

#[cfg(windows)]
fn build_project(debug: bool) -> Result<()> {
    let path = {
        let mut b = OsString::from(Path::new("res").join("win64").join("bin"));
        b.push(";");

        if let Some(path) = env::var_os("PATH") {
            b.push(path);
        }

        b
    };

    let rust_flags = {
        let mut b = OsString::from("-L ");
        b.push(Path::new("res").join("win64").join("lib"));
        b
    };

    let mut c = Command::new("cargo");

    macro_rules! lib {
        ($s:literal, $lib:literal) => {{
            c.env(concat!("SYSTEM_DEPS_", $s, "_LIB"), $lib);
            c.env(concat!("SYSTEM_DEPS_", $s, "_NO_PKG_CONFIG"), "1");
        }};
    }

    lib!("ATK", "atk-1.0");
    lib!("CAIRO_GOBJECT", "cairo-gobject");
    lib!("CAIRO", "cairo");
    lib!("GDK_3_0", "gdk-3.0");
    lib!("GDK_PIXBUF_2_0", "gdk_pixbuf-2.0");
    lib!("GIO_2_0", "gio-2.0");
    lib!("GLIB_2_0", "glib-2.0");
    lib!("GOBJECT_2_0", "gobject-2.0");
    lib!("GTK_3_0", "gtk-3.0");
    lib!("PANGO", "pango-1.0");
    c.env("RUSTFLAGS", rust_flags);
    c.env("PATH", path);
    c.args(&["+nightly", "build"]);

    if !debug {
        c.arg("--release");
    }

    println!("{:?}", c);

    let status = c.status()?;

    if !status.success() {
        return Err("cargo did not build successfully".into());
    }

    Ok(())
}

#[cfg(windows)]
fn build_installer(iscc: &Path) -> Result<()> {
    let mut c = Command::new(iscc);
    c.arg(Path::new("installer").join("ptscan.iss"));
    c.arg(format!("/DVersion={}", version()?));

    println!("{:?}", c);

    let status = c.status()?;

    if !status.success() {
        return Err("cargo did not build successfully".into());
    }

    Ok(())
}

fn main() -> Result<()> {
    let mut debug = false;
    let mut iscc = PathBuf::from("iscc");

    let mut it = env::args();
    it.next();

    while let Some(a) = it.next() {
        match a.as_str() {
            "--debug" => {
                debug = true;
            }
            "--iscc" => {
                iscc = PathBuf::from(it.next().ok_or_else(|| "missing argument to `--iscc`")?);
            }
            "--help" => {
                println!("Usage: builder [--iscc <path>] [--debug]");
                return Ok(());
            }
            other => {
                println!("Usage: builder [--iscc <path>] [--debug]");
                return Err(format!("unsupported argument: {}", other).into());
            }
        }
    }

    build_project(debug)?;
    build_installer(&iscc)?;
    Ok(())
}
