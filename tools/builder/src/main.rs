use std::env;
use std::ffi::{OsStr, OsString};
use std::path::Path;
use std::process::Command;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[cfg(windows)]
fn build_project(remaining: impl IntoIterator<Item: AsRef<OsStr>>) -> Result<()> {
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
    c.args(remaining);

    let status = c.status()?;

    if !status.success() {
        return Err(status.to_string().into());
    }

    Ok(())
}

fn main() -> Result<()> {
    build_project(env::args().skip(1))?;
    Ok(())
}
