use std::env;
use std::path::Path;
use std::process::Command;

#[cfg(windows)]
fn build_project() {
    let path = env::var("PATH").unwrap();
    let path = format!("res\\win64\\bin;{}", path);

    let mut vars = Vec::new();
    vars.push(("SYSTEM_DEPS_GLIB_2_0_NO_PKG_CONFIG", "1"));
    vars.push(("SYSTEM_DEPS_GLIB_2_0_LIB", "glib-2.0"));
    vars.push(("SYSTEM_DEPS_GIO_2_0_NO_PKG_CONFIG", "1"));
    vars.push(("SYSTEM_DEPS_GIO_2_0_LIB", "gio-2.0"));
    vars.push(("SYSTEM_DEPS_GDK_PIXBUF_2_0_NO_PKG_CONFIG", "1"));
    vars.push(("SYSTEM_DEPS_GDK_PIXBUF_2_0_LIB", "gdk_pixbuf-2.0"));
    vars.push(("SYSTEM_DEPS_GDK_3_0_NO_PKG_CONFIG", "1"));
    vars.push(("SYSTEM_DEPS_GDK_3_0_LIB", "gdk-3.0"));
    vars.push(("SYSTEM_DEPS_GTK_3_0_NO_PKG_CONFIG", "1"));
    vars.push(("SYSTEM_DEPS_GTK_3_0_LIB", "gtk-3.0"));
    vars.push(("SYSTEM_DEPS_GOBJECT_2_0_NO_PKG_CONFIG", "1"));
    vars.push(("SYSTEM_DEPS_GOBJECT_2_0_LIB", "gobject-2.0"));
    vars.push(("SYSTEM_DEPS_CAIRO_NO_PKG_CONFIG", "1"));
    vars.push(("SYSTEM_DEPS_CAIRO_LIB", "cairo"));
    vars.push(("SYSTEM_DEPS_PANGO_NO_PKG_CONFIG", "1"));
    vars.push(("SYSTEM_DEPS_PANGO_LIB", "pango-1.0"));
    vars.push(("SYSTEM_DEPS_CAIRO_GOBJECT_NO_PKG_CONFIG", "1"));
    vars.push(("SYSTEM_DEPS_CAIRO_GOBJECT_LIB", "cairo-gobject"));
    vars.push(("SYSTEM_DEPS_ATK_NO_PKG_CONFIG", "1"));
    vars.push(("SYSTEM_DEPS_ATK_LIB", "atk-1.0"));

    vars.push(("RUSTFLAGS", "-L res\\win64\\lib"));

    let status = Command::new("cargo")
        .args(&["+nightly", "build", "--release"])
        .env("PATH", &path)
        .envs(vars)
        .status()
        .unwrap();

    assert!(status.success(), "cargo did not build successfully");
}

#[cfg(windows)]
fn build_installer() {
    let path = Path::new("installer").join("ptscan.iss");
    let status = Command::new("iscc").arg(&path).status().unwrap();

    assert!(status.success(), "compiler did not execute successful");
}

fn main() {
    build_project();
    build_installer();
}
