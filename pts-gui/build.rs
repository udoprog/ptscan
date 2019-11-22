fn main() -> anyhow::Result<()> {
    vcpkg::Config::new().copy_dlls(false).find_package("gtk")?;
    Ok(())
}
