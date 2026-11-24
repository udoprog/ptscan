//! `ptscan update`: replaces the running binary with the latest GitHub release.

use std::env::consts::{ARCH, EXE_SUFFIX, OS};
use std::fs::{self, File};
use std::io;
use std::path::{Component, Path, PathBuf};
use std::time::Duration;

use anyhow::{Context as _, Result, anyhow, bail};
use flate2::read::GzDecoder;
use serde::Deserialize;

const LATEST: &str = "https://api.github.com/repos/udoprog/ptscan/releases/latest";
const USER_AGENT: &str = concat!("ptscan/", env!("CARGO_PKG_VERSION"));

#[derive(Deserialize)]
struct Release {
    tag_name: String,
    assets: Vec<Asset>,
}

#[derive(Deserialize)]
struct Asset {
    name: String,
    browser_download_url: String,
}

/// Updates the running ptscan to the latest release, unless it is already up to date.
pub fn update() -> Result<()> {
    let current = env!("CARGO_PKG_VERSION");
    let client = reqwest::blocking::Client::builder()
        .user_agent(USER_AGENT)
        .connect_timeout(Duration::from_secs(30))
        .timeout(None)
        .build()
        .context("building HTTP client")?;

    let text = get(&client, LATEST)?
        .ok_or_else(|| anyhow!("no release found"))?
        .text()
        .with_context(|| format!("reading response from {LATEST}"))?;
    let release: Release =
        serde_json::from_str(&text).with_context(|| format!("parsing response from {LATEST}"))?;
    let latest = release.tag_name.trim_start_matches('v');
    if !is_newer(latest, current)? {
        println!("ptscan {current} is up to date");
        return Ok(());
    }

    let asset = select_asset(&release.assets, ARCH, OS)?;
    let exe = std::env::current_exe()
        .and_then(fs::canonicalize)
        .context("resolving current executable")?;
    let dir = exe
        .parent()
        .ok_or_else(|| anyhow!("current executable has no directory"))?;
    let work = dir.join(format!(".ptscan-update{}", std::process::id()));
    let _ = fs::remove_dir_all(&work);
    fs::create_dir_all(&work).with_context(|| format!("creating {}", work.display()))?;

    let result = (|| {
        println!("Downloading {}...", asset.name);
        let archive = work.join(&asset.name);
        download(&client, &asset.browser_download_url, &archive)?;
        let binary = format!("ptscan{EXE_SUFFIX}");
        let new = work.join(&binary);
        extract_binary(&archive, &binary, &new)?;
        replace(&new, &exe)
    })();

    let _ = fs::remove_dir_all(&work);
    result?;
    println!("ptscan updated from {current} to {latest}");
    Ok(())
}

fn get(
    client: &reqwest::blocking::Client,
    url: &str,
) -> Result<Option<reqwest::blocking::Response>> {
    let response = client
        .get(url)
        .send()
        .with_context(|| format!("requesting {url}"))?;
    if response.status() == reqwest::StatusCode::NOT_FOUND {
        return Ok(None);
    }
    response
        .error_for_status()
        .map(Some)
        .with_context(|| format!("requesting {url}"))
}

fn download(client: &reqwest::blocking::Client, url: &str, to: &Path) -> Result<()> {
    let mut response = get(client, url)?.ok_or_else(|| anyhow!("{url}: not found"))?;
    let mut file = File::create(to).with_context(|| format!("creating {}", to.display()))?;
    io::copy(&mut response, &mut file)
        .with_context(|| format!("downloading {url} to {}", to.display()))?;
    Ok(())
}

/// Whether the dotted numeric version `latest` is newer than `current`.
fn is_newer(latest: &str, current: &str) -> Result<bool> {
    Ok(parse_version(latest)? > parse_version(current)?)
}

fn parse_version(version: &str) -> Result<Vec<u64>> {
    let mut parts = version
        .split('.')
        .map(|p| {
            p.parse::<u64>()
                .map_err(|_| anyhow!("bad version `{version}`"))
        })
        .collect::<Result<Vec<_>>>()?;
    while parts.len() > 1 && parts.last() == Some(&0) {
        parts.pop();
    }
    Ok(parts)
}

/// The asset named `ptscan-<version>-<arch>-<os>.<ext>` for this platform.
fn select_asset<'a>(assets: &'a [Asset], arch: &str, os: &str) -> Result<&'a Asset> {
    let ext = match os {
        "windows" => "zip",
        "linux" => "tar.gz",
        _ => bail!("no prebuilt ptscan for {arch}-{os}"),
    };
    let suffix = format!("-{arch}-{os}.{ext}");
    assets
        .iter()
        .find(|a| a.name.starts_with("ptscan-") && a.name.ends_with(&suffix))
        .ok_or_else(|| anyhow!("no prebuilt ptscan for {arch}-{os}"))
}

fn extract_binary(archive: &Path, binary_name: &str, to: &Path) -> Result<()> {
    if archive.extension().is_some_and(|e| e == "zip") {
        let file = File::open(archive).with_context(|| format!("opening {}", archive.display()))?;
        let mut zip = zip::ZipArchive::new(io::BufReader::new(file))
            .with_context(|| format!("reading {}", archive.display()))?;
        let mut entry = zip
            .by_name(binary_name)
            .with_context(|| format!("{}: {binary_name}", archive.display()))?;
        let mut out = File::create(to).with_context(|| format!("creating {}", to.display()))?;
        io::copy(&mut entry, &mut out)
            .with_context(|| format!("extracting {binary_name} to {}", to.display()))?;
        return Ok(());
    }

    let file = File::open(archive).with_context(|| format!("opening {}", archive.display()))?;
    let mut tar = tar::Archive::new(GzDecoder::new(io::BufReader::new(file)));
    for entry in tar
        .entries()
        .with_context(|| format!("reading {}", archive.display()))?
    {
        let mut entry = entry.with_context(|| format!("reading {}", archive.display()))?;
        if !entry.header().entry_type().is_file() {
            continue;
        }
        let path = entry
            .path()
            .with_context(|| format!("reading {}", archive.display()))?
            .into_owned();
        let mut components = path.components().peekable();
        while matches!(components.peek(), Some(Component::CurDir)) {
            components.next();
        }
        let rel: PathBuf = components.collect();
        if !rel.components().all(|c| matches!(c, Component::Normal(_))) {
            continue;
        }
        if rel == Path::new(binary_name) {
            let mut out = File::create(to).with_context(|| format!("creating {}", to.display()))?;
            io::copy(&mut entry, &mut out)
                .with_context(|| format!("extracting {binary_name} to {}", to.display()))?;
            return Ok(());
        }
    }

    bail!("{}: no {binary_name} in the archive", archive.display())
}

/// Renames `new` over `exe`; on Windows the running exe is first moved aside to `ptscan.old.exe`.
fn replace(new: &Path, exe: &Path) -> Result<()> {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        fs::set_permissions(new, fs::Permissions::from_mode(0o755))
            .with_context(|| format!("setting permissions on {}", new.display()))?;
    }

    if cfg!(windows) {
        let old: PathBuf = exe.with_file_name("ptscan.old.exe");
        let _ = fs::remove_file(&old);
        fs::rename(exe, &old).with_context(|| format!("moving {} aside", exe.display()))?;
        if let Err(error) = fs::rename(new, exe) {
            let _ = fs::rename(&old, exe);
            return Err(error).with_context(|| format!("replacing {}", exe.display()));
        }
        return Ok(());
    }

    fs::rename(new, exe).with_context(|| format!("replacing {}", exe.display()))
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use super::*;

    fn assets(names: &[&str]) -> Vec<Asset> {
        names
            .iter()
            .map(|n| Asset {
                name: (*n).to_string(),
                browser_download_url: format!("https://example.com/{n}"),
            })
            .collect()
    }

    fn temp(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("ptscan-update-{name}-{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn compares_versions_numerically() {
        assert!(is_newer("0.1.0", "0.0.0").unwrap());
        assert!(is_newer("0.1.1", "0.1.0").unwrap());
        assert!(is_newer("0.10.0", "0.9.9").unwrap());
        assert!(is_newer("1.0", "0.9.9").unwrap());
        assert!(!is_newer("0.1.0", "0.1.0").unwrap());
        assert!(!is_newer("0.1", "0.1.0").unwrap());
        assert!(!is_newer("0.0.9", "0.1.0").unwrap());
        assert!(is_newer("x", "0.1.0").is_err());
    }

    #[test]
    fn selects_the_platform_asset() {
        let list = assets(&[
            "ptscan-0.2.0-x86_64-linux.tar.gz",
            "ptscan-0.2.0-x86_64-windows.zip",
            "checksums.txt",
        ]);
        let name = |arch, os| select_asset(&list, arch, os).map(|a| a.name.as_str());
        assert_eq!(
            name("x86_64", "linux").unwrap(),
            "ptscan-0.2.0-x86_64-linux.tar.gz"
        );
        assert_eq!(
            name("x86_64", "windows").unwrap(),
            "ptscan-0.2.0-x86_64-windows.zip"
        );
        assert_eq!(
            name("aarch64", "linux").unwrap_err().to_string(),
            "no prebuilt ptscan for aarch64-linux"
        );
        assert_eq!(
            name("x86_64", "macos").unwrap_err().to_string(),
            "no prebuilt ptscan for x86_64-macos"
        );
        assert_eq!(
            name("aarch64", "macos").unwrap_err().to_string(),
            "no prebuilt ptscan for aarch64-macos"
        );
    }

    #[test]
    fn extracts_binary_from_tar_gz() {
        let dir = temp("tar-gz");
        let archive = dir.join("ptscan-0.2.0-x86_64-linux.tar.gz");
        let mut builder = tar::Builder::new(flate2::write::GzEncoder::new(
            File::create(&archive).unwrap(),
            flate2::Compression::fast(),
        ));
        for (path, data) in [("README.md", &b"readme"[..]), ("ptscan", b"binary-bytes")] {
            let mut header = tar::Header::new_gnu();
            header.set_size(data.len() as u64);
            header.set_mode(0o755);
            header.set_cksum();
            builder.append_data(&mut header, path, data).unwrap();
        }
        builder.into_inner().unwrap().finish().unwrap();

        let out = dir.join("ptscan");
        extract_binary(&archive, "ptscan", &out).unwrap();
        assert_eq!(fs::read(&out).unwrap(), b"binary-bytes");

        let missing = dir.join("missing");
        let err = extract_binary(&archive, "ptscan.exe", &missing).unwrap_err();
        assert!(
            err.to_string().contains("no ptscan.exe in the archive"),
            "{err}"
        );

        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn extracts_binary_from_zip() {
        let dir = temp("zip");
        let archive = dir.join("ptscan-0.2.0-x86_64-windows.zip");
        {
            let mut zip = zip::ZipWriter::new(File::create(&archive).unwrap());
            let options = zip::write::SimpleFileOptions::default()
                .compression_method(zip::CompressionMethod::Deflated);
            zip.start_file("README.md", options).unwrap();
            zip.write_all(b"readme").unwrap();
            zip.start_file("ptscan.exe", options).unwrap();
            zip.write_all(b"exe-bytes").unwrap();
            zip.finish().unwrap();
        }

        let out = dir.join("ptscan.exe");
        extract_binary(&archive, "ptscan.exe", &out).unwrap();
        assert_eq!(fs::read(&out).unwrap(), b"exe-bytes");

        let missing = dir.join("missing");
        assert!(extract_binary(&archive, "ptscan", &missing).is_err());

        fs::remove_dir_all(&dir).unwrap();
    }
}
