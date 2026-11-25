//! Files the user exports to and imports from, such as a saved scratch list.
//!
//! A file is a JSON envelope `{"version": .., "kind": .., "value": ..}`, the
//! same style as stored settings, with a `kind` naming what it holds.

use std::fs::{self, File};
use std::io::{self, ErrorKind, Read as _, Write as _};
use std::path::Path;

use api::FileRefusal;
use musli::alloc::Global;
use musli::mode::Text;
use musli::{Decode, Encode};

/// Something written to and read from an exported file.
pub(crate) trait Export: Encode<Text> + for<'de> Decode<'de, Text, Global> {
    const KIND: &'static str;
    const VERSION: u32;
    /// The largest file read back.
    const MAX_SIZE: u64;
}

#[derive(Debug)]
pub(crate) struct Refused {
    pub(crate) refusal: FileRefusal,
    pub(crate) error: String,
}

impl Refused {
    fn new(refusal: FileRefusal, error: impl Into<String>) -> Self {
        Self {
            refusal,
            error: error.into(),
        }
    }

    fn io(error: io::Error) -> Self {
        let refusal = match error.kind() {
            ErrorKind::NotFound => FileRefusal::Missing,
            ErrorKind::PermissionDenied | ErrorKind::ReadOnlyFilesystem => FileRefusal::Denied,
            ErrorKind::IsADirectory => FileRefusal::Directory,
            ErrorKind::InvalidData => FileRefusal::Malformed,
            _ => FileRefusal::Failed,
        };

        Self::new(refusal, error.to_string())
    }
}

#[derive(Encode)]
struct EnvelopeRef<'a, T> {
    version: u32,
    kind: &'a str,
    value: &'a T,
}

#[derive(Decode)]
struct Header {
    version: u32,
    kind: String,
}

#[derive(Decode)]
struct Envelope<T> {
    value: T,
}

fn absolute(path: &str) -> Result<&Path, Refused> {
    let path = Path::new(path);

    if !path.is_absolute() {
        return Err(Refused::new(
            FileRefusal::Relative,
            format!("{} is not an absolute path", path.display()),
        ));
    }

    Ok(path)
}

/// Write `value` to `path` through a temporary file beside it, so a failed
/// write never leaves a partial file behind.
pub(crate) fn write<T>(path: &str, value: &T) -> Result<(), Refused>
where
    T: Export,
{
    let path = absolute(path)?;

    if path.is_dir() {
        return Err(Refused::new(
            FileRefusal::Directory,
            format!("{} is a directory", path.display()),
        ));
    }

    let Some(dir) = path.parent().filter(|dir| dir.is_dir()) else {
        return Err(Refused::new(
            FileRefusal::Missing,
            format!("the directory of {} does not exist", path.display()),
        ));
    };

    let envelope = EnvelopeRef {
        version: T::VERSION,
        kind: T::KIND,
        value,
    };

    let json = musli::json::to_vec_pretty(&envelope)
        .map_err(|error| Refused::new(FileRefusal::Failed, error.to_string()))?;

    let mut file = tempfile::Builder::new()
        .prefix(".ptscan-")
        .suffix(".tmp")
        .tempfile_in(dir)
        .map_err(Refused::io)?;

    file.write_all(&json).map_err(Refused::io)?;
    file.write_all(b"\n").map_err(Refused::io)?;
    file.as_file().sync_all().map_err(Refused::io)?;
    file.persist(path)
        .map_err(|error| Refused::io(error.error))?;
    Ok(())
}

/// Read a value written by [`write`].
pub(crate) fn read<T>(path: &str) -> Result<T, Refused>
where
    T: Export,
{
    let path = absolute(path)?;

    if fs::metadata(path).map_err(Refused::io)?.is_dir() {
        return Err(Refused::new(
            FileRefusal::Directory,
            format!("{} is a directory", path.display()),
        ));
    }

    let mut bytes = Vec::new();

    File::open(path)
        .map_err(Refused::io)?
        .take(T::MAX_SIZE.saturating_add(1))
        .read_to_end(&mut bytes)
        .map_err(Refused::io)?;

    if u64::try_from(bytes.len()).map_or(true, |len| len > T::MAX_SIZE) {
        return Err(Refused::new(
            FileRefusal::TooLarge,
            format!("files of this kind are at most {} bytes", T::MAX_SIZE),
        ));
    }

    let text = String::from_utf8(bytes)
        .map_err(|_| Refused::new(FileRefusal::Malformed, "not UTF-8 text"))?;

    decode(&text)
}

fn decode<T>(text: &str) -> Result<T, Refused>
where
    T: Export,
{
    let malformed =
        |error: musli::json::Error| Refused::new(FileRefusal::Malformed, error.to_string());

    let Header { version, kind } = musli::json::from_str(text).map_err(malformed)?;

    if kind != T::KIND {
        return Err(Refused::new(
            FileRefusal::WrongKind,
            format!("holds `{kind}`, not `{}`", T::KIND),
        ));
    }

    if version != T::VERSION {
        return Err(Refused::new(
            FileRefusal::UnsupportedVersion,
            format!("version {version}, this build reads {}", T::VERSION),
        ));
    }

    let Envelope { value } = musli::json::from_str(text).map_err(malformed)?;
    Ok(value)
}

#[cfg(test)]
mod tests {
    use api::FileRefusal;
    use musli::{Decode, Encode};

    use super::{Export, decode, read, write};

    #[derive(Debug, PartialEq, Encode, Decode)]
    struct Thing {
        name: String,
    }

    impl Export for Thing {
        const KIND: &'static str = "thing";
        const VERSION: u32 = 1;
        const MAX_SIZE: u64 = 128;
    }

    fn refusal(text: &str) -> FileRefusal {
        decode::<Thing>(text).unwrap_err().refusal
    }

    #[test]
    fn round_trip_and_replace() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("a.ptscan.json");
        let path = path.to_str().unwrap();

        write(path, &Thing { name: "one".into() }).unwrap();
        write(path, &Thing { name: "two".into() }).unwrap();
        assert_eq!(read::<Thing>(path).unwrap(), Thing { name: "two".into() });

        let names = std::fs::read_dir(dir.path())
            .unwrap()
            .map(|e| e.unwrap().file_name().into_string().unwrap())
            .collect::<Vec<_>>();
        assert_eq!(names, ["a.ptscan.json"]);
    }

    #[test]
    fn refusals() {
        assert_eq!(refusal(""), FileRefusal::Malformed);
        assert_eq!(
            refusal("{\"version\":1,\"kind\":\"thing\",\"val"),
            FileRefusal::Malformed
        );
        assert_eq!(refusal("[1, 2]"), FileRefusal::Malformed);
        assert_eq!(
            refusal("{\"version\":1,\"kind\":\"thing\"}"),
            FileRefusal::Malformed
        );
        assert_eq!(
            refusal("{\"version\":1,\"kind\":\"thing\",\"value\":{\"name\":3}}"),
            FileRefusal::Malformed
        );
        assert_eq!(
            refusal("{\"version\":1,\"kind\":\"other\",\"value\":{}}"),
            FileRefusal::WrongKind
        );
        assert_eq!(
            refusal("{\"version\":2,\"kind\":\"thing\",\"value\":{}}"),
            FileRefusal::UnsupportedVersion
        );

        let dir = tempfile::tempdir().unwrap();
        let dir_path = dir.path().to_str().unwrap();
        let missing = dir.path().join("missing").join("a.json");
        let missing = missing.to_str().unwrap();
        let big = dir.path().join("big.json");
        std::fs::write(&big, [b' '; 129]).unwrap();
        let binary = dir.path().join("binary.json");
        std::fs::write(&binary, [0xff, 0xfe]).unwrap();

        let read_refusal = |path: &str| read::<Thing>(path).unwrap_err().refusal;
        let write_refusal = |path: &str| {
            write(path, &Thing { name: "x".into() })
                .unwrap_err()
                .refusal
        };

        assert_eq!(read_refusal("relative.json"), FileRefusal::Relative);
        assert_eq!(read_refusal(missing), FileRefusal::Missing);
        assert_eq!(read_refusal(dir_path), FileRefusal::Directory);
        assert_eq!(read_refusal(big.to_str().unwrap()), FileRefusal::TooLarge);
        assert_eq!(
            read_refusal(binary.to_str().unwrap()),
            FileRefusal::Malformed
        );
        assert_eq!(write_refusal("relative.json"), FileRefusal::Relative);
        assert_eq!(write_refusal(missing), FileRefusal::Missing);
        assert_eq!(write_refusal(dir_path), FileRefusal::Directory);
    }
}
