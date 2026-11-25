//! Files the user exports to and imports from, such as a saved scratch list.
//!
//! A file is a JSON envelope `{"version": .., "kind": .., "value": ..}`, the
//! same style as stored settings, with a `kind` naming what it holds.

use std::fs::{self, File};
use std::io::{self, BufWriter, ErrorKind, Read as _};
use std::path::Path;

use api::FileRefusal;
use musli::alloc::Global;
use musli::mode::Text;
use musli::{Decode, Encode};

/// What an exported file holds.
pub(crate) trait Export {
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
    pub(crate) fn new(refusal: FileRefusal, error: impl Into<String>) -> Self {
        Self {
            refusal,
            error: error.into(),
        }
    }

    pub(crate) fn io(error: io::Error) -> Self {
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

/// The path a file can be written to and the directory it is in.
pub(crate) fn writable(path: &str) -> Result<(&Path, &Path), Refused> {
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

    Ok((path, dir))
}

/// The path of a file that can be read.
pub(crate) fn readable(path: &str) -> Result<&Path, Refused> {
    let path = absolute(path)?;

    if fs::metadata(path).map_err(Refused::io)?.is_dir() {
        return Err(Refused::new(
            FileRefusal::Directory,
            format!("{} is a directory", path.display()),
        ));
    }

    Ok(path)
}

/// Write `value` to `path` through a temporary file beside it, so a failed
/// write never leaves a partial file behind.
pub(crate) fn write<T>(path: &str, value: &T) -> Result<(), Refused>
where
    T: Export + Encode<Text>,
{
    let envelope = EnvelopeRef {
        version: T::VERSION,
        kind: T::KIND,
        value,
    };

    let json = musli::json::to_vec_pretty(&envelope)
        .map_err(|error| Refused::new(FileRefusal::Failed, error.to_string()))?;

    atomic(path, |out| {
        out.write_all(&json).map_err(Refused::io)?;
        out.write_all(b"\n").map_err(Refused::io)?;
        Ok(true)
    })?;

    Ok(())
}

/// Write a file of kind `T` whose value `body` writes as JSON, piece by piece,
/// through a temporary file like [`write`]. When `body` returns `false` the
/// write is abandoned, leaving `path` as it was, and so is this.
pub(crate) fn write_streamed<T, F>(path: &str, body: F) -> Result<bool, Refused>
where
    T: Export,
    F: FnOnce(&mut dyn io::Write) -> Result<bool, Refused>,
{
    atomic(path, |out| {
        let kind = musli::json::to_string(T::KIND)
            .map_err(|error| Refused::new(FileRefusal::Failed, error.to_string()))?;

        write!(
            out,
            "{{\n  \"version\": {},\n  \"kind\": {kind},\n  \"value\": ",
            T::VERSION
        )
        .map_err(Refused::io)?;

        if !body(out)? {
            return Ok(false);
        }

        out.write_all(b"\n}\n").map_err(Refused::io)?;
        Ok(true)
    })
}

fn atomic<F>(path: &str, body: F) -> Result<bool, Refused>
where
    F: FnOnce(&mut dyn io::Write) -> Result<bool, Refused>,
{
    let (path, dir) = writable(path)?;

    let mut builder = tempfile::Builder::new();
    builder.prefix(".ptscan-").suffix(".tmp");

    // tempfile creates files owner-only (0600). Give the file the mode a
    // plain `File::create` would (0666 less the umask, which `open` applies),
    // or keep the mode of the file being replaced.
    #[cfg(unix)]
    let existing = {
        use std::os::unix::fs::PermissionsExt;
        builder.permissions(fs::Permissions::from_mode(0o666));
        fs::metadata(path).ok().map(|m| m.permissions())
    };

    let file = builder.tempfile_in(dir).map_err(Refused::io)?;

    #[cfg(unix)]
    if let Some(permissions) = existing {
        file.as_file()
            .set_permissions(permissions)
            .map_err(Refused::io)?;
    }

    let mut out = BufWriter::with_capacity(1 << 16, file);

    // Dropping the temporary file removes it.
    if !body(&mut out)? {
        return Ok(false);
    }

    let file = out
        .into_inner()
        .map_err(|error| Refused::io(error.into_error()))?;
    file.as_file().sync_all().map_err(Refused::io)?;
    file.persist(path)
        .map_err(|error| Refused::io(error.error))?;
    Ok(true)
}

/// Read a value written by [`write`].
pub(crate) fn read<T>(path: &str) -> Result<T, Refused>
where
    T: Export + for<'de> Decode<'de, Text, Global>,
{
    let bytes = read_bytes::<T>(path, |_, _| true)?.unwrap_or_default();
    let text = String::from_utf8(bytes)
        .map_err(|_| Refused::new(FileRefusal::Malformed, "not UTF-8 text"))?;
    decode(&text)
}

/// Read the bytes of a file of kind `T`, telling `progress` how many have
/// been read of how many; it returns `false` to stop, and then this returns
/// `None`.
pub(crate) fn read_bytes<T>(
    path: &str,
    mut progress: impl FnMut(u64, u64) -> bool,
) -> Result<Option<Vec<u8>>, Refused>
where
    T: Export,
{
    const CHUNK: usize = 1 << 20;

    let path = readable(path)?;
    let file = File::open(path).map_err(Refused::io)?;
    let size = file.metadata().map_err(Refused::io)?.len();
    let too_large = || {
        Refused::new(
            FileRefusal::TooLarge,
            format!("files of this kind are at most {} bytes", T::MAX_SIZE),
        )
    };

    if size > T::MAX_SIZE {
        return Err(too_large());
    }

    let mut file = file.take(T::MAX_SIZE.saturating_add(1));
    let mut bytes = Vec::with_capacity(usize::try_from(size).unwrap_or(0));
    let mut chunk = vec![0; CHUNK];

    loop {
        let n = match file.read(&mut chunk) {
            Ok(0) => break,
            Ok(n) => n,
            Err(error) if error.kind() == ErrorKind::Interrupted => continue,
            Err(error) => return Err(Refused::io(error)),
        };

        bytes.extend_from_slice(chunk.get(..n).unwrap_or_default());

        let read = u64::try_from(bytes.len()).unwrap_or(u64::MAX);

        if read > T::MAX_SIZE {
            return Err(too_large());
        }

        if !progress(read, size.max(read)) {
            return Ok(None);
        }
    }

    Ok(Some(bytes))
}

pub(crate) fn decode<T>(text: &str) -> Result<T, Refused>
where
    T: Export + for<'de> Decode<'de, Text, Global>,
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

    #[cfg(unix)]
    #[test]
    fn modes() {
        use super::{Refused, write_streamed};
        use std::os::unix::fs::PermissionsExt;
        use std::path::Path;

        let mode = |path: &Path| std::fs::metadata(path).unwrap().permissions().mode() & 0o777;

        let dir = tempfile::tempdir().unwrap();
        let probe = dir.path().join("probe");
        std::fs::File::create(&probe).unwrap();

        let new = dir.path().join("new.json");
        write(new.to_str().unwrap(), &Thing { name: "x".into() }).unwrap();
        assert_eq!(mode(&new), mode(&probe));

        let old = dir.path().join("old.json");
        std::fs::write(&old, b"").unwrap();
        std::fs::set_permissions(&old, std::fs::Permissions::from_mode(0o640)).unwrap();
        write(old.to_str().unwrap(), &Thing { name: "x".into() }).unwrap();
        assert_eq!(mode(&old), 0o640);

        let streamed = dir.path().join("streamed.json");
        std::fs::write(&streamed, b"").unwrap();
        std::fs::set_permissions(&streamed, std::fs::Permissions::from_mode(0o604)).unwrap();
        write_streamed::<Thing, _>(streamed.to_str().unwrap(), |out| {
            out.write_all(b"{}").map_err(Refused::io)?;
            Ok(true)
        })
        .unwrap();
        assert_eq!(mode(&streamed), 0o604);
    }
}
