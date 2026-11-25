//! The paths of a pointer scan as saved to a file.
//!
//! Paths refer to modules by index into the file's module names, as they do
//! in memory, so a large file does not repeat a name per path and loading it
//! needs no process. A file looks like:
//!
//! ```json
//! {
//!   "version": 1,
//!   "kind": "pointer-scan",
//!   "value": {
//!     "value_type": "u32",
//!     "target": "\"game.exe\" + 0x10 -> 0x8",
//!     "settings": {"max_depth": 7, "max_offset": 4096, "max_paths": 1000000},
//!     "truncated": false,
//!     "modules": ["game.exe"],
//!     "paths": [
//!       {"module":0,"offset":4660,"offsets":[16,-8]}
//!     ]
//!   }
//! }
//! ```
//!
//! A path with an `address` instead of a `module` starts at that address,
//! one with neither at an unknown base. Offsets are signed decimals.

use std::io;

use api::FileRefusal;
use musli::mode::Text;
use musli::{Decode, Encode};
use ptscan_core::{Address, Base, Offset, Pointer, Sign, Size, Token, Type};

use super::Paths;
use crate::export::{Export, Refused};

/// How often a long save or load reports progress and checks for a cancel.
pub(super) const STEP: usize = 1 << 16;

/// What a file says of the paths beside the paths themselves.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(super) struct Meta {
    pub(super) value_type: Option<String>,
    /// The pointer scanned for.
    pub(super) target: Option<String>,
    pub(super) settings: Option<api::PointerScanSettings>,
}

#[derive(Debug, Encode, Decode)]
struct FileSettings {
    max_depth: u32,
    max_offset: u32,
    max_paths: u32,
}

#[derive(Debug, Decode)]
pub(super) struct PointerFile {
    #[musli(default)]
    value_type: Option<String>,
    #[musli(default)]
    target: Option<String>,
    #[musli(default)]
    settings: Option<FileSettings>,
    #[musli(default)]
    truncated: bool,
    modules: Vec<String>,
    paths: Vec<FilePath>,
}

#[derive(Debug, Decode)]
struct FilePath {
    #[musli(default)]
    module: Option<u16>,
    #[musli(default)]
    address: Option<u64>,
    /// From the module.
    #[musli(default)]
    offset: i64,
    offsets: Vec<i64>,
}

impl Export for PointerFile {
    const KIND: &'static str = "pointer-scan";
    const VERSION: u32 = 1;
    /// About 20 million paths.
    const MAX_SIZE: u64 = 1 << 30;
}

fn failed(error: io::Error) -> Refused {
    Refused::io(error)
}

fn encoded<T>(value: &T) -> Result<String, Refused>
where
    T: ?Sized + Encode<Text>,
{
    musli::json::to_string(value)
        .map_err(|error| Refused::new(FileRefusal::Failed, error.to_string()))
}

fn to_i64(offset: Offset) -> Result<i64, Refused> {
    let magnitude = i64::try_from(u64::from(offset.abs())).map_err(|_| {
        Refused::new(
            FileRefusal::Failed,
            format!("offset {offset} does not fit in a file"),
        )
    })?;

    Ok(match offset.sign() {
        Sign::Minus => magnitude.saturating_neg(),
        Sign::Plus | Sign::NoSign => magnitude,
    })
}

fn from_i64(value: i64) -> Offset {
    match value {
        0 => Offset::zero(),
        1.. => Offset::new(Sign::Plus, Size::new(value.unsigned_abs())),
        _ => Offset::new(Sign::Minus, Size::new(value.unsigned_abs())),
    }
}

/// Write the value of a pointer-scan file: `meta`, then the paths, telling
/// `progress` how many have been written every [`STEP`]. Returns `false`
/// once `token` is set.
pub(super) fn write_value(
    out: &mut dyn io::Write,
    paths: &Paths,
    meta: &Meta,
    token: &Token,
    progress: &mut dyn FnMut(u64),
) -> Result<bool, Refused> {
    out.write_all(b"{\n").map_err(failed)?;

    if let Some(value_type) = &meta.value_type {
        let value_type = encoded(value_type)?;
        writeln!(out, "    \"value_type\": {value_type},").map_err(failed)?;
    }

    if let Some(target) = &meta.target {
        let target = encoded(target)?;
        writeln!(out, "    \"target\": {target},").map_err(failed)?;
    }

    if let Some(s) = &meta.settings {
        writeln!(
            out,
            "    \"settings\": {{\"max_depth\": {}, \"max_offset\": {}, \"max_paths\": {}}},",
            s.max_depth, s.max_offset, s.max_paths
        )
        .map_err(failed)?;
    }

    writeln!(out, "    \"truncated\": {},", paths.truncated).map_err(failed)?;

    let modules = encoded(&paths.modules)?;
    writeln!(out, "    \"modules\": {modules},").map_err(failed)?;
    out.write_all(b"    \"paths\": [").map_err(failed)?;

    for (index, pointer) in paths.pointers.iter().enumerate() {
        if index % STEP == 0 {
            if token.test() {
                return Ok(false);
            }

            progress(index as u64);
        }

        let separator = if index == 0 { "\n      " } else { ",\n      " };
        out.write_all(separator.as_bytes()).map_err(failed)?;

        match pointer.base {
            Base::Null => write!(out, "{{"),
            Base::Module { id, offset } => {
                write!(out, "{{\"module\":{id},\"offset\":{},", to_i64(offset)?)
            }
            Base::Address { address } => {
                write!(out, "{{\"address\":{},", u128::from(address))
            }
        }
        .map_err(failed)?;

        out.write_all(b"\"offsets\":[").map_err(failed)?;

        for (n, offset) in pointer.offsets.iter().enumerate() {
            let comma = if n == 0 { "" } else { "," };
            write!(out, "{comma}{}", to_i64(*offset)?).map_err(failed)?;
        }

        out.write_all(b"]}").map_err(failed)?;
    }

    if token.test() {
        return Ok(false);
    }

    progress(paths.pointers.len() as u64);
    out.write_all(b"\n    ]\n  }").map_err(failed)?;
    Ok(true)
}

/// What a file holds, ready to page through. Returns `None` once `token` is
/// set.
pub(super) fn into_paths(
    file: PointerFile,
    token: &Token,
    progress: &mut dyn FnMut(u64),
) -> Result<Option<(Paths, Meta, Option<Type>)>, Refused> {
    let malformed = |error: String| Refused::new(FileRefusal::Malformed, error);

    if file.modules.len() > usize::from(u16::MAX) + 1 {
        return Err(malformed(format!(
            "{} modules, at most {} are supported",
            file.modules.len(),
            usize::from(u16::MAX) + 1
        )));
    }

    let value_type = match &file.value_type {
        Some(text) => Some(
            crate::scratch::parse_type(text)
                .map_err(|error| malformed(format!("type `{text}`: {error}")))?,
        ),
        None => None,
    };

    let mut pointers = Vec::with_capacity(file.paths.len());

    for (index, path) in file.paths.into_iter().enumerate() {
        if index % STEP == 0 {
            if token.test() {
                return Ok(None);
            }

            progress(index as u64);
        }

        let base = match (path.module, path.address) {
            (Some(id), None) => {
                if usize::from(id) >= file.modules.len() {
                    return Err(malformed(format!(
                        "path {index} refers to module {id}, the file names {}",
                        file.modules.len()
                    )));
                }

                Base::Module {
                    id,
                    offset: from_i64(path.offset),
                }
            }
            (None, Some(address)) => Base::Address {
                address: Address::from(address),
            },
            (None, None) => Base::Null,
            (Some(_), Some(_)) => {
                return Err(malformed(format!(
                    "path {index} has both a module and an address"
                )));
            }
        };

        pointers.push(Pointer::new(base, path.offsets.into_iter().map(from_i64)));
    }

    let meta = Meta {
        value_type: file.value_type,
        target: file.target,
        settings: file.settings.map(|s| api::PointerScanSettings {
            max_depth: s.max_depth,
            max_offset: s.max_offset,
            max_paths: s.max_paths,
        }),
    };

    let paths = Paths {
        pointers,
        modules: file.modules,
        truncated: file.truncated,
    };

    Ok(Some((paths, meta, value_type)))
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;

    use ptscan_core::{Base, Offset, Pointer, Sign, Size, Token};

    use super::{Meta, Paths, PointerFile, from_i64, into_paths, to_i64, write_value};
    use crate::export;

    fn paths(count: usize) -> Paths {
        let pointers = (0..count)
            .map(|n| {
                let base = match n % 3 {
                    0 => Base::Module {
                        id: (n % 2) as u16,
                        offset: from_i64(n as i64),
                    },
                    1 => Base::Address {
                        address: (n as u64 * 8).into(),
                    },
                    _ => Base::Null,
                };

                Pointer::new(base, [from_i64(0x10), from_i64(-8), Offset::zero()])
            })
            .collect();

        Paths {
            pointers,
            modules: vec!["game.exe".into(), "lib \"quoted\".dll".into()],
            truncated: true,
        }
    }

    fn meta() -> Meta {
        Meta {
            value_type: Some("u32".into()),
            target: Some("\"game.exe\" + 0x10 -> 0x8".into()),
            settings: Some(api::PointerScanSettings::default()),
        }
    }

    fn save(path: &str, paths: &Paths, token: &Token, progress: &mut dyn FnMut(u64)) -> bool {
        export::write_streamed::<PointerFile, _>(path, |out| {
            write_value(out, paths, &meta(), token, progress)
        })
        .unwrap()
    }

    fn load(path: &str) -> (Paths, Meta) {
        let bytes = export::read_bytes::<PointerFile>(path, |_, _| true)
            .unwrap()
            .unwrap();
        let file = export::decode::<PointerFile>(std::str::from_utf8(&bytes).unwrap()).unwrap();
        let (paths, meta, _) = into_paths(file, &Token::new(), &mut |_| {})
            .unwrap()
            .unwrap();
        (paths, meta)
    }

    #[test]
    fn offsets() {
        for value in [0, 1, -1, 0x1000, i64::MAX, -i64::MAX] {
            assert_eq!(to_i64(from_i64(value)).unwrap(), value);
        }

        let huge = Offset::new(Sign::Plus, Size::new(u64::MAX));
        assert!(to_i64(huge).is_err());
    }

    #[test]
    fn round_trip() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("paths.ptscan.json");
        let path = path.to_str().unwrap();
        let original = paths(STEP_TEST);

        assert!(save(path, &original, &Token::new(), &mut |_| {}));
        let (loaded, loaded_meta) = load(path);

        assert_eq!(loaded.pointers, original.pointers);
        assert_eq!(loaded.modules, original.modules);
        assert!(loaded.truncated);
        assert_eq!(loaded_meta, meta());

        let empty = Paths::default();
        assert!(save(path, &empty, &Token::new(), &mut |_| {}));
        assert!(load(path).0.pointers.is_empty());
    }

    const STEP_TEST: usize = super::STEP * 2 + 5;

    #[test]
    fn cancelled_save_leaves_no_file() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("paths.ptscan.json");
        let path = path.to_str().unwrap();
        let token = Token::new();
        let reports = Cell::new(0);

        // Cancelled part way through, after the first progress report.
        let mut progress = |written: u64| {
            reports.set(reports.get() + 1);

            if written > 0 {
                token.set();
            }
        };

        assert!(!save(path, &paths(STEP_TEST), &token, &mut progress));
        assert_eq!(reports.get(), 2);
        assert_eq!(std::fs::read_dir(dir.path()).unwrap().count(), 0);

        // Over an existing file, which stays as it was.
        std::fs::write(path, "before").unwrap();
        let token = Token::new();
        token.set();
        assert!(!save(path, &paths(10), &token, &mut |_| {}));
        assert_eq!(std::fs::read_to_string(path).unwrap(), "before");
        assert_eq!(std::fs::read_dir(dir.path()).unwrap().count(), 1);
    }

    #[test]
    fn malformed_paths() {
        let refusal = |value: &str| {
            let text = format!("{{\"version\":1,\"kind\":\"pointer-scan\",\"value\":{value}}}");

            match export::decode::<PointerFile>(&text) {
                Err(refused) => refused.refusal,
                Ok(file) => match into_paths(file, &Token::new(), &mut |_| {}) {
                    Err(refused) => refused.refusal,
                    Ok(_) => panic!("{value} loaded"),
                },
            }
        };

        let malformed = api::FileRefusal::Malformed;
        assert_eq!(refusal("{}"), malformed);
        assert_eq!(refusal("{\"modules\":[],\"paths\":[{}]}"), malformed);
        assert_eq!(
            refusal("{\"modules\":[],\"paths\":[{\"module\":0,\"offsets\":[]}]}"),
            malformed
        );
        assert_eq!(
            refusal(
                "{\"modules\":[\"a\"],\"paths\":[{\"module\":0,\"address\":1,\"offsets\":[]}]}"
            ),
            malformed
        );
        assert_eq!(
            refusal("{\"modules\":[],\"paths\":[{\"offsets\":[\"x\"]}]}"),
            malformed
        );
        assert_eq!(
            refusal("{\"value_type\":\"nope\",\"modules\":[],\"paths\":[]}"),
            malformed
        );
        assert_eq!(
            refusal("{\"modules\":[],\"paths\":[{\"offsets\":[99999999999999999999]}]}"),
            malformed
        );
    }
}
