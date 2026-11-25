//! Listing directories on the server's machine for the web UI's file
//! browser, which runs in a browser that cannot see them itself. Read only.

use std::cmp::Ordering;
use std::fs;
use std::io::ErrorKind;
use std::path::{Component, MAIN_SEPARATOR_STR, Path, PathBuf};
use std::time::UNIX_EPOCH;

/// The directories a file browser offers as shortcuts.
#[derive(Debug, Clone, Default)]
pub struct Places {
    pub home: Option<PathBuf>,
    pub data: Option<PathBuf>,
    /// Filesystem roots, found once since probing drives can stall.
    pub roots: Vec<PathBuf>,
}

impl Places {
    /// The user's home and the filesystem roots.
    pub fn from_env() -> Self {
        let var = if cfg!(windows) { "USERPROFILE" } else { "HOME" };

        let home = std::env::var_os(var)
            .map(PathBuf::from)
            .filter(|path| path.is_absolute());

        Self {
            home,
            data: None,
            roots: roots(),
        }
    }

    /// Also offer the directory holding the database at `db`.
    pub fn with_data(mut self, db: &Path) -> Self {
        self.data = std::path::absolute(db)
            .ok()
            .and_then(|path| path.parent().map(Path::to_owned));
        self
    }

    fn list(&self) -> Vec<api::Place> {
        let mut places = Vec::new();

        let named = [
            (api::PlaceKind::Home, "Home", &self.home),
            (api::PlaceKind::Data, "ptscan data", &self.data),
        ];

        for (kind, name, path) in named {
            if let Some(path) = path
                && path.is_dir()
            {
                places.push(api::Place {
                    kind,
                    name: name.to_owned(),
                    path: path.display().to_string(),
                });
            }
        }

        for root in &self.roots {
            places.push(api::Place {
                kind: api::PlaceKind::Root,
                name: root.display().to_string(),
                path: root.display().to_string(),
            });
        }

        places
    }
}

/// List `path`, or the home directory (else the first root) when `None`.
pub fn list_directory(path: Option<&str>, places: &Places) -> api::ListDirectoryResponse {
    let path = match path {
        Some(path) => PathBuf::from(path),
        None => places
            .home
            .clone()
            .or_else(|| places.roots.first().cloned())
            .unwrap_or_default(),
    };

    let mut response = api::ListDirectoryResponse {
        path: path.display().to_string(),
        places: places.list(),
        separator: MAIN_SEPARATOR_STR.to_owned(),
        ..api::ListDirectoryResponse::default()
    };

    if !path.is_absolute() {
        response.refusal = Some(api::ListDirectoryRefusal::Relative);
        return response;
    }

    let path = normalize(&path);
    response.path = path.display().to_string();
    response.parent = path.parent().map(|p| p.display().to_string());
    response.crumbs = crumbs(&path);

    match read(&path) {
        Ok(entries) => response.entries = entries,
        Err(refusal) => response.refusal = Some(refusal),
    }

    response
}

fn read(path: &Path) -> Result<Vec<api::DirectoryEntry>, api::ListDirectoryRefusal> {
    let metadata = fs::metadata(path).map_err(|error| refusal(&error))?;

    if !metadata.is_dir() {
        return Err(api::ListDirectoryRefusal::NotADirectory);
    }

    let mut entries = Vec::new();

    for entry in fs::read_dir(path).map_err(|error| refusal(&error))? {
        let Ok(entry) = entry else {
            continue;
        };

        let Some(name) = entry.file_name().to_str().map(str::to_owned) else {
            continue;
        };

        // Follows symlinks, so a link to a directory browses as one; a broken
        // link is skipped.
        let Ok(metadata) = fs::metadata(entry.path()) else {
            continue;
        };

        let kind = if metadata.is_dir() {
            api::EntryKind::Directory
        } else {
            api::EntryKind::File
        };

        let modified = metadata
            .modified()
            .ok()
            .and_then(|time| time.duration_since(UNIX_EPOCH).ok())
            .map(|d| d.as_secs());

        entries.push(api::DirectoryEntry {
            path: entry.path().display().to_string(),
            name,
            kind,
            size: (kind == api::EntryKind::File).then_some(metadata.len()),
            modified,
        });
    }

    entries.sort_by(order);
    Ok(entries)
}

fn order(a: &api::DirectoryEntry, b: &api::DirectoryEntry) -> Ordering {
    let rank = |e: &api::DirectoryEntry| e.kind != api::EntryKind::Directory;

    rank(a)
        .cmp(&rank(b))
        .then_with(|| a.name.to_lowercase().cmp(&b.name.to_lowercase()))
        .then_with(|| a.name.cmp(&b.name))
}

fn refusal(error: &std::io::Error) -> api::ListDirectoryRefusal {
    match error.kind() {
        ErrorKind::NotFound => api::ListDirectoryRefusal::Missing,
        ErrorKind::NotADirectory => api::ListDirectoryRefusal::NotADirectory,
        _ => api::ListDirectoryRefusal::Unreadable,
    }
}

/// `path` without `.` and `..` components, without touching the filesystem.
fn normalize(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();

    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                out.pop();
            }
            other => out.push(other),
        }
    }

    out
}

fn crumbs(path: &Path) -> Vec<api::Crumb> {
    let mut crumbs = path
        .ancestors()
        .map(|dir| api::Crumb {
            name: match dir.file_name() {
                Some(name) => name.to_string_lossy().into_owned(),
                None => dir.display().to_string(),
            },
            path: dir.display().to_string(),
        })
        .collect::<Vec<_>>();

    crumbs.reverse();
    crumbs
}

fn roots() -> Vec<PathBuf> {
    if cfg!(windows) {
        (b'A'..=b'Z')
            .map(|letter| PathBuf::from(format!("{}:\\", char::from(letter))))
            .filter(|drive| drive.is_dir())
            .collect()
    } else {
        vec![PathBuf::from("/")]
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;

    fn places(home: &Path) -> Places {
        Places {
            home: Some(home.to_owned()),
            data: Some(home.join("data")),
            roots: vec![PathBuf::from("/")],
        }
    }

    #[test]
    fn lists_directories_first_then_files_by_name() -> std::io::Result<()> {
        let root = tempfile::tempdir()?;
        fs::create_dir(root.path().join("b"))?;
        fs::create_dir(root.path().join("A"))?;
        fs::write(root.path().join("c.ptscan.json"), b"{}")?;
        fs::write(root.path().join("B.txt"), b"")?;

        let path = root.path().display().to_string();
        let listed = list_directory(Some(&path), &places(root.path()));

        assert_eq!(listed.refusal, None);
        assert_eq!(listed.path, path);
        assert_eq!(listed.separator, "/");

        let names = listed
            .entries
            .iter()
            .map(|e| e.name.as_str())
            .collect::<Vec<_>>();
        assert_eq!(names, ["A", "b", "B.txt", "c.ptscan.json"]);

        let file = &listed.entries[3];
        assert_eq!(file.kind, api::EntryKind::File);
        assert_eq!(file.size, Some(2));
        assert_eq!(
            file.path,
            root.path().join("c.ptscan.json").display().to_string()
        );
        assert!(file.modified.is_some());

        let dir = &listed.entries[0];
        assert_eq!(dir.kind, api::EntryKind::Directory);
        assert_eq!(dir.size, None);
        Ok(())
    }

    #[test]
    fn crumbs_run_from_the_root_down() -> std::io::Result<()> {
        let root = tempfile::tempdir()?;
        let nested = root.path().join("one").join("two");
        fs::create_dir_all(&nested)?;

        let asked = format!("{}/./../two", nested.display());
        let listed = list_directory(Some(&asked), &places(root.path()));

        assert_eq!(listed.refusal, None);
        assert_eq!(listed.path, nested.display().to_string());
        assert_eq!(
            listed.parent,
            Some(root.path().join("one").display().to_string())
        );

        let first = listed.crumbs.first().unwrap();
        assert_eq!((first.name.as_str(), first.path.as_str()), ("/", "/"));

        let last = listed.crumbs.last().unwrap();
        assert_eq!(last.name, "two");
        assert_eq!(last.path, listed.path);
        Ok(())
    }

    #[test]
    fn places_are_the_existing_home_data_and_root() -> std::io::Result<()> {
        let root = tempfile::tempdir()?;
        let path = root.path().display().to_string();

        let listed = list_directory(Some(&path), &places(root.path()));
        let kinds = listed.places.iter().map(|p| p.kind).collect::<Vec<_>>();
        // The data directory does not exist yet.
        assert_eq!(kinds, [api::PlaceKind::Home, api::PlaceKind::Root]);

        fs::create_dir(root.path().join("data"))?;
        let listed = list_directory(Some(&path), &places(root.path()));
        let kinds = listed.places.iter().map(|p| p.kind).collect::<Vec<_>>();
        assert_eq!(
            kinds,
            [
                api::PlaceKind::Home,
                api::PlaceKind::Data,
                api::PlaceKind::Root
            ]
        );
        Ok(())
    }

    #[test]
    fn no_path_lists_home() -> std::io::Result<()> {
        let root = tempfile::tempdir()?;
        fs::write(root.path().join("x"), b"")?;

        let listed = list_directory(None, &places(root.path()));

        assert_eq!(listed.path, root.path().display().to_string());
        assert_eq!(listed.entries.len(), 1);
        Ok(())
    }

    #[test]
    fn refuses_missing_files_and_relative_paths() -> std::io::Result<()> {
        let root = tempfile::tempdir()?;
        fs::write(root.path().join("file"), b"")?;
        let places = places(root.path());

        let missing = root.path().join("missing").display().to_string();
        let listed = list_directory(Some(&missing), &places);
        assert_eq!(listed.refusal, Some(api::ListDirectoryRefusal::Missing));
        assert_eq!(listed.path, missing);
        assert!(listed.entries.is_empty());
        assert!(!listed.crumbs.is_empty());

        let file = root.path().join("file").display().to_string();
        let listed = list_directory(Some(&file), &places);
        assert_eq!(
            listed.refusal,
            Some(api::ListDirectoryRefusal::NotADirectory)
        );

        let listed = list_directory(Some("relative/dir"), &places);
        assert_eq!(listed.refusal, Some(api::ListDirectoryRefusal::Relative));
        assert!(listed.crumbs.is_empty());
        Ok(())
    }

    #[test]
    fn refuses_unreadable_directories() -> std::io::Result<()> {
        use std::os::unix::fs::PermissionsExt;

        let root = tempfile::tempdir()?;
        let locked = root.path().join("locked");
        fs::create_dir(&locked)?;
        fs::set_permissions(&locked, fs::Permissions::from_mode(0o000))?;

        let listed = list_directory(Some(&locked.display().to_string()), &places(root.path()));
        fs::set_permissions(&locked, fs::Permissions::from_mode(0o755))?;

        // Root reads anything, so the refusal only shows for other users.
        if listed.refusal.is_none() {
            return Ok(());
        }

        assert_eq!(listed.refusal, Some(api::ListDirectoryRefusal::Unreadable));
        Ok(())
    }
}
