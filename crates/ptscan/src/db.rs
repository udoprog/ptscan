//! The SQLite store behind the server.
//!
//! Every query is a statement of [`Read`] or [`Write`]. Settings live in
//! key/value tables whose values are a small versioned JSON envelope (see
//! [`Setting`]), so a new setting needs no migration.

use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use anyhow::{Context, Result, anyhow};
use api::ConfigKey;
use musli::alloc::Global;
use musli::mode::Text;
use musli::{Decode, Encode};
use rust_embed::RustEmbed;
use sqll::{JournalMode, OpenOptions, Pool, PoolBuilder, Statements, Synchronous, TypedStatement};

/// The migrations, applied in ascending filename order. The first is the
/// whole current schema; the rest are `YYYY-MM-DD-NN-name.sql` changes.
#[derive(RustEmbed)]
#[folder = "migrations"]
struct MigrationFiles;

#[derive(Statements)]
#[sql(read_only)]
struct Read {
    #[sql = "SELECT key, value FROM config ORDER BY key"]
    select_config: TypedStatement<(), (String, String)>,
    #[sql = "SELECT value FROM config WHERE key = ?1"]
    select_config_value: TypedStatement<(ConfigKey,), String>,
    #[sql = "SELECT id FROM sessions ORDER BY id"]
    select_sessions: TypedStatement<(), i64>,
    #[sql = "SELECT key, value FROM session_config WHERE session_id = ?1 ORDER BY key"]
    select_session_config: TypedStatement<(i64,), (String, String)>,
}

#[derive(Statements)]
struct Write {
    #[sql = "INSERT INTO config (key, value) VALUES (?, ?)"]
    #[sql = "ON CONFLICT(key) DO UPDATE SET value = excluded.value"]
    upsert_config: TypedStatement<(ConfigKey, String), ()>,
    #[sql = "DELETE FROM config WHERE key = ?"]
    delete_config: TypedStatement<(ConfigKey,), ()>,
    #[sql = "INSERT INTO sessions DEFAULT VALUES RETURNING id"]
    insert_session: TypedStatement<(), i64>,
    #[sql = "DELETE FROM sessions WHERE id = ?"]
    delete_session: TypedStatement<(i64,), ()>,
    #[sql = "INSERT INTO session_config (session_id, key, value) VALUES (?, ?, ?)"]
    #[sql = "ON CONFLICT(session_id, key) DO UPDATE SET value = excluded.value"]
    upsert_session_config: TypedStatement<(i64, ConfigKey, String), ()>,
    #[sql = "DELETE FROM session_config WHERE session_id = ? AND key = ?"]
    delete_session_config: TypedStatement<(i64, ConfigKey), ()>,
}

/// Settings to write, by key; `None` removes the setting.
pub type Changes = Vec<(ConfigKey, Option<String>)>;

/// A value stored under a [`ConfigKey`].
///
/// It is stored as `{"version": VERSION, "value": ..}`. A stored value of
/// another version reads as absent, so changing a setting's shape means
/// bumping its version (and dropping or converting what was stored).
pub trait Setting: Encode<Text> + for<'de> Decode<'de, Text, Global> {
    const VERSION: u32;
}

#[derive(Encode)]
struct EnvelopeRef<'a, T> {
    version: u32,
    value: &'a T,
}

#[derive(Decode)]
struct Version {
    version: u32,
}

#[derive(Decode)]
struct Envelope<T> {
    value: T,
}

pub(crate) fn encode<T>(value: &T) -> Result<String>
where
    T: Setting,
{
    let envelope = EnvelopeRef {
        version: T::VERSION,
        value,
    };

    Ok(musli::json::to_string(&envelope)?)
}

/// Decode a stored value, or `None` if it was stored under another version.
pub(crate) fn decode<T>(json: &str) -> Result<Option<T>>
where
    T: Setting,
{
    let Version { version } = musli::json::from_str(json)?;

    if version != T::VERSION {
        return Ok(None);
    }

    let Envelope { value } = musli::json::from_str(json)?;
    Ok(Some(value))
}

/// The database, shared by every connection to the server.
#[derive(Clone)]
pub struct Db {
    pool: Arc<Pool<Read, Write>>,
}

impl Db {
    /// Open the database at `path`, creating it and its directory if missing,
    /// and bring its schema up to date. A path starting with `file:` is taken
    /// as an SQLite URI.
    pub fn open(path: impl AsRef<Path>) -> Result<Db> {
        let path = path.as_ref();
        let uri = path.to_str().is_some_and(|path| path.starts_with("file:"));

        if !uri
            && let Some(dir) = path.parent()
            && !dir.as_os_str().is_empty()
        {
            fs::create_dir_all(dir)
                .with_context(|| format!("Creating database directory {}", dir.display()))?;
        }

        let mut first = OpenOptions::new();
        first.read_write().create();

        if uri {
            first.uri();
        }

        let migrated = first
            .open(path)
            .with_context(|| format!("Opening database {}", path.display()))?;

        do_migrations(&migrated).context("Running migrations")?;

        let mut options = OpenOptions::new();
        options
            .busy_timeout(Duration::from_secs(5))
            .foreign_keys(true)
            .journal_mode(JournalMode::Wal)
            .synchronous(Synchronous::Normal);

        if uri {
            options.uri();
        }

        let pool = PoolBuilder::new(options, 4)
            .open(path)
            .with_context(|| format!("Opening connection pool for {}", path.display()))?;

        drop(migrated);
        Ok(Db {
            pool: Arc::new(pool),
        })
    }

    /// Every global setting, by key. A key this build does not know is skipped
    /// with a warning.
    pub async fn config(&self) -> Result<BTreeMap<ConfigKey, String>> {
        let rows = self
            .pool
            .read(|r| {
                r.select_config
                    .query()?
                    .collect::<Result<Vec<_>, sqll::Error>>()
            })
            .await??;

        let mut out = BTreeMap::new();

        for (key, value) in rows {
            match key.parse::<ConfigKey>() {
                Ok(parsed) => {
                    out.insert(parsed, value);
                }
                Err(error) => {
                    tracing::warn!(key, "Skipping unknown configuration key: {error}");
                }
            }
        }

        Ok(out)
    }

    pub async fn get_config<T>(&self, key: ConfigKey) -> Result<Option<T>>
    where
        T: Setting,
    {
        let json = self
            .pool
            .read(move |r| r.select_config_value.bind((key,))?.first())
            .await??;

        let Some(json) = json else {
            return Ok(None);
        };

        let value = decode(&json).with_context(|| format!("Decoding configuration {key}"))?;

        if value.is_none() {
            tracing::warn!(%key, "Ignoring configuration stored under another version");
        }

        Ok(value)
    }

    pub async fn set_config<T>(&self, key: ConfigKey, value: &T) -> Result<()>
    where
        T: Setting,
    {
        let json = encode(value)?;

        self.pool
            .write(move |w| w.transaction(|w, _| w.upsert_config.execute((key, json.as_str()))))
            .await??;

        Ok(())
    }

    pub async fn remove_config(&self, key: ConfigKey) -> Result<()> {
        self.pool
            .write(move |w| w.transaction(|w, _| w.delete_config.execute((key,))))
            .await??;

        Ok(())
    }

    pub async fn session_ids(&self) -> Result<Vec<u64>> {
        let ids = self
            .pool
            .read(|r| {
                r.select_sessions
                    .query()?
                    .collect::<Result<Vec<_>, sqll::Error>>()
            })
            .await??;

        Ok(ids
            .into_iter()
            .filter_map(|id| u64::try_from(id).ok())
            .collect())
    }

    /// Every setting of a session, by key. A key this build does not know is
    /// skipped with a warning.
    pub async fn session_config(&self, id: u64) -> Result<BTreeMap<ConfigKey, String>> {
        let id = i64::try_from(id)?;

        let rows = self
            .pool
            .read(move |r| {
                r.select_session_config
                    .bind((id,))?
                    .collect::<Result<Vec<_>, sqll::Error>>()
            })
            .await??;

        let mut out = BTreeMap::new();

        for (key, value) in rows {
            match key.parse::<ConfigKey>() {
                Ok(parsed) => {
                    out.insert(parsed, value);
                }
                Err(error) => {
                    tracing::warn!(session = id, key, "Skipping unknown session key: {error}");
                }
            }
        }

        Ok(out)
    }

    /// Create a session holding `changes`, returning its id.
    pub async fn create_session(&self, changes: Changes) -> Result<u64> {
        let id = self
            .pool
            .write(move |w| {
                w.transaction(|w, _| {
                    let id = w
                        .insert_session
                        .query()?
                        .first()?
                        .ok_or_else(|| anyhow!("Inserting a session returned no id"))?;
                    apply(w, id, &changes)?;
                    Ok::<_, anyhow::Error>(id)
                })
            })
            .await??;

        Ok(u64::try_from(id)?)
    }

    pub async fn update_session(&self, id: u64, changes: Changes) -> Result<()> {
        let id = i64::try_from(id)?;

        self.pool
            .write(move |w| w.transaction(|w, _| apply(w, id, &changes)))
            .await??;

        Ok(())
    }

    /// Apply `changes` to a session and record it as the open one under
    /// `current` in the global settings.
    pub async fn open_session<T>(
        &self,
        id: u64,
        changes: Changes,
        current: ConfigKey,
        value: &T,
    ) -> Result<()>
    where
        T: Setting,
    {
        let id = i64::try_from(id)?;
        let json = encode(value)?;

        self.pool
            .write(move |w| {
                w.transaction(|w, _| {
                    apply(w, id, &changes)?;
                    w.upsert_config.execute((current, json.as_str()))
                })
            })
            .await??;

        Ok(())
    }

    /// Delete a session and every setting it holds.
    pub async fn delete_session(&self, id: u64) -> Result<()> {
        let id = i64::try_from(id)?;

        self.pool
            .write(move |w| w.transaction(|w, _| w.delete_session.execute((id,))))
            .await??;

        Ok(())
    }
}

fn apply(w: &mut Write, id: i64, changes: &Changes) -> Result<(), sqll::Error> {
    for (key, value) in changes {
        match value {
            Some(value) => w
                .upsert_session_config
                .execute((id, *key, value.as_str()))?,
            None => w.delete_session_config.execute((id, *key))?,
        };
    }

    Ok(())
}

/// Where the database lives unless `--db` says otherwise:
/// `%LOCALAPPDATA%\ptscan\ptscan.db` on Windows, else
/// `$XDG_DATA_HOME/ptscan/ptscan.db` falling back to `~/.local/share`.
pub fn default_path() -> Result<PathBuf> {
    let dir = if cfg!(windows) {
        absolute_var("LOCALAPPDATA").ok_or_else(|| anyhow!("LOCALAPPDATA is not set"))?
    } else if let Some(dir) = absolute_var("XDG_DATA_HOME") {
        dir
    } else {
        absolute_var("HOME")
            .ok_or_else(|| anyhow!("neither XDG_DATA_HOME nor HOME is set"))?
            .join(".local")
            .join("share")
    };

    Ok(dir.join("ptscan").join("ptscan.db"))
}

fn absolute_var(name: &str) -> Option<PathBuf> {
    let path = PathBuf::from(env::var_os(name)?);
    path.is_absolute().then_some(path)
}

/// Bring the schema up to date. A fresh database runs the initial migration
/// alone, since it is the whole current schema, and has the rest recorded as
/// applied; any other runs each migration it has not recorded yet.
fn do_migrations(c: &sqll::Connection) -> Result<()> {
    let mut ids: Vec<_> = MigrationFiles::iter().collect();
    ids.sort();

    let mut migrations = Vec::with_capacity(ids.len());

    for id in &ids {
        let asset =
            MigrationFiles::get(id).with_context(|| format!("Migration file not found: {id}"))?;

        let sql = String::from_utf8(asset.data.into_owned())
            .with_context(|| format!("Migration {id} is not valid UTF-8"))?;

        migrations.push((id.as_ref(), sql));
    }

    let Some((_, base)) = migrations.first() else {
        return Ok(());
    };

    let applied = sqll::Migrations::new()
        .fresh_schema(base)
        .run(c, migrations.iter().map(|(id, sql)| (*id, sql.as_str())))?;

    if applied > 0 {
        tracing::info!(applied, "Migrations applied");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, BTreeSet};

    use anyhow::Result;
    use api::{ConfigField, ConfigKey};
    use musli::{Decode, Encode};
    use sqll::OpenOptions;

    use super::{Db, Setting, do_migrations};

    /// The initial schema as the first released database had it. Never edit
    /// it: it is what databases in the wild were created from, and every dated
    /// migration has to bring it to the current initial schema.
    const OLDEST_BASE: &str = include_str!("../tests-data/initial-2026-11-25.sql");

    #[derive(Debug, PartialEq, Encode, Decode)]
    struct Filter {
        filter: String,
    }

    impl Setting for Filter {
        const VERSION: u32 = 1;
    }

    #[derive(Debug, PartialEq, Encode, Decode)]
    struct FilterV2 {
        filter: String,
    }

    impl Setting for FilterV2 {
        const VERSION: u32 = 2;
    }

    fn filter_key() -> ConfigKey {
        ConfigField::Scan
            .key()
            .with_field(ConfigField::Filter)
            .unwrap()
    }

    /// SQL with comments dropped and runs of whitespace collapsed, removed
    /// around punctuation, so text an `ALTER` leaves behind compares equal.
    fn normalize_sql(sql: &str) -> String {
        let mut out = String::new();
        let mut space = false;
        let mut quote = None;
        let mut chars = sql.chars().peekable();

        while let Some(c) = chars.next() {
            if let Some(q) = quote {
                out.push(c);

                if c == q {
                    quote = None;
                }

                continue;
            }

            match c {
                '-' if chars.peek() == Some(&'-') => {
                    for c in chars.by_ref() {
                        if c == '\n' {
                            break;
                        }
                    }

                    space = true;
                }
                c if c.is_whitespace() => space = true,
                c => {
                    let punct = matches!(c, '(' | ')' | ',' | ';');

                    if space && !punct && !out.ends_with(['(', ',']) && !out.is_empty() {
                        out.push(' ');
                    }

                    space = false;

                    if matches!(c, '\'' | '"') {
                        quote = Some(c);
                    }

                    out.push(c);
                }
            }
        }

        out
    }

    fn schema_of(c: &sqll::Connection) -> Result<BTreeMap<(String, String), (String, String)>> {
        let mut q = c.prepare(
            "SELECT type, name, tbl_name, coalesce(sql, '') FROM sqlite_master
             WHERE tbl_name != 'migrations'",
        )?;
        let mut out = BTreeMap::new();

        while let Some((ty, name, table, sql)) = q.next::<(String, String, String, String)>()? {
            out.insert((ty, name), (table, normalize_sql(&sql)));
        }

        Ok(out)
    }

    /// A fresh database runs the initial schema alone; an upgraded one starts
    /// from the oldest released base and runs every dated migration. Both
    /// must end with the same schema.
    #[test]
    fn fresh_and_upgraded_databases_agree() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let open = |name: &str| -> Result<sqll::Connection> {
            Ok(OpenOptions::new()
                .read_write()
                .create()
                .open(dir.path().join(name))?)
        };

        let fresh = open("fresh.db")?;
        do_migrations(&fresh)?;

        let upgraded = open("upgraded.db")?;
        sqll::Migrations::new().run(&upgraded, [("0000-00-00-00-initial.sql", OLDEST_BASE)])?;
        do_migrations(&upgraded)?;

        let fresh = schema_of(&fresh)?;
        let upgraded = schema_of(&upgraded)?;
        assert!(fresh.contains_key(&("table".to_owned(), "config".to_owned())));

        let keys: BTreeSet<_> = fresh.keys().chain(upgraded.keys()).collect();
        let mut drift = Vec::new();

        for key in keys {
            let (f, u) = (fresh.get(key), upgraded.get(key));

            if f != u {
                drift.push(format!("{key:?}\n  fresh:    {f:?}\n  upgraded: {u:?}"));
            }
        }

        assert!(drift.is_empty(), "schema drift:\n{}", drift.join("\n"));
        Ok(())
    }

    #[test]
    fn normalize_sql_ignores_layout() {
        assert_eq!(
            normalize_sql("CREATE TABLE t (\n  a TEXT, -- note\n  b 'x  y'\n)"),
            normalize_sql("CREATE TABLE t (a TEXT,b 'x  y')"),
        );
    }

    #[tokio::test]
    async fn config_survives_reopening() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let path = dir.path().join("nested").join("ptscan.db");
        let filter = Filter {
            filter: "value == 42".into(),
        };

        let db = Db::open(&path)?;
        assert_eq!(db.get_config::<Filter>(filter_key()).await?, None);
        db.set_config(filter_key(), &filter).await?;
        drop(db);

        let db = Db::open(&path)?;
        assert_eq!(db.get_config::<Filter>(filter_key()).await?, Some(filter));

        let all = db.config().await?;
        assert_eq!(
            all.get(&filter_key()).map(String::as_str),
            Some(r#"{"version":1,"value":{"filter":"value == 42"}}"#)
        );

        db.remove_config(filter_key()).await?;
        assert_eq!(db.get_config::<Filter>(filter_key()).await?, None);
        Ok(())
    }

    #[tokio::test]
    async fn another_version_reads_as_absent() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let db = Db::open(dir.path().join("ptscan.db"))?;

        db.set_config(
            filter_key(),
            &Filter {
                filter: "value".into(),
            },
        )
        .await?;

        assert_eq!(db.get_config::<FilterV2>(filter_key()).await?, None);
        Ok(())
    }

    #[tokio::test]
    async fn unknown_keys_are_skipped_at_load() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let path = dir.path().join("ptscan.db");
        let db = Db::open(&path)?;
        db.set_config(filter_key(), &Filter { filter: "x".into() })
            .await?;

        let c = OpenOptions::new().read_write().open(&path)?;
        c.execute(r#"INSERT INTO config (key, value) VALUES ('from-the-future', '{"version":1,"value":0}')"#)?;
        drop(c);

        let all = db.config().await?;
        assert_eq!(all.keys().copied().collect::<Vec<_>>(), [filter_key()]);
        Ok(())
    }

    #[test]
    fn values_must_be_json() -> Result<()> {
        let dir = tempfile::tempdir()?;
        let path = dir.path().join("ptscan.db");
        drop(Db::open(&path)?);

        let c = OpenOptions::new().read_write().open(&path)?;
        assert!(
            c.execute("INSERT INTO config (key, value) VALUES ('scan', 'not json')")
                .is_err()
        );
        Ok(())
    }
}
