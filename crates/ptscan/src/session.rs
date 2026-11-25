//! Named sessions, persisted in the database: a name, the process they
//! target, the scan form and the pointer-scan settings. Scan and pointer-scan
//! results are never stored.

use std::collections::BTreeMap;
use std::time::{SystemTime, UNIX_EPOCH};

use anyhow::{Context as _, Result, anyhow, bail};
use api::{ConfigField, ConfigKey};

use crate::Shared;
use crate::db::{self, Changes, Db, Setting};

const DEFAULT_NAME: &str = "Default";

/// The open session, in the global `config` table.
const CURRENT: ConfigKey = ConfigKey::field(ConfigField::Session);

const NAME: ConfigKey = ConfigKey::field(ConfigField::Name);
const CREATED: ConfigKey = ConfigKey::field(ConfigField::Created);
const OPENED: ConfigKey = ConfigKey::field(ConfigField::Opened);
const TARGET: ConfigKey = ConfigKey::field(ConfigField::Target);
const SCAN_FILTER: ConfigKey = ConfigKey::nested(ConfigField::Scan, ConfigField::Filter);
const SCAN_VALUE: ConfigKey = ConfigKey::nested(ConfigField::Scan, ConfigField::Value);
const SCAN_ALIGNMENT: ConfigKey = ConfigKey::nested(ConfigField::Scan, ConfigField::Alignment);
const SCAN_SUSPEND: ConfigKey = ConfigKey::nested(ConfigField::Scan, ConfigField::Suspend);
const SCAN_MODULES_ONLY: ConfigKey = ConfigKey::nested(ConfigField::Scan, ConfigField::ModulesOnly);
const MAX_DEPTH: ConfigKey = ConfigKey::nested(ConfigField::PointerScan, ConfigField::MaxDepth);
const MAX_OFFSET: ConfigKey = ConfigKey::nested(ConfigField::PointerScan, ConfigField::MaxOffset);
const MAX_PATHS: ConfigKey = ConfigKey::nested(ConfigField::PointerScan, ConfigField::MaxPaths);
const LAYOUT_SIDE: ConfigKey = ConfigKey::nested(ConfigField::Layout, ConfigField::Side);
const LAYOUT_RESULTS: ConfigKey = ConfigKey::nested(ConfigField::Layout, ConfigField::Results);
const LAYOUT_SCRATCH: ConfigKey = ConfigKey::nested(ConfigField::Layout, ConfigField::Scratch);

impl Setting for String {
    const VERSION: u32 = 1;
}

impl Setting for bool {
    const VERSION: u32 = 1;
}

impl Setting for u16 {
    const VERSION: u32 = 1;
}

impl Setting for u32 {
    const VERSION: u32 = 1;
}

impl Setting for u64 {
    const VERSION: u32 = 1;
}

impl Setting for api::SessionTarget {
    const VERSION: u32 = 1;
}

fn now() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or_default()
}

fn put<T>(changes: &mut Changes, key: ConfigKey, value: Option<&T>) -> Result<()>
where
    T: Setting,
{
    let value = match value {
        Some(value) => Some(db::encode(value)?),
        None => None,
    };

    changes.push((key, value));
    Ok(())
}

/// A stored setting, or `None` when it is missing or cannot be read, which
/// is logged.
fn get<T>(id: u64, config: &BTreeMap<ConfigKey, String>, key: ConfigKey) -> Option<T>
where
    T: Setting,
{
    let json = config.get(&key)?;

    match db::decode(json) {
        Ok(Some(value)) => Some(value),
        Ok(None) => {
            tracing::warn!(session = id, %key, "Ignoring setting stored under another version");
            None
        }
        Err(error) => {
            tracing::warn!(session = id, %key, "Ignoring unreadable setting: {error:#}");
            None
        }
    }
}

fn info(id: u64, config: &BTreeMap<ConfigKey, String>) -> api::SessionInfo {
    api::SessionInfo {
        id,
        name: get(id, config, NAME).unwrap_or_else(|| format!("Session {id}")),
        created: get(id, config, CREATED).unwrap_or_default(),
        opened: get(id, config, OPENED).unwrap_or_default(),
        target: get(id, config, TARGET),
    }
}

fn scan_form(id: u64, config: &BTreeMap<ConfigKey, String>) -> api::ScanForm {
    api::ScanForm {
        filter: get(id, config, SCAN_FILTER).unwrap_or_default(),
        value: get(id, config, SCAN_VALUE).unwrap_or_default(),
        alignment: get(id, config, SCAN_ALIGNMENT),
        suspend: get(id, config, SCAN_SUSPEND).unwrap_or_default(),
        modules_only: get(id, config, SCAN_MODULES_ONLY).unwrap_or_default(),
    }
}

fn pointer_scan(id: u64, config: &BTreeMap<ConfigKey, String>) -> api::PointerScanSettings {
    let default = api::PointerScanSettings::default();

    api::PointerScanSettings {
        max_depth: get(id, config, MAX_DEPTH).unwrap_or(default.max_depth),
        max_offset: get(id, config, MAX_OFFSET).unwrap_or(default.max_offset),
        max_paths: get(id, config, MAX_PATHS).unwrap_or(default.max_paths),
    }
}

fn layout(id: u64, config: &BTreeMap<ConfigKey, String>) -> api::Layout {
    let default = api::Layout::default();
    let share = |key, default: u16| {
        get(id, config, key)
            .filter(|share: &u16| *share <= api::Layout::SCALE)
            .unwrap_or(default)
    };

    api::Layout {
        side: share(LAYOUT_SIDE, default.side),
        results: share(LAYOUT_RESULTS, default.results),
        scratch: share(LAYOUT_SCRATCH, default.scratch),
    }
}

fn validate_layout(layout: &api::Layout) -> Result<()> {
    let max = api::Layout::SCALE;

    if [layout.side, layout.results, layout.scratch]
        .iter()
        .any(|share| *share > max)
    {
        bail!("A region's share must be at most {max}");
    }

    Ok(())
}

fn validate_name(name: &str) -> Result<String> {
    let name = name.trim();

    if name.is_empty() {
        bail!("A session needs a name");
    }

    Ok(name.to_owned())
}

fn validate_pointer_scan(settings: &api::PointerScanSettings) -> Result<()> {
    let max = api::PointerScanSettings::MAX_DEPTH;

    if !(1..=max).contains(&settings.max_depth) {
        bail!("The depth must be between 1 and {max}");
    }

    let max = api::PointerScanSettings::MAX_OFFSET;

    if !(1..=max).contains(&settings.max_offset) {
        bail!("The offset must be between 1 and {max:#x}");
    }

    let max = api::PointerScanSettings::MAX_PATHS;

    if !(1..=max).contains(&settings.max_paths) {
        bail!("The most paths must be between 1 and {max}");
    }

    Ok(())
}

/// Most recently opened first.
fn sort(sessions: &mut [api::SessionInfo]) {
    sessions.sort_by(|a, b| b.opened.cmp(&a.opened).then(b.id.cmp(&a.id)));
}

impl Shared {
    pub fn session_state(&self) -> api::SessionState {
        self.session_state.borrow().clone()
    }

    pub(crate) fn pointer_scan_settings(&self) -> api::PointerScanSettings {
        self.session_state.borrow().pointer_scan.clone()
    }

    fn db_for_sessions(&self) -> Result<&Db> {
        self.db
            .as_ref()
            .ok_or_else(|| anyhow!("Sessions need a database"))
    }

    fn respond(&self, result: Result<()>) -> api::SessionResponse {
        api::SessionResponse {
            state: self.session_state(),
            error: result.err().map(|e| format!("{e:#}")),
        }
    }

    fn has_session(&self, id: u64) -> bool {
        self.session_state
            .borrow()
            .sessions
            .iter()
            .any(|s| s.id == id)
    }

    /// Open the session that was open last, creating a default one when there
    /// are none. Does nothing without a database.
    pub async fn open_last_session(&self) -> Result<()> {
        let Some(db) = &self.db else {
            return Ok(());
        };

        let _guard = self.session_lock.lock().await;
        let mut sessions = Vec::new();

        for id in db.session_ids().await? {
            let config = db.session_config(id).await?;
            sessions.push(info(id, &config));
        }

        sort(&mut sessions);

        let last = db
            .get_config::<u64>(CURRENT)
            .await?
            .filter(|id| sessions.iter().any(|s| s.id == *id))
            .or_else(|| sessions.first().map(|s| s.id));

        let id = match last {
            Some(id) => id,
            None => {
                let session = create(db, DEFAULT_NAME.to_owned()).await?;
                let id = session.id;
                sessions.push(session);
                id
            }
        };

        self.session_state
            .send_modify(|state| state.sessions = sessions);
        self.open_locked(db, id).await
    }

    async fn open_locked(&self, db: &Db, id: u64) -> Result<()> {
        let config = db
            .session_config(id)
            .await
            .with_context(|| format!("Loading session {id}"))?;
        let watches = db
            .watches(id)
            .await
            .with_context(|| format!("Loading the watches of session {id}"))?;

        let opened = now();
        let mut changes = Changes::new();
        put(&mut changes, OPENED, Some(&opened))?;
        db.open_session(id, changes, CURRENT, &id).await?;

        self.detach();
        self.reset_scan();
        self.reset_pointer_scan();
        self.load_scratch(watches);

        let mut session = info(id, &config);
        session.opened = opened;
        let scan = scan_form(id, &config);
        let pointer_scan = pointer_scan(id, &config);
        let layout = layout(id, &config);

        self.session_state.send_modify(|state| {
            state.sessions.retain(|s| s.id != id);
            state.sessions.push(session);
            sort(&mut state.sessions);
            state.current = Some(id);
            state.scan = scan;
            state.pointer_scan = pointer_scan;
            state.layout = layout;
        });

        Ok(())
    }

    pub async fn create_session(&self, name: &str) -> api::SessionResponse {
        let result = async {
            let db = self.db_for_sessions()?;
            let name = validate_name(name)?;
            let _guard = self.session_lock.lock().await;
            let session = create(db, name).await?;

            self.session_state.send_modify(|state| {
                state.sessions.push(session);
                sort(&mut state.sessions);
            });

            Ok(())
        };

        self.respond(result.await)
    }

    pub async fn rename_session(&self, id: u64, name: &str) -> api::SessionResponse {
        let result = async {
            let db = self.db_for_sessions()?;
            let name = validate_name(name)?;
            let _guard = self.session_lock.lock().await;

            if !self.has_session(id) {
                bail!("No session {id}");
            }

            let mut changes = Changes::new();
            put(&mut changes, NAME, Some(&name))?;
            db.update_session(id, changes).await?;

            self.session_state.send_modify(|state| {
                if let Some(session) = state.sessions.iter_mut().find(|s| s.id == id) {
                    session.name = name;
                }
            });

            Ok(())
        };

        self.respond(result.await)
    }

    pub async fn open_session(&self, id: u64) -> api::SessionResponse {
        let result = async {
            let db = self.db_for_sessions()?;
            let _guard = self.session_lock.lock().await;

            if !self.has_session(id) {
                bail!("No session {id}");
            }

            self.open_locked(db, id).await
        };

        self.respond(result.await)
    }

    pub async fn delete_session(&self, id: u64) -> api::SessionResponse {
        let result = async {
            let db = self.db_for_sessions()?;
            let _guard = self.session_lock.lock().await;

            if self.session_state.borrow().current == Some(id) {
                bail!("The open session cannot be deleted");
            }

            if !self.has_session(id) {
                bail!("No session {id}");
            }

            db.delete_session(id).await?;

            self.session_state
                .send_modify(|state| state.sessions.retain(|s| s.id != id));

            Ok(())
        };

        self.respond(result.await)
    }

    pub async fn set_scan_form(&self, form: api::ScanForm) -> api::SessionResponse {
        let result = async {
            let _guard = self.session_lock.lock().await;

            let current = self.session_state.borrow().current;

            if let (Some(db), Some(id)) = (&self.db, current) {
                let mut changes = Changes::new();
                put(&mut changes, SCAN_FILTER, Some(&form.filter))?;
                put(&mut changes, SCAN_VALUE, Some(&form.value))?;
                put(&mut changes, SCAN_ALIGNMENT, form.alignment.as_ref())?;
                put(&mut changes, SCAN_SUSPEND, Some(&form.suspend))?;
                put(&mut changes, SCAN_MODULES_ONLY, Some(&form.modules_only))?;
                db.update_session(id, changes).await?;
            }

            self.session_state.send_modify(|state| state.scan = form);
            Ok(())
        };

        self.respond(result.await)
    }

    pub async fn set_pointer_scan_settings(
        &self,
        settings: api::PointerScanSettings,
    ) -> api::SessionResponse {
        let result = async {
            validate_pointer_scan(&settings)?;
            let _guard = self.session_lock.lock().await;

            let current = self.session_state.borrow().current;

            if let (Some(db), Some(id)) = (&self.db, current) {
                let mut changes = Changes::new();
                put(&mut changes, MAX_DEPTH, Some(&settings.max_depth))?;
                put(&mut changes, MAX_OFFSET, Some(&settings.max_offset))?;
                put(&mut changes, MAX_PATHS, Some(&settings.max_paths))?;
                db.update_session(id, changes).await?;
            }

            self.session_state
                .send_modify(|state| state.pointer_scan = settings);
            Ok(())
        };

        self.respond(result.await)
    }

    pub async fn set_layout(&self, layout: api::Layout) -> api::SessionResponse {
        let result = async {
            validate_layout(&layout)?;
            let _guard = self.session_lock.lock().await;

            let current = self.session_state.borrow().current;

            if let (Some(db), Some(id)) = (&self.db, current) {
                let mut changes = Changes::new();
                put(&mut changes, LAYOUT_SIDE, Some(&layout.side))?;
                put(&mut changes, LAYOUT_RESULTS, Some(&layout.results))?;
                put(&mut changes, LAYOUT_SCRATCH, Some(&layout.scratch))?;
                db.update_session(id, changes).await?;
            }

            self.session_state
                .send_modify(|state| state.layout = layout);
            Ok(())
        };

        self.respond(result.await)
    }

    /// Record the process the open session targets.
    pub(crate) async fn set_target(&self, target: api::SessionTarget) -> Result<()> {
        let _guard = self.session_lock.lock().await;
        let current = self.session_state.borrow().current;

        let Some(id) = current else {
            return Ok(());
        };

        if let Some(db) = &self.db {
            let mut changes = Changes::new();
            put(&mut changes, TARGET, Some(&target))?;
            db.update_session(id, changes).await?;
        }

        self.session_state.send_modify(|state| {
            if let Some(session) = state.sessions.iter_mut().find(|s| s.id == id) {
                session.target = Some(target);
            }
        });

        Ok(())
    }
}

async fn create(db: &Db, name: String) -> Result<api::SessionInfo> {
    let created = now();
    let mut changes = Changes::new();
    put(&mut changes, NAME, Some(&name))?;
    put(&mut changes, CREATED, Some(&created))?;
    put(&mut changes, OPENED, Some(&created))?;
    let id = db.create_session(changes).await?;

    Ok(api::SessionInfo {
        id,
        name,
        created,
        opened: created,
        target: None,
    })
}
