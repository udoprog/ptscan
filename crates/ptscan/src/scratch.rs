//! The scratch list: pointers watched and edited outside of scans.
//!
//! With a database, the entries of the open session are its watches: they
//! are loaded when the session opens and written through on every change,
//! and their ids are the database ids. Values and addresses are never stored.

use std::collections::BTreeMap;
use std::sync::Arc;

use anyhow::{Result, anyhow};
use api::{ConfigField, ConfigKey};
use ptscan_core::{Address, PortablePointer, ProcessHandle, Proxy as _, Type, Value};

use crate::db::{self, Changes, Db, StoredWatch};
use crate::{Shared, lock, read};

const POINTER: ConfigKey = ConfigKey::field(ConfigField::Pointer);
const TYPE: ConfigKey = ConfigKey::field(ConfigField::Type);
const ORDER: ConfigKey = ConfigKey::field(ConfigField::Order);

/// Stored text that no longer parses, kept so that it is shown rather than
/// lost.
#[derive(Clone)]
struct Broken {
    text: String,
    error: String,
}

struct Entry {
    id: u64,
    order: u64,
    pointer: Result<PortablePointer, Broken>,
    value_type: Result<Type, Broken>,
}

impl Entry {
    fn usable(&self) -> Result<(PortablePointer, Type), String> {
        let pointer = match &self.pointer {
            Ok(pointer) => pointer,
            Err(broken) => return Err(format!("pointer `{}`: {}", broken.text, broken.error)),
        };

        let value_type = match &self.value_type {
            Ok(value_type) => *value_type,
            Err(broken) => return Err(format!("type `{}`: {}", broken.text, broken.error)),
        };

        Ok((pointer.clone(), value_type))
    }

    fn read(&self, handle: Option<&ProcessHandle>) -> api::ScratchEntry {
        let mut entry = api::ScratchEntry {
            id: self.id,
            pointer: match &self.pointer {
                Ok(pointer) => pointer.to_string(),
                Err(broken) => broken.text.clone(),
            },
            value_type: match &self.value_type {
                Ok(value_type) => value_type.to_string(),
                Err(broken) => broken.text.clone(),
            },
            address: None,
            value: None,
            error: None,
        };

        let (pointer, value_type) = match self.usable() {
            Ok(usable) => usable,
            Err(error) => {
                entry.error = Some(error);
                return entry;
            }
        };

        if let Some(handle) = handle {
            let mut proxy = handle.address_proxy(&pointer);
            entry.address = proxy.address().ok().flatten().map(u64::from);

            if let Ok((value, _)) = proxy.eval(value_type)
                && !matches!(value, Value::None)
            {
                entry.value = Some(value.to_string());
            }
        }

        entry
    }

    /// Load a stored watch, keeping what does not parse as [`Broken`].
    fn load(id: u64, config: &BTreeMap<ConfigKey, String>) -> Self {
        let text = |key| -> Result<String, Broken> {
            let missing = || Broken {
                text: String::new(),
                error: "nothing stored".into(),
            };

            let json = config.get(&key).ok_or_else(missing)?;

            match db::decode::<String>(json) {
                Ok(Some(text)) => Ok(text),
                Ok(None) => Err(Broken {
                    text: String::new(),
                    error: "stored under another version".into(),
                }),
                Err(error) => Err(Broken {
                    text: String::new(),
                    error: format!("{error:#}"),
                }),
            }
        };

        let pointer = text(POINTER)
            .and_then(|text| parse_pointer(&text).map_err(|error| Broken { text, error }));

        let value_type =
            text(TYPE).and_then(|text| parse_type(&text).map_err(|error| Broken { text, error }));

        let order = config
            .get(&ORDER)
            .and_then(|json| db::decode::<u64>(json).ok().flatten())
            .unwrap_or(id);

        Self {
            id,
            order,
            pointer,
            value_type,
        }
    }
}

#[derive(Default)]
pub(crate) struct Scratch {
    entries: Vec<Entry>,
    next_id: u64,
}

impl Scratch {
    /// The pointer and type of an entry, or why it cannot be used.
    pub(crate) fn get(&self, id: u64) -> Result<(PortablePointer, Type), String> {
        let entry = self
            .entries
            .iter()
            .find(|e| e.id == id)
            .ok_or_else(|| format!("no scratch entry {id}"))?;

        entry.usable()
    }

    fn next_order(&self) -> u64 {
        self.entries
            .iter()
            .map(|e| e.order.saturating_add(1))
            .max()
            .unwrap_or_default()
    }

    fn take_id(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id = self.next_id.saturating_add(1);
        id
    }

    fn replace(&mut self, mut entries: Vec<Entry>) {
        entries.sort_by_key(|e| (e.order, e.id));

        self.next_id = entries
            .iter()
            .map(|e| e.id.saturating_add(1))
            .max()
            .unwrap_or_default();

        self.entries = entries;
    }
}

fn parse_pointer(input: &str) -> Result<PortablePointer, String> {
    PortablePointer::parse(input.trim()).map_err(|e| format!("{e:#}"))
}

pub(crate) fn parse_type(input: &str) -> Result<Type, String> {
    input
        .trim()
        .parse::<Type>()
        .map_err(|e| e.to_string())
        .and_then(|ty| match ty {
            Type::None => Err("type `none` cannot be read".into()),
            ty => Ok(ty),
        })
}

fn put_text(changes: &mut Changes, key: ConfigKey, text: &str) -> Result<()> {
    changes.push((key, Some(db::encode(&text.to_owned())?)));
    Ok(())
}

fn put_order(changes: &mut Changes, order: u64) -> Result<()> {
    changes.push((ORDER, Some(db::encode(&order)?)));
    Ok(())
}

impl Shared {
    fn with_scratch<T>(&self, f: impl FnOnce(&mut Scratch, Option<&ProcessHandle>) -> T) -> T {
        let handle = lock(&self.handle).clone();
        let handle = handle.as_deref().map(read);
        let mut scratch = lock(&self.scratch);
        f(&mut scratch, handle.as_deref())
    }

    /// Run `f` on the scratch list on a blocking thread, since reading values
    /// reads process memory.
    async fn with_scratch_blocking<T>(
        self: &Arc<Self>,
        f: impl FnOnce(&Shared, &mut Scratch, Option<&ProcessHandle>) -> T + Send + 'static,
    ) -> Result<T>
    where
        T: Send + 'static,
    {
        let this = self.clone();
        let output = tokio::task::spawn_blocking(move || {
            this.with_scratch(|scratch, handle| f(&this, scratch, handle))
        })
        .await?;
        Ok(output)
    }

    /// The database and open session watches are written to, if any.
    fn watch_store(&self) -> Option<(&Db, u64)> {
        let current = self.session_state.borrow().current?;
        Some((self.db.as_ref()?, current))
    }

    fn list(scratch: &Scratch, handle: Option<&ProcessHandle>) -> api::ScratchList {
        api::ScratchList {
            entries: scratch.entries.iter().map(|e| e.read(handle)).collect(),
        }
    }

    fn publish_scratch(
        &self,
        scratch: &Scratch,
        handle: Option<&ProcessHandle>,
    ) -> api::ScratchList {
        let list = Self::list(scratch, handle);
        self.scratch_list.send_replace(list.clone());
        list
    }

    /// Replace the scratch list with a session's stored watches. Called with
    /// the session lock held, while nothing is attached.
    pub(crate) fn load_scratch(&self, watches: Vec<StoredWatch>) {
        let entries = watches
            .into_iter()
            .map(|(id, config)| Entry::load(id, &config))
            .collect();

        self.with_scratch(|scratch, handle| {
            scratch.replace(entries);
            self.publish_scratch(scratch, handle);
        });
    }

    /// The scratch list with values read now.
    pub fn scratch(&self) -> api::ScratchList {
        self.with_scratch(|scratch, handle| Self::list(scratch, handle))
    }

    pub async fn add_scratch(
        self: &Arc<Self>,
        request: api::AddScratchRequest,
    ) -> Result<api::ScratchResponse> {
        let mut response = api::ScratchResponse::default();
        let pointer = parse_pointer(&request.pointer).map_err(|e| response.pointer_error = Some(e));
        let value_type =
            parse_type(&request.value_type).map_err(|e| response.value_type_error = Some(e));

        let (Ok(pointer), Ok(value_type)) = (pointer, value_type) else {
            return Ok(response);
        };

        let _guard = self.session_lock.lock().await;
        let order = lock(&self.scratch).next_order();

        let id = match self.watch_store() {
            Some((db, session)) => {
                let mut changes = Changes::new();
                put_text(&mut changes, POINTER, &pointer.to_string())?;
                put_text(&mut changes, TYPE, &value_type.to_string())?;
                put_order(&mut changes, order)?;

                match db.create_watch(session, changes).await {
                    Ok(id) => id,
                    Err(error) => {
                        response.error = Some(format!("Saving the watch: {error:#}"));
                        return Ok(response);
                    }
                }
            }
            None => lock(&self.scratch).take_id(),
        };

        self.with_scratch_blocking(move |this, scratch, handle| {
            let entry = Entry {
                id,
                order,
                pointer: Ok(pointer),
                value_type: Ok(value_type),
            };

            scratch.next_id = scratch.next_id.max(id.saturating_add(1));
            response.entry = Some(entry.read(handle));
            scratch.entries.push(entry);
            this.publish_scratch(scratch, handle);
            response
        })
        .await
    }

    pub async fn edit_scratch(
        self: &Arc<Self>,
        request: api::EditScratchRequest,
    ) -> Result<api::ScratchResponse> {
        let mut response = api::ScratchResponse::default();

        let pointer = request
            .pointer
            .as_deref()
            .map(parse_pointer)
            .transpose()
            .map_err(|e| response.pointer_error = Some(e));

        let value_type = request
            .value_type
            .as_deref()
            .map(parse_type)
            .transpose()
            .map_err(|e| response.value_type_error = Some(e));

        let (Ok(pointer), Ok(value_type)) = (pointer, value_type) else {
            return Ok(response);
        };

        let _guard = self.session_lock.lock().await;

        let value_type = {
            let scratch = lock(&self.scratch);

            let Some(entry) = scratch.entries.iter().find(|e| e.id == request.id) else {
                response.error = Some(format!("no scratch entry {}", request.id));
                return Ok(response);
            };

            match (value_type, &entry.value_type) {
                (Some(value_type), Ok(old)) => Some(value_type.unsize(*old)),
                (value_type, _) => value_type,
            }
        };

        if let Some((db, session)) = self.watch_store() {
            let mut changes = Changes::new();

            if let Some(pointer) = &pointer {
                put_text(&mut changes, POINTER, &pointer.to_string())?;
            }

            if let Some(value_type) = value_type {
                put_text(&mut changes, TYPE, &value_type.to_string())?;
            }

            if let Err(error) = db
                .update_watches(session, vec![(request.id, changes)])
                .await
            {
                response.error = Some(format!("Saving the watch: {error:#}"));
                return Ok(response);
            }
        }

        self.with_scratch_blocking(move |this, scratch, handle| {
            let Some(entry) = scratch.entries.iter_mut().find(|e| e.id == request.id) else {
                response.error = Some(format!("no scratch entry {}", request.id));
                return response;
            };

            if let Some(pointer) = pointer {
                entry.pointer = Ok(pointer);
            }

            if let Some(value_type) = value_type {
                entry.value_type = Ok(value_type);
            }

            response.entry = Some(entry.read(handle));
            this.publish_scratch(scratch, handle);
            response
        })
        .await
    }

    /// Parse `value` as the entry's type and write it where its pointer
    /// follows to.
    pub fn write_scratch_value(
        &self,
        request: api::WriteScratchValueRequest,
    ) -> api::ScratchResponse {
        let mut response = api::ScratchResponse::default();

        self.with_scratch(|scratch, handle| {
            let Some(entry) = scratch.entries.iter().find(|e| e.id == request.id) else {
                response.error = Some(format!("no scratch entry {}", request.id));
                return;
            };

            let (pointer, value_type) = match entry.usable() {
                Ok(usable) => usable,
                Err(error) => {
                    response.error = Some(error);
                    return;
                }
            };

            let value = match value_type.parse(request.value.trim()) {
                Ok(value) => value,
                Err(e) => {
                    response.value_error = Some(e.to_string());
                    return;
                }
            };

            let Some(handle) = handle else {
                response.error = Some("not attached to a process".into());
                return;
            };

            if let Err(e) = write_value(handle, &pointer, &value) {
                response.error = Some(format!("{e:#}"));
            }

            response.entry = Some(entry.read(Some(handle)));
            self.publish_scratch(scratch, Some(handle));
        });

        response
    }

    /// Remove entries; when they cannot be removed from the database the list
    /// is left as it was.
    pub async fn remove_scratch(self: &Arc<Self>, ids: Vec<u64>) -> Result<api::ScratchList> {
        let _guard = self.session_lock.lock().await;

        if let Some((db, session)) = self.watch_store()
            && let Err(error) = db.delete_watches(session, ids.clone()).await
        {
            tracing::error!("Removing watches: {error:#}");
            return self
                .with_scratch_blocking(|_, scratch, handle| Self::list(scratch, handle))
                .await;
        }

        self.with_scratch_blocking(move |this, scratch, handle| {
            scratch.entries.retain(|e| !ids.contains(&e.id));
            this.publish_scratch(scratch, handle)
        })
        .await
    }

    /// Move the entries `ids` first, in that order, the rest following in the
    /// order they had. When the order cannot be saved the list is left as it
    /// was.
    pub async fn reorder_scratch(self: &Arc<Self>, ids: Vec<u64>) -> Result<api::ScratchList> {
        let _guard = self.session_lock.lock().await;

        let order = {
            let scratch = lock(&self.scratch);
            let mut order: Vec<u64> = Vec::with_capacity(scratch.entries.len());

            for id in &ids {
                if !order.contains(id) && scratch.entries.iter().any(|e| e.id == *id) {
                    order.push(*id);
                }
            }

            for entry in &scratch.entries {
                if !order.contains(&entry.id) {
                    order.push(entry.id);
                }
            }

            order
        };

        if let Some((db, session)) = self.watch_store() {
            let mut updates = Vec::new();

            for (index, id) in (0u64..).zip(&order) {
                let mut changes = Changes::new();
                put_order(&mut changes, index)?;
                updates.push((*id, changes));
            }

            if let Err(error) = db.update_watches(session, updates).await {
                tracing::error!("Reordering watches: {error:#}");
                return self
                    .with_scratch_blocking(|_, scratch, handle| Self::list(scratch, handle))
                    .await;
            }
        }

        self.with_scratch_blocking(move |this, scratch, handle| {
            for (index, id) in (0u64..).zip(&order) {
                if let Some(entry) = scratch.entries.iter_mut().find(|e| e.id == *id) {
                    entry.order = index;
                }
            }

            scratch.entries.sort_by_key(|e| (e.order, e.id));
            this.publish_scratch(scratch, handle)
        })
        .await
    }
}

fn write_value(handle: &ProcessHandle, pointer: &PortablePointer, value: &Value) -> Result<()> {
    let address = handle
        .address_proxy(pointer)
        .address()?
        .ok_or_else(|| anyhow!("pointer `{pointer}` does not follow to an address"))?;

    write_at(handle, address, value)
}

pub(crate) fn write_at(handle: &ProcessHandle, address: Address, value: &Value) -> Result<()> {
    let mut buf = Vec::new();

    if !value.encode(&handle.process, &mut buf) {
        return Err(anyhow!("value `{value}` cannot be encoded"));
    }

    if !handle.process.write_process_memory(address, &buf)? {
        return Err(anyhow!("could not write {} bytes at {address}", buf.len()));
    }

    Ok(())
}
