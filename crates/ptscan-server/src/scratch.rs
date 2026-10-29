//! The scratch list: pointers watched and edited outside of scans.

use anyhow::{Result, anyhow};
use ptscan::{PortablePointer, ProcessHandle, Proxy as _, Type, Value};

use crate::{Shared, lock, read};

struct Entry {
    id: u64,
    pointer: PortablePointer,
    value_type: Type,
}

impl Entry {
    fn read(&self, handle: Option<&ProcessHandle>) -> api::ScratchEntry {
        let mut entry = api::ScratchEntry {
            id: self.id,
            pointer: self.pointer.to_string(),
            value_type: self.value_type.to_string(),
            address: None,
            value: None,
        };

        if let Some(handle) = handle {
            let mut proxy = handle.address_proxy(&self.pointer);
            entry.address = proxy.address().ok().flatten().map(u64::from);

            if let Ok((value, _)) = proxy.eval(self.value_type)
                && !matches!(value, Value::None)
            {
                entry.value = Some(value.to_string());
            }
        }

        entry
    }
}

#[derive(Default)]
pub(crate) struct Scratch {
    entries: Vec<Entry>,
    next_id: u64,
}

fn parse_pointer(input: &str) -> Result<PortablePointer, String> {
    PortablePointer::parse(input.trim()).map_err(|e| format!("{e:#}"))
}

fn parse_type(input: &str) -> Result<Type, String> {
    input
        .trim()
        .parse::<Type>()
        .map_err(|e| e.to_string())
        .and_then(|ty| match ty {
            Type::None => Err("type `none` cannot be read".into()),
            ty => Ok(ty),
        })
}

impl Shared {
    fn with_scratch<T>(&self, f: impl FnOnce(&mut Scratch, Option<&ProcessHandle>) -> T) -> T {
        let handle = lock(&self.handle).clone();
        let handle = handle.as_deref().map(read);
        let mut scratch = lock(&self.scratch);
        f(&mut scratch, handle.as_deref())
    }

    fn list(scratch: &Scratch, handle: Option<&ProcessHandle>) -> api::ScratchList {
        api::ScratchList {
            entries: scratch.entries.iter().map(|e| e.read(handle)).collect(),
        }
    }

    fn publish_scratch(&self, scratch: &Scratch, handle: Option<&ProcessHandle>) {
        self.scratch_list.send_replace(Self::list(scratch, handle));
    }

    /// The scratch list with values read now.
    pub fn scratch(&self) -> api::ScratchList {
        self.with_scratch(|scratch, handle| Self::list(scratch, handle))
    }

    pub fn add_scratch(&self, request: api::AddScratchRequest) -> api::ScratchResponse {
        let mut response = api::ScratchResponse::default();
        let pointer = parse_pointer(&request.pointer).map_err(|e| response.pointer_error = Some(e));
        let value_type =
            parse_type(&request.value_type).map_err(|e| response.value_type_error = Some(e));

        let (Ok(pointer), Ok(value_type)) = (pointer, value_type) else {
            return response;
        };

        self.with_scratch(|scratch, handle| {
            let entry = Entry {
                id: scratch.next_id,
                pointer,
                value_type,
            };

            scratch.next_id += 1;
            response.entry = Some(entry.read(handle));
            scratch.entries.push(entry);
            self.publish_scratch(scratch, handle);
        });

        response
    }

    pub fn edit_scratch(&self, request: api::EditScratchRequest) -> api::ScratchResponse {
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
            return response;
        };

        self.with_scratch(|scratch, handle| {
            let Some(entry) = scratch.entries.iter_mut().find(|e| e.id == request.id) else {
                response.error = Some(format!("no scratch entry {}", request.id));
                return;
            };

            if let Some(pointer) = pointer {
                entry.pointer = pointer;
            }

            if let Some(value_type) = value_type {
                entry.value_type = value_type.unsize(entry.value_type);
            }

            response.entry = Some(entry.read(handle));
            self.publish_scratch(scratch, handle);
        });

        response
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

            let value = match entry.value_type.parse(request.value.trim()) {
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

            if let Err(e) = write_value(handle, &entry.pointer, &value) {
                response.error = Some(format!("{e:#}"));
            }

            response.entry = Some(entry.read(Some(handle)));
            self.publish_scratch(scratch, Some(handle));
        });

        response
    }

    pub fn remove_scratch(&self, ids: &[u64]) -> api::ScratchList {
        self.with_scratch(|scratch, handle| {
            scratch.entries.retain(|e| !ids.contains(&e.id));
            let list = Self::list(scratch, handle);
            self.scratch_list.send_replace(list.clone());
            list
        })
    }
}

fn write_value(handle: &ProcessHandle, pointer: &PortablePointer, value: &Value) -> Result<()> {
    let address = handle
        .address_proxy(pointer)
        .address()?
        .ok_or_else(|| anyhow!("pointer `{pointer}` does not follow to an address"))?;

    let mut buf = Vec::new();

    if !value.encode(&handle.process, &mut buf) {
        return Err(anyhow!("value `{value}` cannot be encoded"));
    }

    if !handle.process.write_process_memory(address, &buf)? {
        return Err(anyhow!("could not write {} bytes at {address}", buf.len()));
    }

    Ok(())
}
