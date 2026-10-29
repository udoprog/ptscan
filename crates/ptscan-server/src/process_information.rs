//! Modules, threads and memory regions of the attached process.

use anyhow::{Context as _, Result};
use ptscan::{
    AddressRange, MemoryInformation, ModulesState, ProcessHandle, ProcessInfo as _, ProcessThread,
};

use crate::{Shared, lock, read};

fn region(
    m: &MemoryInformation,
    modules: &ModulesState,
    threads: &[ProcessThread],
) -> api::MemoryRegion {
    let module = AddressRange::find_range_in_range(&modules.modules, |m| &m.range, &m.range);

    let thread_stack = match module {
        Some(..) => None,
        None => AddressRange::find_range_in_range(threads, |t| &t.stack, &m.range),
    };

    api::MemoryRegion {
        base: m.range.base.into(),
        size: m.range.size.into(),
        state: m.state.as_str().to_owned(),
        ty: m.ty.map(|ty| ty.as_str().to_owned()),
        protect: m.protect.iter().map(|p| p.as_str().to_owned()).collect(),
        readable: m.is_readable(),
        writable: m.is_writable(),
        executable: m.is_executable(),
        module: module.map(|m| m.name.clone()),
        thread_stack: thread_stack.map(|t| t.id),
    }
}

fn matches(filter: &api::MemoryFilter, r: &api::MemoryRegion) -> bool {
    if filter.other {
        return !r.readable
            && !r.writable
            && !r.executable
            && r.module.is_none()
            && r.thread_stack.is_none();
    }

    (!filter.modules || r.module.is_some())
        && (!filter.thread_stacks || r.thread_stack.is_some())
        && (!filter.readable || r.readable)
        && (!filter.writable || r.writable)
        && (!filter.executable || r.executable)
}

fn collect(
    handle: &ProcessHandle,
    filter: &api::MemoryFilter,
    info: &mut api::ProcessInformation,
) -> Result<()> {
    let modules = handle
        .get_modules()
        .context("failed to get information on modules")?;

    info.name = modules.process_name.clone();
    info.file_name = modules
        .process_file_name
        .as_ref()
        .map(|f| f.to_string_lossy().into_owned());
    info.modules = modules
        .modules
        .iter()
        .map(|m| api::ModuleInformation {
            name: m.name.clone(),
            file_name: m.file_name.clone(),
            base: m.range.base.into(),
            size: m.range.size.into(),
        })
        .collect();

    let threads = handle
        .get_threads_with_modules(&modules)
        .context("failed to get information on threads")?;

    info.threads = threads
        .iter()
        .map(|t| api::ThreadInformation {
            id: t.id,
            stack_base: t.stack.base.into(),
            stack_size: t.stack.size.into(),
            stack_exit: t.stack_exit.map(u64::from),
        })
        .collect();

    for m in handle.process.virtual_memory_regions() {
        let m = m.context("failed to get memory information")?;
        let region = region(&m, &modules, &threads);

        if matches(filter, &region) {
            info.regions.push(region);
        }
    }

    Ok(())
}

impl Shared {
    pub(crate) fn process_information(
        &self,
        filter: &api::MemoryFilter,
    ) -> api::ProcessInformation {
        let mut info = api::ProcessInformation::default();

        let Some(handle) = lock(&self.handle).clone() else {
            info.error = Some("no process is attached".into());
            return info;
        };

        if let Err(e) = collect(&read(&handle), filter, &mut info) {
            info.error = Some(format!("{e:#}"));
        }

        info
    }
}
