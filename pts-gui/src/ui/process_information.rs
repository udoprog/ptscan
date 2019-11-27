use crate::{prelude::*, MemoryInfo, MemoryKind};
use anyhow::Context as _;
use parking_lot::RwLock;
use ptscan::{AddressRange, MemoryInformation, ModulesState, ProcessHandle, ProcessThread};
use std::{cell::RefCell, convert::TryFrom as _, rc::Rc, sync::Arc};

struct Widgets {
    window: glib::WeakRef<Window>,
    name_entry: glib::WeakRef<Entry>,
    file_path_entry: glib::WeakRef<Entry>,
    modules_model: glib::WeakRef<ListStore>,
    threads_model: glib::WeakRef<ListStore>,
    memory_model: glib::WeakRef<ListStore>,
    checkboxes: Vec<glib::WeakRef<CheckButton>>,
    other_checkbox: glib::WeakRef<CheckButton>,
}

struct Decorator<'a> {
    modules: &'a ModulesState,
    threads: &'a [ProcessThread],
    state: State,
}

impl Decorator<'_> {
    fn map(&self, m: anyhow::Result<MemoryInformation>) -> anyhow::Result<MemoryInfo> {
        let m = m?;

        if let Some(module) =
            AddressRange::find_range_in_range(&self.modules.modules, |m| &m.range, &m.range)
        {
            return Ok(MemoryInfo {
                base: m,
                kind: MemoryKind::Module(module.name.clone()),
            });
        }

        if let Some(thread) =
            AddressRange::find_range_in_range(self.threads, |m| &m.stack, &m.range)
        {
            return Ok(MemoryInfo {
                base: m,
                kind: MemoryKind::ThreadStack(thread.id),
            });
        }

        Ok(MemoryInfo {
            base: m,
            kind: MemoryKind::None,
        })
    }

    /// Filter memory info.
    fn filter(&self, m: &anyhow::Result<MemoryInfo>) -> bool {
        let m = match m {
            Ok(m) => m,
            Err(..) => return true,
        };

        let state = &self.state;

        let readable = m.base.is_readable();
        let writable = m.base.is_writable();
        let executable = m.base.is_executable();

        if state.memory_other {
            return !readable && !writable && !executable && m.is_none();
        }

        if state.memory_modules && !m.is_module() {
            return false;
        }

        if state.memory_threadstacks && !m.is_threadstack() {
            return false;
        }

        if state.memory_readable && !m.base.is_readable() {
            return false;
        }

        if state.memory_writable && !m.base.is_writable() {
            return false;
        }

        if state.memory_executable && !m.base.is_executable() {
            return false;
        }

        true
    }
}

#[derive(Clone)]
struct State {
    memory_readable: bool,
    memory_writable: bool,
    memory_executable: bool,
    memory_other: bool,
    memory_modules: bool,
    memory_threadstacks: bool,
}

impl State {
    /// Make a filter out of the current state.
    pub fn decorator<'a>(
        self,
        modules: &'a ModulesState,
        threads: &'a [ProcessThread],
    ) -> Decorator<'a> {
        Decorator {
            modules,
            threads,
            state: self,
        }
    }
}
pub struct ProcessInformation {
    widgets: Widgets,
    state: State,
    handle: Option<Arc<RwLock<ProcessHandle>>>,
    modules: Option<ModulesState>,
    threads: Vec<ProcessThread>,
    memory: Vec<MemoryInfo>,
    refresh_task: Option<task::Handle>,
}

impl ProcessInformation {
    pub fn new(accel_group: &AccelGroup, clipboard: Rc<Clipboard>) -> (Rc<RefCell<Self>>, Window) {
        let builder = resource("process_information.glade").into_builder();

        let window = cascade! {
            builder.get_object::<Window>("window");
            ..add_accel_group(accel_group);
        };

        let state = State {
            memory_readable: true,
            memory_writable: true,
            memory_executable: false,
            memory_other: false,
            memory_modules: false,
            memory_threadstacks: false,
        };

        let memory_readable_check = cascade! {
            builder.get_object::<CheckButton>("memory_readable_check");
            ..set_active(state.memory_readable);
        };

        let memory_writable_check = cascade! {
            builder.get_object::<CheckButton>("memory_writable_check");
            ..set_active(state.memory_writable);
        };

        let memory_executable_check = cascade! {
            builder.get_object::<CheckButton>("memory_executable_check");
            ..set_active(state.memory_executable);
        };

        let memory_modules_check = cascade! {
            builder.get_object::<CheckButton>("memory_modules_check");
            ..set_active(state.memory_modules);
        };

        let memory_threadstacks_check = cascade! {
            builder.get_object::<CheckButton>("memory_threadstacks_check");
            ..set_active(state.memory_threadstacks);
        };

        let memory_other_check = cascade! {
            builder.get_object::<CheckButton>("memory_other_check");
            ..set_active(state.memory_other);
        };

        let slf = Rc::new(RefCell::new(ProcessInformation {
            widgets: Widgets {
                window: window.downgrade(),
                name_entry: builder.get_object::<Entry>("name_entry").downgrade(),
                file_path_entry: builder.get_object::<Entry>("file_path_entry").downgrade(),
                threads_model: builder.get_object::<ListStore>("threads_model").downgrade(),
                modules_model: builder.get_object::<ListStore>("modules_model").downgrade(),
                memory_model: builder.get_object::<ListStore>("memory_model").downgrade(),
                checkboxes: vec![
                    memory_readable_check.downgrade(),
                    memory_writable_check.downgrade(),
                    memory_executable_check.downgrade(),
                    memory_modules_check.downgrade(),
                    memory_threadstacks_check.downgrade(),
                ],
                other_checkbox: memory_other_check.downgrade(),
            },
            state,
            handle: None,
            modules: None,
            threads: Vec::new(),
            memory: Vec::new(),
            refresh_task: None,
        }));

        let clip = clipboard.handle();

        memory_readable_check.connect_toggled(clone!(slf => move |btn| {
            Self::memory_refilter(&slf, move |state| state.memory_readable = btn.get_active());
        }));

        memory_writable_check.connect_toggled(clone!(slf => move |btn| {
            Self::memory_refilter(&slf, move |state| state.memory_writable = btn.get_active());
        }));

        memory_executable_check.connect_toggled(clone!(slf => move |btn| {
            Self::memory_refilter(&slf, move |state| state.memory_executable = btn.get_active());
        }));

        memory_modules_check.connect_toggled(clone!(slf => move |btn| {
            Self::memory_refilter(&slf, move |state| state.memory_modules = btn.get_active());
        }));

        memory_threadstacks_check.connect_toggled(clone!(slf => move |btn| {
            Self::memory_refilter(&slf, move |state| state.memory_threadstacks = btn.get_active());
        }));

        memory_other_check.connect_toggled(clone!(slf => move |btn| {
            Self::memory_refilter(&slf, move |state| state.memory_other = btn.get_active());
        }));

        let weak_window = window.downgrade();

        cascade! {
            builder.get_object::<Button>("ok_button");
            ..connect_clicked(clone!(weak_window => move |_| {
                upgrade!(weak_window).hide();
            }));
        }

        cascade! {
            builder.get_object::<Button>("refresh_button");
            ..connect_clicked(clone!(slf => move |_| {
                Self::refresh(&slf);
            }));
        }

        clip.hook_tree(
            &builder.get_object("modules_tree"),
            clone!(slf => move |selection| {
                let slf = slf.borrow();
                let modules = slf.modules.as_ref()?;

                let (paths, model) = selection.get_selected_rows();

                let mut modules_list = Vec::new();

                for path in paths {
                    let iter = model.get_iter(&path)?;
                    let index = model.get_value(&iter, 0).get::<u64>().ok()?? as usize;
                    modules_list.extend(modules.modules.get(index).cloned());
                }

                Some(ClipboardBuffer::Modules(modules_list))
            }),
        );

        clip.hook_tree(
            &builder.get_object("threads_tree"),
            clone!(slf => move |selection| {
                let slf = slf.borrow();
                let (paths, model) = selection.get_selected_rows();

                let mut threads = Vec::new();

                for path in paths {
                    let iter = model.get_iter(&path)?;
                    let index = model.get_value(&iter, 0).get::<u64>().ok()?? as usize;
                    threads.extend(slf.threads.get(index).cloned());
                }

                Some(ClipboardBuffer::Threads(threads))
            }),
        );

        clip.hook_tree(
            &builder.get_object("memory_tree"),
            clone!(slf => move |selection| {
                let slf = slf.borrow();
                let (paths, model) = selection.get_selected_rows();

                let mut memory = Vec::new();

                for path in paths {
                    let iter = model.get_iter(&path)?;
                    let index = model.get_value(&iter, 0).get::<u64>().ok()?? as usize;
                    memory.extend(slf.memory.get(index).cloned());
                }

                Some(ClipboardBuffer::MemoryList(memory))
            }),
        );

        window.connect_show(clone!(slf => move |_| {
            Self::refresh(&slf);
        }));

        window.connect_hide(clone!(clip, slf => move |_| {
            clip.clear();

            let mut slf = slf.borrow_mut();
            slf.modules = None;
            slf.threads.clear();
            slf.memory.clear();
            slf.component_update();
        }));

        window.connect_delete_event(|window, _| {
            window.hide();
            Inhibit(true)
        });

        (slf, window)
    }

    /// Set the process handle to display in the process information view.
    pub fn set_handle(slf: &Rc<RefCell<Self>>, handle: Option<Arc<RwLock<ProcessHandle>>>) {
        {
            let mut slf = slf.borrow_mut();
            slf.handle = handle;
            let window = upgrade!(slf.widgets.window);

            if !window.is_visible() {
                return;
            }
        }

        Self::refresh(&slf);
    }

    /// Change the current state and refilter memory model.
    fn memory_refilter(slf: &Rc<RefCell<Self>>, update: impl FnOnce(&mut State)) {
        update(&mut slf.borrow_mut().state);
        Self::refresh(slf);
    }

    /// Update the set of components.
    fn component_update(&mut self) {
        let name_entry = upgrade!(self.widgets.name_entry);
        let file_path_entry = upgrade!(self.widgets.file_path_entry);
        let modules_model = upgrade!(self.widgets.modules_model);
        let threads_model = upgrade!(self.widgets.threads_model);
        let memory_model = upgrade!(self.widgets.memory_model);

        for c in &self.widgets.checkboxes {
            let c = upgrade!(c);
            c.set_sensitive(self.refresh_task.is_none() && !self.state.memory_other);
        }

        upgrade!(self.widgets.other_checkbox).set_sensitive(self.refresh_task.is_none());

        match &self.modules.as_ref().and_then(|m| m.process_name.as_ref()) {
            Some(name) => name_entry.set_text(name),
            None => name_entry.set_text("n/a"),
        }

        match &self
            .modules
            .as_ref()
            .and_then(|m| m.process_file_name.as_ref())
        {
            Some(file_name) => match file_name.to_str() {
                Some(file_name) => file_path_entry.set_text(file_name),
                None => file_path_entry
                    .set_text(&format!("non-utf8 path: {}", file_name.to_string_lossy())),
            },
            None => {
                file_path_entry.set_text("n/a");
            }
        }

        modules_model.clear();

        if let Some(modules) = &self.modules {
            for (index, module) in modules.modules.iter().enumerate() {
                let base_sort = match u64::try_from(module.range.base) {
                    Ok(n) => n,
                    Err(..) => continue,
                };

                let size_sort = match u64::try_from(module.range.size) {
                    Ok(n) => n,
                    Err(..) => continue,
                };

                let iter = modules_model.insert_with_values(
                    None,
                    &[1, 2, 3],
                    &[
                        &module.name,
                        &module.range.base.to_string(),
                        &module.range.size.to_string(),
                    ],
                );

                modules_model.set_value(&iter, 0, &(index as u64).to_value());
                modules_model.set_value(&iter, 4, &base_sort.to_value());
                modules_model.set_value(&iter, 5, &size_sort.to_value());
            }
        }

        threads_model.clear();

        for (index, thread) in self.threads.iter().enumerate() {
            let stack_exit = match &thread.stack_exit {
                Some(stack_exit) => stack_exit.to_string(),
                None => "n/a".to_string(),
            };

            let base_sort = match u64::try_from(thread.stack.base) {
                Ok(n) => n,
                Err(..) => continue,
            };

            let size_sort = match u64::try_from(thread.stack.size) {
                Ok(n) => n,
                Err(..) => continue,
            };

            let stack_exit_sort = match thread.stack_exit.map(|n| u64::try_from(n).ok()) {
                Some(n) => n,
                None => continue,
            };

            let iter = threads_model.insert_with_values(
                None,
                &[1, 2, 3, 4],
                &[
                    &(thread.id as u64),
                    &thread.stack.base.to_string(),
                    &thread.stack.size.to_string(),
                    &stack_exit,
                ],
            );

            threads_model.set_value(&iter, 0, &(index as u64).to_value());
            threads_model.set_value(&iter, 4, &base_sort.to_value());
            threads_model.set_value(&iter, 5, &size_sort.to_value());

            if let Some(stack_exit_sort) = stack_exit_sort {
                threads_model.set_value(&iter, 6, &stack_exit_sort.to_value());
            }
        }

        memory_model.clear();

        for (index, m) in self.memory.iter().enumerate() {
            let base_sort = match u64::try_from(m.base.range.base) {
                Ok(n) => n,
                Err(..) => continue,
            };

            let size_sort = match u64::try_from(m.base.range.size) {
                Ok(n) => n,
                Err(..) => continue,
            };

            let protection = m
                .base
                .protect
                .iter()
                .map(|p| p.as_str())
                .collect::<Vec<_>>()
                .join(" | ");

            let iter = memory_model.insert_with_values(
                None,
                &[1, 2, 3, 4],
                &[
                    &m.base.range.base.to_string(),
                    &m.base.range.size.to_string(),
                    &m.base.state.as_str(),
                    &protection,
                ],
            );

            match m.kind {
                MemoryKind::Module(ref m) => {
                    memory_model.set_value(&iter, 5, &format!("Module: {}", m).to_value());
                }
                MemoryKind::ThreadStack(id) => {
                    memory_model.set_value(
                        &iter,
                        5,
                        &format!("Thread Stack (id: {})", id).to_value(),
                    );
                }
                _ => (),
            }

            memory_model.set_value(&iter, 0, &(index as u64).to_value());
            memory_model.set_value(&iter, 6, &base_sort.to_value());
            memory_model.set_value(&iter, 7, &size_sort.to_value());
        }
    }

    fn refresh(slf_rc: &Rc<RefCell<Self>>) {
        let mut slf = slf_rc.borrow_mut();

        // task already in progress.
        if slf.refresh_task.is_some() {
            return;
        }

        let handle = match slf.handle.clone() {
            Some(handle) => handle,
            None => {
                slf.modules = None;
                slf.threads.clear();
                slf.memory.clear();
                slf.component_update();
                return;
            }
        };

        let state = slf.state.clone();

        let task = cascade! {
            task::Task::oneshot(slf_rc, move |_| {
                use ptscan::ProcessInfo as _;
                let handle = handle.read();
                let modules = handle.get_modules().context("failed to get information on modules")?;
                let threads = handle.get_threads_with_modules(&modules).context("failed to get information on threads")?;

                let decorator = state.decorator(&modules, &threads);

                let memory = handle.process
                    .virtual_memory_regions()
                    .map(|m| decorator.map(m))
                    .filter(|m| decorator.filter(m))
                    .collect::<anyhow::Result<_>>().context("failed to get memory information")?;

                Ok((modules, threads, memory))
            });
            ..then(|slf, result| {
                if let Ok((modules, threads, memory)) = result {
                    slf.modules = Some(modules);
                    slf.threads = threads;
                    slf.memory = memory;
                }

                slf.refresh_task = None;
                slf.component_update();
            });
        };

        slf.refresh_task = Some(task.run());
        slf.component_update();
    }
}
