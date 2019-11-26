use crate::prelude::*;
use anyhow::Context as _;
use parking_lot::RwLock;
use ptscan::{MemoryInformation, ModulesState, ProcessHandle, ProcessThread};
use std::{cell::RefCell, rc::Rc, sync::Arc};

struct Widgets {
    name_entry: glib::WeakRef<Entry>,
    file_path_entry: glib::WeakRef<Entry>,
    modules_model: glib::WeakRef<ListStore>,
    threads_model: glib::WeakRef<ListStore>,
    memory_model: glib::WeakRef<ListStore>,
}

#[derive(Clone)]
struct State {
    memory_readable: bool,
    memory_writable: bool,
    memory_executable: bool,
    memory_other: bool,
}

pub struct ProcessInformation {
    widgets: Widgets,
    state: State,
    handle: Option<Arc<RwLock<ProcessHandle>>>,
    modules: Option<ModulesState>,
    threads: Vec<ProcessThread>,
    memory: Vec<MemoryInformation>,
    refresh_task: Option<task::Handle>,
}

impl ProcessInformation {
    pub fn new() -> (Rc<RefCell<Self>>, Window) {
        let builder = resource("process_information.glade").into_builder();
        let window = builder.get_object::<Window>("window");

        let state = State {
            memory_readable: true,
            memory_writable: true,
            memory_executable: true,
            memory_other: true,
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

        let memory_other_check = cascade! {
            builder.get_object::<CheckButton>("memory_other_check");
            ..set_active(state.memory_other);
        };

        let slf = Rc::new(RefCell::new(ProcessInformation {
            widgets: Widgets {
                name_entry: builder.get_object::<Entry>("name_entry").downgrade(),
                file_path_entry: builder.get_object::<Entry>("file_path_entry").downgrade(),
                threads_model: builder.get_object::<ListStore>("threads_model").downgrade(),
                modules_model: builder.get_object::<ListStore>("modules_model").downgrade(),
                memory_model: builder.get_object::<ListStore>("memory_model").downgrade(),
            },
            state,
            handle: None,
            modules: None,
            threads: Vec::new(),
            memory: Vec::new(),
            refresh_task: None,
        }));

        memory_readable_check.connect_toggled(clone!(slf => move |btn| {
            Self::memory_refilter(&slf, move |state| state.memory_readable = btn.get_active());
        }));

        memory_writable_check.connect_toggled(clone!(slf => move |btn| {
            Self::memory_refilter(&slf, move |state| state.memory_writable = btn.get_active());
        }));

        memory_executable_check.connect_toggled(clone!(slf => move |btn| {
            Self::memory_refilter(&slf, move |state| state.memory_executable = btn.get_active());
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

        window.connect_show(clone!(slf => move |_| {
            Self::refresh(&slf);
        }));

        window.connect_hide(clone!(slf => move |_| {
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
        slf.borrow_mut().handle = handle;
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
            for module in &modules.modules {
                let name = module.name.to_string_lossy();

                modules_model.insert_with_values(
                    None,
                    &[0, 1, 2],
                    &[
                        &&*name,
                        &module.range.base.to_string(),
                        &module.range.size.to_string(),
                    ],
                );
            }
        }

        threads_model.clear();

        for thread in &self.threads {
            let stack_exit = match &thread.stack_exit {
                Some(stack_exit) => stack_exit.to_string(),
                None => "n/a".to_string(),
            };

            threads_model.insert_with_values(
                None,
                &[0, 1, 2, 3],
                &[
                    &(thread.thread.thread_id() as u64),
                    &thread.stack.base.to_string(),
                    &thread.stack.size.to_string(),
                    &stack_exit,
                ],
            );
        }

        memory_model.clear();

        for m in &self.memory {
            let protection = m
                .protect
                .iter()
                .map(|p| p.as_str())
                .collect::<Vec<_>>()
                .join(" | ");

            memory_model.insert_with_values(
                None,
                &[0, 1, 2, 3],
                &[
                    &m.range.base.to_string(),
                    &m.range.size.to_string(),
                    &m.state.as_str(),
                    &protection,
                ],
            );
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

                let filter = move |m: &anyhow::Result<MemoryInformation>| {
                    if let Ok(m) = m {
                        if state.memory_readable && m.is_readable() {
                            return true;
                        }

                        if state.memory_writable && m.is_writable() {
                            return true;
                        }

                        if state.memory_executable && m.is_executable() {
                            return true;
                        }

                        state.memory_other
                    } else {
                        true
                    }
                };

                let memory: Vec<MemoryInformation> = handle.process
                    .virtual_memory_regions()
                    .filter(filter)
                    .collect::<anyhow::Result<_>>().context("failed to get memory information")?;

                Ok((modules, threads, memory))
            });
            ..then(|slf, result| {
                if let Ok((modules, threads, memory)) = result {
                    slf.modules = Some(modules);
                    slf.threads = threads;
                    slf.memory = memory;
                }

                slf.component_update();
                slf.refresh_task = None;
            });
        };

        slf.refresh_task = Some(task.run());
    }
}
