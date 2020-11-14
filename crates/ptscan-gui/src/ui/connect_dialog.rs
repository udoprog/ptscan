use crate::prelude::*;
use std::{cell::RefCell, rc::Rc};

struct Widgets {
    spinner: glib::WeakRef<Spinner>,
    model: glib::WeakRef<ListStore>,
    tree: glib::WeakRef<TreeView>,
}

struct ProcessInfo {
    process_id: ptscan::ProcessId,
    #[allow(unused)]
    name: Option<String>,
}

pub struct ConnectDialog<E> {
    on_attach: Option<Box<dyn Fn(&Self, ptscan::ProcessId)>>,
    widgets: Widgets,
    processes: Vec<ProcessInfo>,
    selected: Option<usize>,
    error: E,
    refresh_in_progress: Option<task::Handle>,
}

impl<E> ConnectDialog<E> {
    /// Construct a new connect dialog.
    pub fn new(error: E) -> (Rc<RefCell<ConnectDialog<E>>>, gtk::Window)
    where
        E: ErrorHandler,
    {
        let builder = resource("attach_dialog.glade").into_builder();

        let model = ListStore::new(&[
            u32::static_type(),
            ptscan::ProcessId::static_type(),
            String::static_type(),
        ]);
        let spinner = Spinner::new();

        let id_cell = cascade! {
            CellRendererText::new();
        };

        let name_cell = cascade! {
            CellRendererText::new();
        };

        let tree = cascade! {
            builder.get_object::<TreeView>("tree");
            ..set_enable_search(true);
            ..set_headers_visible(true);
            ..set_search_column(2);
            ..append_column(&cascade! {
                TreeViewColumn::new();
                ..set_title("pid");
                ..pack_start(&id_cell, true);
                ..add_attribute(&id_cell, "text", 1);
                ..set_clickable(true);
                ..set_sort_column_id(1);
            });
            ..append_column(&cascade! {
                TreeViewColumn::new();
                ..set_title("name");
                ..pack_start(&name_cell, true);
                ..add_attribute(&name_cell, "text", 2);
                ..set_clickable(true);
                ..set_sort_column_id(2);
            });
        };

        let slf = Rc::new(RefCell::new(Self {
            on_attach: None,
            widgets: Widgets {
                spinner: spinner.downgrade(),
                model: model.downgrade(),
                tree: tree.downgrade(),
            },
            processes: Vec::new(),
            selected: None,
            error,
            refresh_in_progress: None,
        }));

        let window = builder.get_object::<Window>("window");

        let weak_window = window.downgrade();

        tree.connect_row_activated(clone!(weak_window, slf => move |_, path, _| {
            let window = upgrade!(weak_window);

            {
                let slf = slf.borrow();
                let model = upgrade!(slf.widgets.model);
                let on_attach = optional!(&slf.on_attach);
                let iter = optional!(model.get_iter(path));
                let index = optional!(model.get_value(&iter, 0).get::<u32>().ok());
                let index = optional!(index);
                let process_id = optional!(slf.processes.get(index as usize)).process_id;
                on_attach(&*slf, process_id);
            }

            window.hide();
        }));

        tree.set_model(Some(&model));

        cascade! {
            builder.get_object::<Button>("cancel_button");
            ..connect_clicked(clone!(weak_window => move |_| {
                let window = upgrade!(weak_window);
                window.hide();
            }));
        };

        cascade! {
            builder.get_object::<Button>("ok_button");
            ..connect_clicked(clone!(slf, weak_window => move |_| {
                let window = upgrade!(weak_window);
                let slf = slf.borrow();
                let process_id = optional!(slf.get_selected());

                if let Some(on_attach) = &slf.on_attach {
                    on_attach(&*slf, process_id);
                }

                window.hide();
            }));
        };

        window.connect_show(clone!(slf => move |_| {
            Self::refresh_processes(&slf);
        }));

        window.connect_delete_event(clone!(slf => move |window, _| {
            slf.borrow_mut().clear();
            window.hide();
            Inhibit(true)
        }));

        (slf, window)
    }

    /// Setup a callback to run on attach events.
    pub fn on_attach<T>(&mut self, on_attach: T)
    where
        T: Fn(&Self, ptscan::ProcessId) + 'static,
    {
        self.on_attach = Some(Box::new(on_attach));
    }

    /// Refresh the available process list.
    fn refresh_processes(slf_rc: &Rc<RefCell<Self>>)
    where
        E: ErrorHandler,
    {
        let mut slf = slf_rc.borrow_mut();

        if slf.refresh_in_progress.is_some() {
            return;
        }

        if let Some(spinner) = slf.widgets.spinner.upgrade() {
            spinner.show();
            spinner.start();
        }

        let error = slf.error.clone();

        let task = cascade! {
            task::Task::oneshot(slf_rc, move |c| {
                let mut infos = Vec::new();

                for process_id in ptscan::processes()? {
                    // this one is probably not necessary. just to showcase.
                    if c.is_stopped() {
                        break;
                    }

                    let result = ptscan::Process::builder()
                        .query_information()
                        .vm_read()
                        .build(process_id)
                        .map_err(anyhow::Error::from);

                    let process = match result {
                        Ok(process) => process,
                        Err(e) => {
                            error.report(e.context(format!("failed to get information for process `{}`", process_id)));
                            continue;
                        }
                    };

                    let name = process.module_base_name()?.into_string().ok();

                    infos.push(ProcessInfo {
                        process_id,
                        name,
                    })
                }

                Ok(infos)
            });
            ..then(|dialog, infos| {
                if let Some(spinner) = dialog.widgets.spinner.upgrade() {
                    spinner.hide();
                    spinner.stop();
                }

                dialog.refresh_in_progress = None;

                match infos {
                    Ok(infos) => {
                        let model = upgrade!(dialog.widgets.model);

                        model.clear();

                        for (index, info) in infos.iter().enumerate() {
                            model.insert_with_values(None, &[0, 1, 2], &[&(index as u32), &info.process_id, &info.name]);
                        }

                        dialog.processes = infos;
                    }
                    Err(e) => {
                        dialog.error.report(e);
                    }
                }
            });
        };

        slf.refresh_in_progress = Some(task.run());
    }

    /// Clear the current state of the dialog model.
    fn clear(&mut self) {
        self.selected = None;
        self.processes.clear();
        self.refresh_in_progress = None;
    }

    /// Get the currently selected process id.
    fn get_selected(&self) -> Option<ptscan::ProcessId> {
        let tree = self.widgets.tree.upgrade()?;
        let (model, iter) = tree.get_selection().get_selected()?;
        let index = model.get_value(&iter, 0).get::<u32>().ok()??;
        Some(self.processes.get(index as usize)?.process_id)
    }
}
