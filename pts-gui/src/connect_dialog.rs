use self::Orientation::*;
use crate::prelude::*;
use std::{cell::RefCell, rc::Rc};

struct ProcessInfo {
    process_id: ptscan::ProcessId,
    #[allow(unused)]
    name: Option<String>,
}

#[derive(Default)]
pub struct ConnectDialog<A, E> {
    attach: A,
    processes: Vec<ProcessInfo>,
    selected: Option<usize>,
    weak_spinner: glib::WeakRef<Spinner>,
    weak_model: glib::WeakRef<ListStore>,
    error: E,
    refresh_in_progress: bool,
}

impl<A, E> ConnectDialog<A, E> {
    /// Construct a new connect dialog.
    pub fn new(attach: A, error: E) -> gtk::Window
    where
        A: Fn(ptscan::ProcessId) + 'static,
        E: ErrorHandler,
    {
        let model = ListStore::new(&[ptscan::ProcessId::static_type(), String::static_type()]);
        let spinner = Spinner::new();

        let dialog = Rc::new(RefCell::new(Self {
            attach,
            processes: Vec::new(),
            selected: None,
            weak_spinner: spinner.downgrade(),
            weak_model: model.downgrade(),
            error,
            refresh_in_progress: false,
        }));

        let window = cascade! {
            gtk::Window::new(gtk::WindowType::Toplevel);
            ..set_title("Attach to Process");
            ..set_position(WindowPosition::Center);
            ..set_size_request(400, 400);
        };

        let id_cell = cascade! {
            CellRendererText::new();
        };

        let name_cell = cascade! {
            CellRendererText::new();
        };

        let tree = cascade! {
            TreeView::new();
            ..set_headers_visible(true);
            ..append_column(&cascade! {
                TreeViewColumn::new();
                    ..set_title("pid");
                    ..pack_start(&id_cell, true);
                    ..add_attribute(&id_cell, "text", 0);
            });
            ..append_column(&cascade! {
                TreeViewColumn::new();
                    ..set_title("name");
                    ..pack_start(&name_cell, true);
                    ..add_attribute(&name_cell, "text", 1);
            });
        };

        cascade! {
            tree.get_selection();
            ..connect_changed(clone!(dialog => move |tree_selection| {
                let (left_model, iter) = optional!(tree_selection.get_selected());
                let path = optional!(left_model.get_path(&iter));
                let index = optional!(path.get_indices().into_iter().next());
                dialog.borrow_mut().selected = Some(index as usize);
            }));
        };

        tree.set_model(Some(&model));

        let tree = cascade! {
            gtk::ScrolledWindow::new(gtk::NONE_ADJUSTMENT, gtk::NONE_ADJUSTMENT);
            ..set_policy(gtk::PolicyType::Automatic, gtk::PolicyType::Automatic);
            ..add(&tree);
        };

        let weak_window = window.downgrade();

        let cancel_button = cascade! {
            gtk::Button::new_from_icon_name(Some("edit-delete"), IconSize::Button);
            ..set_label("Cancel");
            ..connect_clicked(clone!(weak_window => move |_| {
                let window = upgrade_weak!(weak_window);
                window.hide();
            }));
        };

        let weak_window = window.downgrade();

        let attach_button = cascade! {
            gtk::Button::new_from_icon_name(Some("list-add"), IconSize::Button);
            ..set_label("Attach");
            ..connect_clicked(clone!(dialog, weak_window => move |_| {
                let window = upgrade_weak!(weak_window);
                let dialog = dialog.borrow();
                let process_id = optional!(dialog.get_selected());
                (dialog.attach)(process_id);
                window.hide();
            }));
        };

        let buttons = cascade! {
            gtk::Box::new(Horizontal, 10);
                ..pack_start(&cancel_button, false, false, 0);
                ..pack_start(&attach_button, false, false, 0);
        };

        let window = cascade! {
            window;
            ..add(&cascade! {
                gtk::Box::new(Vertical, 10);
                ..set_border_width(10);
                ..pack_start(&spinner, true, true, 0);
                ..pack_start(&tree, true, true, 0);
                ..pack_start(&buttons, false, false, 0);
            });
            ..show_all();
        };

        // spinner should not be initially visible.
        spinner.hide();

        window.connect_show(clone!(dialog => move |_| {
            Self::refresh_processes(&dialog);
        }));

        window.connect_delete_event(clone!(dialog => move |window, _| {
            dialog.borrow_mut().clear();
            window.hide();
            Inhibit(true)
        }));

        window
    }

    /// Refresh the available process list.
    fn refresh_processes(dialog: &Rc<RefCell<Self>>)
    where
        A: 'static,
        E: ErrorHandler,
    {
        let mut slf = dialog.borrow_mut();

        if slf.refresh_in_progress {
            return;
        }

        slf.refresh_in_progress = true;

        if let Some(spinner) = slf.weak_spinner.upgrade() {
            spinner.show();
            spinner.start();
        }

        let error = slf.error.clone();

        let rx = task::oneshot(move || {
            let pids = ptscan::processes()?;
            let mut infos = Vec::new();

            for process_id in &pids {
                let result = ptscan::Process::builder()
                    .query_information()
                    .vm_read()
                    .build(*process_id);

                let process = match result {
                    Ok(process) => process,
                    Err(e) => {
                        error.report(e);
                        continue;
                    }
                };

                let name = process.module_base_name()?.into_string().ok();

                infos.push(ProcessInfo {
                    process_id: *process_id,
                    name,
                })
            }

            Ok(infos)
        });

        rx.attach(
            None,
            clone!(dialog => move |infos| {
                let mut slf = dialog.borrow_mut();
                slf.refresh_in_progress = false;

                if let Some(spinner) = slf.weak_spinner.upgrade() {
                    spinner.hide();
                    spinner.stop();
                }

                let infos = match infos {
                    Ok(infos) => infos,
                    Err(e) => {
                        slf.error.report(e);
                        return glib::Continue(false);
                    }
                };

                let model = upgrade_weak!(slf.weak_model, glib::Continue(false));

                for info in &infos {
                    model.insert_with_values(None, &[0, 1], &[&info.process_id, &info.name]);
                }

                slf.processes = infos;
                glib::Continue(false)
            }),
        );
    }

    /// Clear the current state of the dialog model.
    fn clear(&mut self) {
        self.selected = None;
        self.processes.clear();
    }

    /// Get the currently selected process id.
    fn get_selected(&self) -> Option<ptscan::ProcessId> {
        let index = self.selected?;
        self.processes.get(index).map(|info| info.process_id)
    }
}
