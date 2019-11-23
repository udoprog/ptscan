use crate::{prelude::*, FilterOptions, MainMenu};
use std::{cell::RefCell, rc::Rc};

use self::Orientation::*;
use anyhow::anyhow;
use ptscan::ProcessId;

pub struct MainWindow {
    attached: Option<Rc<ptscan::ProcessHandle>>,
    attached_label: glib::WeakRef<gtk::Label>,
    selected_scan_result: Option<usize>,
    open_process_task: Option<task::Handle>,
    filter_options: Rc<RefCell<FilterOptions>>,
}

impl MainWindow {
    /// Construct a new main window and assocaite it with the given application.
    pub fn build(application: &gtk::Application, error: impl ErrorHandler) {
        let attached_label = cascade! {
            Label::new(Some("Not attached"));
            ..show();
        };

        let (filter_options, filter_options_frame) = FilterOptions::new();

        let main = Rc::new(RefCell::new(MainWindow {
            attached: None,
            attached_label: attached_label.downgrade(),
            selected_scan_result: None,
            open_process_task: None,
            filter_options,
        }));

        let accel_group = AccelGroup::new();

        let window = cascade! {
            ApplicationWindow::new(application);
            ..set_title("ptscan");
            ..set_position(WindowPosition::Center);
            ..set_size_request(400, 400);
            ..add_accel_group(&accel_group);
        };

        let attach = clone!(main => move |process_id| {
            Self::attach_process_id(&main, process_id);
        });

        let main_menu = MainMenu::new(&window, &accel_group, attach, error);

        let address_cell = CellRendererText::new();
        let value_cell = CellRendererText::new();
        let current_cell = CellRendererText::new();

        let scan_results = cascade! {
            TreeView::new();
            ..set_headers_visible(true);
            ..append_column(&cascade! {
                TreeViewColumn::new();
                    ..set_title("address");
                    ..pack_start(&address_cell, false);
                    ..add_attribute(&address_cell, "text", 0);
            });
            ..append_column(&cascade! {
                TreeViewColumn::new();
                    ..set_title("value");
                    ..pack_start(&value_cell, true);
                    ..add_attribute(&value_cell, "text", 1);
            });
            ..append_column(&cascade! {
                TreeViewColumn::new();
                    ..set_title("current");
                    ..pack_start(&current_cell, true);
                    ..add_attribute(&current_cell, "text", 2);
            });
            ..show_all();
        };

        scan_results
            .get_selection()
            .connect_changed(clone!(main => move |tree_selection| {
                let (left_model, iter) = optional!(tree_selection.get_selected());
                let path = optional!(left_model.get_path(&iter));
                let index = optional!(path.get_indices().into_iter().next());
                main.borrow_mut().selected_scan_result = Some(index as usize);
            }));

        let model = ListStore::new(&[ptscan::ProcessId::static_type(), String::static_type()]);
        scan_results.set_model(Some(&model));

        let scan_results = cascade! {
            gtk::ScrolledWindow::new(gtk::NONE_ADJUSTMENT, gtk::NONE_ADJUSTMENT);
            ..set_policy(gtk::PolicyType::Automatic, gtk::PolicyType::Automatic);
            ..add(&scan_results);
            ..show();
        };

        window.add(&cascade! {
            gtk::Box::new(Vertical, 10);
            ..pack_start(&main_menu, false, false, 0);
            ..pack_start(&cascade! {
                gtk::Box::new(Vertical, 10);
                ..set_border_width(10);
                ..pack_start(&attached_label, false, false, 0);
                ..pack_start(&filter_options_frame, false, false, 0);
                ..pack_start(&scan_results, true, true, 0);
                ..show();
            }, true, true, 0);
            ..show();
        });

        window.show();
    }

    pub fn attach_process_id(main: &Rc<RefCell<Self>>, process_id: ProcessId) {
        let mut slf = main.borrow_mut();

        let task = cascade! {
            task::Task::oneshot(main, move |_| {
                let process = ptscan::ProcessHandle::open(process_id)?;
                let process = process.ok_or_else(|| anyhow!("failed to attach, no such process"))?;
                Ok(process)
            });
            ..then(|main, process| {
                main.open_process_task = None;

                match process {
                    Ok(process) => {
                        if let Some(label) = main.attached_label.upgrade() {
                            label.set_text(&format!("Attached to: {}", process.name));
                        }

                        let process = Rc::new(process);
                        main.attached = Some(process.clone());
                        main.filter_options.borrow_mut().set_process(process);
                    }
                    Err(e) => {
                        if let Some(label) = main.attached_label.upgrade() {
                            label.set_text(&format!("Failed to attach: {}", e));
                        }
                    }
                }
            });
        };

        slf.open_process_task = Some(task.run());
    }
}
