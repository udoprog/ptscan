use crate::{prelude::*, MainMenu};
use std::{cell::RefCell, rc::Rc};

use self::Orientation::*;

pub struct MainWindow {
    attached: Option<ptscan::ProcessHandle>,
    attached_label: glib::WeakRef<gtk::Label>,
    error_label: glib::WeakRef<gtk::Label>,
    selected_scan_result: Option<usize>,
}

impl MainWindow {
    /// Construct a new main window and assocaite it with the given application.
    pub fn build(application: &gtk::Application, error: impl ErrorHandler) {
        let attached_label = Label::new(Some("Not attached"));
        let error_label = Label::new(None);

        let main = Rc::new(RefCell::new(MainWindow {
            attached: None,
            attached_label: attached_label.downgrade(),
            error_label: error_label.downgrade(),
            selected_scan_result: None,
        }));

        let accel_group = AccelGroup::new();

        let window = cascade! {
            ApplicationWindow::new(application);
            ..set_title("ptscan");
            ..set_position(WindowPosition::Center);
            ..set_size_request(400, 400);
            ..add_accel_group(&accel_group);
        };

        let filter = cascade! {
            Entry::new();
            ..set_tooltip_text(Some("Filter to apply to a scan"));
        };

        let apply_button = cascade! {
            Button::new();
            ..set_label("Apply Filter");
            ..connect_clicked(clone!(filter => move |_| {
                let buffer = filter.get_buffer();
                let text = buffer.get_text();
                println!("filter: {}", text);
            }));
        };

        let attach = clone!(main => move |process_id| {
            let mut main = main.borrow_mut();

            let process = match ptscan::ProcessHandle::open(process_id) {
                Ok(process) => process,
                Err(e) => {
                    main.show_error(e);
                    return;
                }
            };

            let process = match process {
                Some(process) => process,
                None => {
                    main.show_error("failed to attach (no such pid)");
                    return;
                }
            };

            main.attach_to(process);
        });

        let main_menu = MainMenu::new(&window, &accel_group, attach, error);

        let filter: Frame = cascade! {
            Frame::new(Some("filter"));
            ..add(&cascade! {
                gtk::Box::new(Horizontal, 10);
                ..set_border_width(10);
                ..pack_start(&filter, true, true, 0);
                ..pack_start(&apply_button, false, false, 0);
            });
        };

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
        };

        window.add(&cascade! {
            gtk::Box::new(Vertical, 10);
            ..pack_start(&main_menu, false, false, 0);
            ..pack_start(&cascade! {
                gtk::Box::new(Vertical, 10);
                ..set_border_width(10);
                ..pack_start(&error_label, false, false, 0);
                ..pack_start(&attached_label, false, false, 0);
                ..pack_start(&filter, false, false, 0);
                ..pack_start(&scan_results, true, true, 0);
            }, true, true, 0);
        });

        window.show_all();
        error_label.hide();
    }

    /// Show an error.
    pub fn show_error(&mut self, error: impl std::fmt::Display) {
        if let Some(label) = self.error_label.upgrade() {
            label.set_text(&error.to_string());
            label.show();
        }
    }

    /// Attach the main state to the specified process.
    pub fn attach_to(&mut self, process: ptscan::ProcessHandle) {
        println!("attached to: {}", process.name);

        if let Some(label) = self.attached_label.upgrade() {
            println!("attached to: {}", process.name);
            label.set_text(&format!("Attached to: {}", process.name));
        }

        self.attached = Some(process);
    }
}
