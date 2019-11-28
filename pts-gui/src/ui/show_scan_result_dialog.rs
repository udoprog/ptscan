use crate::prelude::*;
use parking_lot::RwLock;
use ptscan::{Address, Cached, ProcessHandle, ScanResult, Value};
use std::{cell::RefCell, rc::Rc, sync::Arc};

struct Widgets {
    address_label: glib::WeakRef<Label>,
    last_address_label: glib::WeakRef<Label>,
    type_label: glib::WeakRef<Label>,
    initial_label: glib::WeakRef<Label>,
    last_label: glib::WeakRef<Label>,
    value_label: glib::WeakRef<Label>,
}

pub struct ShowScanResultDialog {
    widgets: Widgets,
    settings: Arc<Settings>,
    handle: Option<Arc<RwLock<ProcessHandle>>>,
    /// The result being edited.
    result: Option<Box<ScanResult>>,
    refresh_timer: Option<glib::SourceId>,
    refresh_value_task: Option<task::Handle>,
    value: Option<Value>,
    last_address: Option<Address>,
}

impl Drop for ShowScanResultDialog {
    fn drop(&mut self) {
        if let Some(timer) = self.refresh_timer.take() {
            glib::source_remove(timer);
        }
    }
}

impl ShowScanResultDialog {
    /// Construct a new connect dialog.
    pub fn new(settings: Arc<Settings>) -> (Rc<RefCell<Self>>, Window) {
        let builder = resource("show_scan_result_dialog.glade").into_builder();

        let window = builder.get_object::<Window>("window");
        let address_label = builder.get_object::<Label>("address_label");
        let last_address_label = builder.get_object::<Label>("last_address_label");
        let type_label = builder.get_object::<Label>("type_label");
        let initial_label = builder.get_object::<Label>("initial_label");
        let last_label = builder.get_object::<Label>("last_label");
        let value_label = builder.get_object::<Label>("value_label");
        let close_button = builder.get_object::<Button>("close_button");

        let slf = Rc::new(RefCell::new(Self {
            widgets: Widgets {
                address_label: address_label.downgrade(),
                last_address_label: last_address_label.downgrade(),
                type_label: type_label.downgrade(),
                initial_label: initial_label.downgrade(),
                last_label: last_label.downgrade(),
                value_label: value_label.downgrade(),
            },
            settings,
            handle: None,
            result: None,
            refresh_timer: None,
            refresh_value_task: None,
            value: None,
            last_address: None,
        }));

        let weak_window = window.downgrade();

        close_button.connect_clicked(clone!(weak_window => move |_| {
            upgrade!(weak_window).hide();
        }));

        window.connect_delete_event(|window, _| {
            window.hide();
            Inhibit(true)
        });

        window.connect_hide(clone!(slf => move |_| {
            slf.borrow_mut().clear();
        }));

        window.connect_show(clone!(slf => move |_| {
            let _ = slf.borrow_mut();
        }));

        slf.borrow_mut().refresh_timer = Some(glib::source::timeout_add_local(
            500,
            clone!(slf => move || {
                Self::refresh_value(&slf);
                glib::Continue(true)
            }),
        ));

        (slf, window)
    }

    /// Set the process handle to use.
    pub fn set_handle(&mut self, handle: Option<Arc<RwLock<ProcessHandle>>>) {
        self.handle = handle;
    }

    /// Set the scan result being edited.
    pub fn set_result(&mut self, result: Box<ScanResult>) {
        let address_label = upgrade!(self.widgets.address_label);
        let type_label = upgrade!(self.widgets.type_label);

        address_label.set_text(&result.pointer.to_string());
        type_label.set_text(&result.last_type().to_string());

        upgrade!(self.widgets.initial_label).set_text(&result.initial.to_string());
        upgrade!(self.widgets.last_label).set_text(&result.last().to_string());
        upgrade!(self.widgets.value_label).set_text(&result.last().to_string());
        self.result = Some(result);
    }

    /// Refresh the current set of results.
    pub fn refresh_value(dialog: &Rc<RefCell<Self>>) {
        let mut slf = dialog.borrow_mut();

        // waiting for refresh already in progress.
        if slf.refresh_value_task.is_some() {
            return;
        }

        let result = optional!(&slf.result);
        let handle = optional!(slf.handle.clone());

        let pointer = result.pointer.clone();
        let ty = result.initial_type();

        let task = cascade! {
            task::Task::oneshot(dialog, move |_| {
                let handle = match handle.try_read() {
                    Some(handle) => handle,
                    None => return Ok(None),
                };

                let mut proxy = handle.address_proxy(&pointer);
                let value = proxy.eval(ty)?.0;
                Ok(Some((value, proxy.followed)))
            });
            ..then(move |slf, value| {
                if let Ok(Some((value, address))) = value {
                    if let Cached::Some(address) = address {
                        slf.last_address = address;
                    }

                    slf.value = Some(value);
                }

                slf.refresh_value_task = None;
                slf.update_components();
            });
        };

        slf.refresh_value_task = Some(task.run());
    }

    fn clear(&mut self) {
        self.result = None;
        self.value = None;
        self.last_address = None;
        self.update_components();
    }

    fn update_components(&mut self) {
        let last_address_label = upgrade!(self.widgets.last_address_label);

        if let Some(address) = &self.last_address {
            last_address_label.set_text(&address.to_string());
        } else {
            last_address_label.set_text("?");
        }

        let value_label = upgrade!(self.widgets.value_label);

        if let Some(value) = &self.value {
            let differ = match &self.result {
                Some(result) => result.last() != value,
                _ => false,
            };

            if differ {
                value_label.set_attributes(Some(&self.settings.highlight_color));
            } else {
                value_label.set_attributes(None);
            }

            value_label.set_text(&value.to_string());
        } else {
            value_label.set_text("?");
        }

        let initial_label = upgrade!(self.widgets.initial_label);
        let last_label = upgrade!(self.widgets.last_label);

        if let Some(result) = &self.result {
            initial_label.set_text(&result.initial.to_string());
            last_label.set_text(&result.last().to_string());
        } else {
            initial_label.set_text("");
            last_label.set_text("");
        }
    }
}
