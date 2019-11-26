use crate::prelude::*;
use glib::signal::SignalHandlerId;
use parking_lot::RwLock;
use ptscan::{Address, Cached, Pointer, ProcessHandle, ScanResult, Type, Value};
use std::{cell::RefCell, rc::Rc, sync::Arc};

struct Widgets {
    address_entry: glib::WeakRef<Entry>,
    address_error: glib::WeakRef<Image>,
    last_address_label: glib::WeakRef<Label>,
    type_entry: glib::WeakRef<Entry>,
    type_error: glib::WeakRef<Image>,
    initial_label: glib::WeakRef<Label>,
    last_label: glib::WeakRef<Label>,
    value_label: glib::WeakRef<Label>,
}

struct Signals {
    address_entry: SignalHandlerId,
    type_entry: SignalHandlerId,
}

pub struct EditScanResultDialog {
    widgets: Widgets,
    settings: Arc<Settings>,
    signals: Option<Signals>,
    handle: Option<Arc<RwLock<ProcessHandle>>>,
    /// The result being edited.
    result: Option<Box<ScanResult>>,
    /// Save callback.
    on_save: Option<Box<dyn Fn(&Self, Box<ScanResult>)>>,
    refresh_timer: Option<glib::SourceId>,
    refresh_value_task: Option<task::Handle>,
    value: Option<Value>,
    last_address: Option<Address>,
}

impl Drop for EditScanResultDialog {
    fn drop(&mut self) {
        if let Some(timer) = self.refresh_timer.take() {
            glib::source_remove(timer);
        }
    }
}

impl EditScanResultDialog {
    /// Construct a new connect dialog.
    pub fn new(settings: Arc<Settings>) -> (Rc<RefCell<Self>>, Window) {
        let builder = resource("edit_scan_result_dialog.glade").into_builder();

        let window = builder.get_object::<Window>("window");
        let address_entry = builder.get_object::<Entry>("address_entry");
        let address_error = builder.get_object::<Image>("address_error");
        let last_address_label = builder.get_object::<Label>("last_address_label");
        let type_entry = builder.get_object::<Entry>("type_entry");
        let type_error = builder.get_object::<Image>("type_error");
        let initial_label = builder.get_object::<Label>("initial_label");
        let last_label = builder.get_object::<Label>("last_label");
        let value_label = builder.get_object::<Label>("value_label");
        let save_button = builder.get_object::<Button>("save_button");
        let close_button = builder.get_object::<Button>("close_button");

        let slf = Rc::new(RefCell::new(Self {
            widgets: Widgets {
                address_entry: address_entry.downgrade(),
                address_error: address_error.downgrade(),
                last_address_label: last_address_label.downgrade(),
                type_entry: type_entry.downgrade(),
                type_error: type_error.downgrade(),
                initial_label: initial_label.downgrade(),
                last_label: last_label.downgrade(),
                value_label: value_label.downgrade(),
            },
            settings,
            signals: None,
            handle: None,
            result: None,
            on_save: None,
            refresh_timer: None,
            refresh_value_task: None,
            last_address: None,
            value: None,
        }));

        slf.borrow_mut().signals = Some(Signals {
            address_entry: address_entry.connect_changed(clone!(slf => move |entry| {
                {
                    let text = optional!(entry.get_text());
                    let mut slf = slf.borrow_mut();

                    let Self {
                        ref mut result,
                        ref widgets,
                        ..
                    } = *slf;

                    let result = optional!(result);

                    let pointer = match Pointer::parse(text.as_str()) {
                        Ok(p) => p,
                        Err(..) => {
                            if let Some(image) = widgets.address_error.upgrade() {
                                image.show();
                            }

                            return;
                        }
                    };

                    if let Some(image) = widgets.address_error.upgrade() {
                        image.hide();
                    }

                    result.pointer = pointer;
                }

                Self::refresh_value(&slf);
            })),
            type_entry: type_entry.connect_changed(clone!(slf => move |entry| {
                {
                    let text = optional!(entry.get_text());
                    let mut slf = slf.borrow_mut();

                    let Self {
                        ref mut result,
                        ref widgets,
                        ..
                    } = *slf;

                    let result = optional!(result);

                    let ty = match str::parse::<Type>(text.as_str()) {
                        Ok(p) => p,
                        Err(..) => {
                            if let Some(image) = widgets.type_error.upgrade() {
                                image.show();
                            }

                            return;
                        }
                    };

                    if let Some(image) = widgets.type_error.upgrade() {
                        image.hide();
                    }

                    result.initial = ty.default_value();
                    result.last = None;
                }

                Self::refresh_value(&slf);
            })),
        });

        let weak_window = window.downgrade();

        close_button.connect_clicked(clone!(weak_window => move |_| {
            upgrade!(weak_window).hide();
        }));

        save_button.connect_clicked(clone!(weak_window, slf => move |_| {
            {
                let mut slf = slf.borrow_mut();
                let result = optional!(slf.result.take());

                if let Some(on_save) = &slf.on_save {
                    on_save(&*slf, result);
                }
            }

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

    /// Set callback to call when the window is saved.
    pub fn on_save<T>(&mut self, on_save: T)
    where
        T: 'static + Fn(&Self, Box<ScanResult>),
    {
        self.on_save = Some(Box::new(on_save));
    }

    /// Set the process handle to use.
    pub fn set_handle(&mut self, handle: Option<Arc<RwLock<ProcessHandle>>>) {
        self.handle = handle;
    }

    /// Set the scan result being edited.
    pub fn set_result(&mut self, result: Box<ScanResult>) {
        {
            let signals = optional!(&self.signals);

            let address_entry = upgrade!(self.widgets.address_entry).block(&signals.address_entry);
            let type_entry = upgrade!(self.widgets.type_entry).block(&signals.type_entry);

            let last = result.last();
            address_entry.set_text(&result.pointer.to_string());
            type_entry.set_text(&last.ty().to_string());
        }

        self.last_address = result.pointer.address();
        self.value = Some(result.last().clone());
        self.result = Some(result);
        self.update_components();
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
        let ty = result.initial.ty();

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
                } else {
                    slf.last_address = None;
                    slf.value = None;
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
