use self::Orientation::*;
use crate::prelude::*;
use glib::signal::SignalHandlerId;
use ptscan::{Pointer, ProcessHandle, ScanResult, Type};
use std::{cell::RefCell, rc::Rc, sync::Arc};

struct Widgets {
    address_entry: glib::WeakRef<Entry>,
    address_error: glib::WeakRef<Image>,
    type_entry: glib::WeakRef<Entry>,
    type_error: glib::WeakRef<Image>,
    value_label: glib::WeakRef<Label>,
}

struct Signals {
    address_entry_signal: SignalHandlerId,
    type_entry_signal: SignalHandlerId,
}

pub struct ScanResultDialog {
    widgets: Widgets,
    signals: Option<Signals>,
    handle: Option<Arc<ProcessHandle>>,
    /// The result being edited.
    result: Option<Box<ScanResult>>,
    /// Save callback.
    on_save: Option<Box<dyn Fn(&Self, Box<ScanResult>)>>,
    refresh_timer: Option<glib::SourceId>,
    refresh_value_task: Option<task::Handle>,
}

impl Drop for ScanResultDialog {
    fn drop(&mut self) {
        if let Some(timer) = self.refresh_timer.take() {
            glib::source_remove(timer);
        }
    }
}

impl ScanResultDialog {
    /// Construct a new connect dialog.
    pub fn new() -> (Rc<RefCell<Self>>, Window) {
        let window = cascade! {
            Window::new(WindowType::Toplevel);
            ..set_title("Edit Scan Result");
            ..set_position(WindowPosition::Center);
            ..set_size_request(-1, -1);
        };

        let address_entry = cascade! {
            Entry::new();
        };

        let address_error = cascade! {
            Image::new_from_icon_name(Some("dialog-error"), IconSize::Button);
        };

        let type_entry = cascade! {
            Entry::new();
        };

        let type_error = cascade! {
            Image::new_from_icon_name(Some("dialog-error"), IconSize::Button);
        };

        let value_label = cascade! {
            Label::new(None);
        };

        let container = cascade! {
            gtk::Box::new(Vertical, 10);
            ..pack_start(&cascade! {
                gtk::Box::new(Vertical, 2);
                ..pack_start(&cascade! {
                    Label::new(Some("Address:"));
                    ..set_halign(Align::Start);
                }, false, false, 0);
                ..pack_start(&cascade! {
                    gtk::Box::new(Horizontal, 5);
                    ..pack_start(&address_entry, true, true, 0);
                    ..pack_start(&address_error, false, false, 0);
                }, false, false, 0);
            }, false, false, 0);
            ..pack_start(&cascade! {
                gtk::Box::new(Horizontal, 2);
                ..pack_start(&cascade! {
                    Label::new(Some("Value:"));
                    ..set_halign(Align::Start);
                }, false, false, 0);
                ..pack_start(&value_label, false, false, 0);
            }, false, false, 0);
            ..pack_start(&cascade! {
                gtk::Box::new(Vertical, 2);
                ..pack_start(&cascade! {
                    Label::new(Some("Type:"));
                    ..set_halign(Align::Start);
                }, false, false, 0);
                ..pack_start(&cascade! {
                    gtk::Box::new(Horizontal, 5);
                    ..pack_start(&type_entry, true, true, 0);
                    ..pack_start(&type_error, false, false, 0);
                }, false, false, 0);
            }, false, false, 0);
            ..show_all();
        };

        address_error.hide();
        type_error.hide();

        let dialog = Rc::new(RefCell::new(Self {
            widgets: Widgets {
                address_entry: address_entry.downgrade(),
                address_error: address_error.downgrade(),
                type_entry: type_entry.downgrade(),
                type_error: type_error.downgrade(),
                value_label: value_label.downgrade(),
            },
            signals: None,
            handle: None,
            result: None,
            on_save: None,
            refresh_timer: None,
            refresh_value_task: None,
        }));

        let address_entry_signal = address_entry.connect_changed(clone!(dialog => move |entry| {
            {
                let text = optional!(entry.get_text());
                let mut dialog = dialog.borrow_mut();

                let Self {
                    ref mut result,
                    ref widgets,
                    ..
                } = *dialog;

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

            Self::refresh_value(&dialog);
        }));

        let type_entry_signal = type_entry.connect_changed(clone!(dialog => move |entry| {
            {
                let text = optional!(entry.get_text());
                let mut dialog = dialog.borrow_mut();

                let Self {
                    ref mut result,
                    ref widgets,
                    ..
                } = *dialog;

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

            Self::refresh_value(&dialog);
        }));

        dialog.borrow_mut().signals = Some(Signals {
            address_entry_signal,
            type_entry_signal,
        });

        let weak_window = window.downgrade();

        let close_button = cascade! {
            gtk::Button::new_from_icon_name(Some("edit-delete"), IconSize::Button);
            ..set_label("Close");
            ..connect_clicked(clone!(weak_window => move |_| {
                upgrade_weak!(weak_window).hide();
            }));
            ..show();
        };

        let save_button = cascade! {
            gtk::Button::new_from_icon_name(Some("edit-save"), IconSize::Button);
            ..set_label("Save");
            ..connect_clicked(clone!(weak_window, dialog => move |_| {
                {
                    let mut dialog = dialog.borrow_mut();
                    let result = optional!(dialog.result.take());

                    if let Some(on_save) = &dialog.on_save {
                        on_save(&*dialog, result);
                    }
                }

                upgrade_weak!(weak_window).hide();
            }));
            ..show();
        };

        let buttons = cascade! {
            gtk::Box::new(Horizontal, 10);
                ..pack_start(&close_button, false, false, 0);
                ..pack_start(&save_button, false, false, 0);
                ..show();
        };

        let window = cascade! {
            window;
            ..add(&cascade! {
                gtk::Box::new(Vertical, 10);
                ..set_border_width(10);
                ..pack_start(&container, true, true, 0);
                ..pack_start(&buttons, false, false, 0);
                ..show();
            });
        };

        window.connect_delete_event(|window, _| {
            window.hide();
            Inhibit(true)
        });

        window.connect_hide(clone!(dialog => move |_| {
            let _ = dialog.borrow_mut();
        }));

        window.connect_show(clone!(dialog => move |_| {
            let _ = dialog.borrow_mut();
        }));

        dialog.borrow_mut().refresh_timer = Some(glib::source::timeout_add_local(
            500,
            clone!(dialog => move || {
                Self::refresh_value(&dialog);
                glib::Continue(true)
            }),
        ));

        (dialog, window)
    }

    /// Set callback to call when the window is saved.
    pub fn on_save<T>(&mut self, on_save: T)
    where
        T: 'static + Fn(&Self, Box<ScanResult>),
    {
        self.on_save = Some(Box::new(on_save));
    }

    /// Set the process handle to use.
    pub fn set_handle(&mut self, handle: Option<Arc<ProcessHandle>>) {
        self.handle = handle;
    }

    /// Set the scan result being edited.
    pub fn set_result(&mut self, result: Box<ScanResult>) {
        let signals = optional!(&self.signals);
        let address_entry = upgrade_weak!(self.widgets.address_entry);
        let type_entry = upgrade_weak!(self.widgets.type_entry);

        address_entry.block_signal(&signals.address_entry_signal);
        type_entry.block_signal(&signals.type_entry_signal);

        address_entry.set_text(&result.pointer.to_string());
        type_entry.set_text(&result.last().ty().to_string());

        address_entry.unblock_signal(&signals.address_entry_signal);
        type_entry.unblock_signal(&signals.type_entry_signal);

        upgrade_weak!(self.widgets.value_label).set_text(&result.last().to_string());
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
        let ty = result.initial.ty();

        let task = cascade! {
            task::Task::oneshot(dialog, move |_| {
                let value = handle.address_proxy(&pointer).eval(ty)?;
                Ok(value)
            });
            ..then(move |dialog, value| {
                let result = optional!(&mut dialog.result);

                if let Ok((value, _)) = value {
                    upgrade_weak!(dialog.widgets.value_label).set_text(&value.to_string());
                    result.last = Some(value);
                }

                dialog.refresh_value_task = None;
            });
        };

        slf.refresh_value_task = Some(task.run());
    }
}
