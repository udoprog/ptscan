use crate::{clipboard, prelude::*};
use std::{cell::RefCell, rc::Rc};

struct Widgets {
    detach_item: glib::WeakRef<MenuItem>,
    undo_item: glib::WeakRef<MenuItem>,
    redo_item: glib::WeakRef<MenuItem>,
}

pub struct MainMenu {
    widgets: Widgets,
    on_detach: Option<Box<dyn Fn()>>,
    on_undo: Option<Box<dyn Fn()>>,
    on_redo: Option<Box<dyn Fn()>>,
    attached: bool,
    can_undo: bool,
    can_redo: bool,
}

impl MainMenu {
    pub fn new(
        builder: &Builder,
        window: glib::WeakRef<ApplicationWindow>,
        clipboard: Rc<Clipboard>,
        connect_dialog_window: glib::WeakRef<Window>,
        error_dialog_window: glib::WeakRef<Window>,
        process_information_window: glib::WeakRef<Window>,
    ) -> Rc<RefCell<Self>> {
        cascade! {
            builder.get_object::<MenuItem>("attach_item");
            ..connect_activate(clone!(connect_dialog_window => move |_| {
                let connect_dialog_window = upgrade!(connect_dialog_window);
                connect_dialog_window.show();
                connect_dialog_window.present();
            }));
        }

        let detach_item = cascade! {
            builder.get_object::<MenuItem>("detach_item");
        };

        let undo_item = cascade! {
            builder.get_object::<MenuItem>("undo_item");
        };

        let redo_item = cascade! {
            builder.get_object::<MenuItem>("redo_item");
        };

        cascade! {
            builder.get_object::<MenuItem>("error_dialog_item");
            ..connect_activate(clone!(error_dialog_window => move |_| {
                let w = upgrade!(error_dialog_window);
                w.show();
                w.present();
            }));
        }

        cascade! {
            builder.get_object::<MenuItem>("process_information_item");
            ..connect_activate(clone!(process_information_window => move |_| {
                let w = upgrade!(process_information_window);
                w.show();
                w.present();
            }));
        }

        cascade! {
            builder.get_object::<MenuItem>("about_item");
            ..connect_activate(clone!(window => move |_| {
                let window = upgrade!(window);
                cascade! {
                    AboutDialog::new();
                    ..set_website_label(Some("ptscan GitHub"));
                    ..set_website(Some("https://github.com/udoprog/ptscan"));
                    ..set_authors(&["John-John Tedro <udoprog@tedro.se>"]);
                    ..set_title("About ptscan");
                    ..set_transient_for(Some(&window));
                    ..run();
                    ..destroy();
                };
            }));
        }

        cascade! {
            builder.get_object::<MenuItem>("quit_item");
            ..connect_activate(clone!(window => move |_| {
                let window = upgrade!(window);
                window.destroy();
            }));
        };

        let json = gdk::Atom::intern("application/json");

        let json_clipboard = gtk::Clipboard::get(&json);
        let text_clipboard = gtk::Clipboard::get(&gdk::SELECTION_CLIPBOARD);

        cascade! {
            builder.get_object::<MenuItem>("copy_item");
            ..connect_activate(clone!(clipboard, json_clipboard, text_clipboard => move |_| {
                match clipboard.copy() {
                    clipboard::Copied::Rich(buffer) => {
                        let json = optional!(serde_json::to_string_pretty(&buffer).ok());

                        match &buffer {
                            ClipboardBuffer::String(ref text) => {
                                text_clipboard.set_text(text);
                            }
                            other => {
                                text_clipboard.set_text(&other.to_string());
                            }
                        }

                        json_clipboard.set_text(&json);
                    }
                    clipboard::Copied::Text(text) => {
                        let json = optional!(serde_json::to_string_pretty(&ClipboardBufferRef::String(&text)).ok());
                        json_clipboard.set_text(&json);
                        text_clipboard.set_text(&text);
                    }
                    clipboard::Copied::None => (),
                }
            }));
        };

        cascade! {
            builder.get_object::<MenuItem>("copy_as_json_item");
            ..connect_activate(clone!(clipboard, json_clipboard, text_clipboard => move |_| {
                let json = match clipboard.copy() {
                    clipboard::Copied::Rich(buffer) => {
                        optional!(serde_json::to_string_pretty(&buffer).ok())
                    }
                    clipboard::Copied::Text(text) => {
                        optional!(serde_json::to_string_pretty(&ClipboardBufferRef::String(&text)).ok())
                    }
                    clipboard::Copied::None => return,
                };

                text_clipboard.set_text(&json);
                json_clipboard.set_text(&json);
            }));
        };

        let paster = clipboard::Paster {
            rich: clone!(json_clipboard => move |cb| {
                json_clipboard.request_text(move |_, text| {
                    let cb = upgrade!(cb);
                    let text = optional!(text);
                    let buffer = optional!(serde_json::from_str::<ClipboardBuffer>(text).ok());
                    cb(&buffer);
                });
            }),
            text: clone!(text_clipboard => move |cb| {
                text_clipboard.request_text(move |_, text| {
                    let cb = upgrade!(cb);
                    let text = optional!(text);
                    cb(&text);
                });
            }),
        };

        cascade! {
            builder.get_object::<MenuItem>("paste_item");
            ..connect_activate(clone!(clipboard => move |_| {
                clipboard.paste(&paster);
            }));
        };

        let slf = Rc::new(RefCell::new(Self {
            widgets: Widgets {
                detach_item: detach_item.downgrade(),
                undo_item: undo_item.downgrade(),
                redo_item: redo_item.downgrade(),
            },
            on_detach: None,
            on_undo: None,
            on_redo: None,
            attached: false,
            can_undo: false,
            can_redo: false,
        }));

        detach_item.connect_activate(clone!(slf => move |_| {
            let on_detach = optional!(slf.borrow_mut().on_detach.take());
            on_detach();
            slf.borrow_mut().on_detach = Some(on_detach);
        }));

        undo_item.connect_activate(clone!(slf => move |_| {
            let on_undo = optional!(slf.borrow_mut().on_undo.take());
            on_undo();
            slf.borrow_mut().on_undo = Some(on_undo);
        }));

        redo_item.connect_activate(clone!(slf => move |_| {
            let on_redo = optional!(slf.borrow_mut().on_redo.take());
            on_redo();
            slf.borrow_mut().on_redo = Some(on_redo);
        }));

        slf.borrow_mut().update_components();
        slf
    }

    /// Register the on detach handle.
    pub fn on_detach(&mut self, on_detach: (impl Fn() + 'static)) {
        self.on_detach = Some(Box::new(on_detach));
    }

    /// Register a handle to call on undo actions.
    pub fn on_undo(&mut self, on_undo: (impl Fn() + 'static)) {
        self.on_undo = Some(Box::new(on_undo));
    }

    /// Register a handle to call on redo actions.
    pub fn on_redo(&mut self, on_redo: (impl Fn() + 'static)) {
        self.on_redo = Some(Box::new(on_redo));
    }

    pub fn set_attached(&mut self, attached: bool) {
        self.attached = attached;
        self.update_components();
    }

    pub fn set_can_redo(&mut self, can_redo: bool) {
        self.can_redo = can_redo;
        self.update_components();
    }

    pub fn set_can_undo(&mut self, can_undo: bool) {
        self.can_undo = can_undo;
        self.update_components();
    }

    fn update_components(&mut self) {
        let detach_item = upgrade!(self.widgets.detach_item);
        let undo_item = upgrade!(self.widgets.undo_item);
        let redo_item = upgrade!(self.widgets.redo_item);

        detach_item.set_sensitive(self.attached);
        undo_item.set_sensitive(self.can_undo);
        redo_item.set_sensitive(self.can_redo);
    }
}
