use crate::prelude::*;
use std::{cell::RefCell, rc::Rc};

struct Widgets {
    detach_item: glib::WeakRef<MenuItem>,
}

pub struct MainMenu {
    widgets: Widgets,
    on_detach: Option<Box<dyn Fn()>>,
    attached: bool,
}

impl MainMenu {
    pub fn new(
        builder: &Builder,
        window: glib::WeakRef<ApplicationWindow>,
        accel_group: &AccelGroup,
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

        let (key, modifier) = gtk::accelerator_parse("<Primary>Q");

        cascade! {
            builder.get_object::<MenuItem>("quit_item");
            ..connect_activate(clone!(window => move |_| {
                let window = upgrade!(window);
                window.destroy();
            }));
            ..add_accelerator("activate", accel_group, key, modifier, AccelFlags::VISIBLE);
        };

        let (key, modifier) = gtk::accelerator_parse("<Ctrl>C");

        cascade! {
            builder.get_object::<MenuItem>("copy_item");
            ..connect_activate(clone!(clipboard => move |_| {
                if let Some(buffer) = &*clipboard.get() {
                    let data = optional!(serde_json::to_string_pretty(buffer).ok());
                    let c = gtk::Clipboard::get(&gdk::SELECTION_CLIPBOARD);
                    c.set_text(&data);
                }
            }));
            ..add_accelerator("activate", accel_group, key, modifier, AccelFlags::VISIBLE);
        };

        let (key, modifier) = gtk::accelerator_parse("<Ctrl>V");

        cascade! {
            builder.get_object::<MenuItem>("paste_item");
            ..connect_activate(clone!(clipboard => move |_| {
                gtk::Clipboard::get(&gdk::SELECTION_CLIPBOARD).request_text(clone!(clipboard => move |_, text| {
                    let buffer = match text {
                        Some(text) => Some(match serde_json::from_str::<ClipboardBuffer>(text) {
                            Ok(buffer) => buffer,
                            Err(..) => ClipboardBuffer::String(text.to_string()),
                        }),
                        None => None,
                    };

                    clipboard.paste(buffer.as_ref());
                }));
            }));
            ..add_accelerator("activate", accel_group, key, modifier, AccelFlags::VISIBLE);
        };

        let slf = Rc::new(RefCell::new(Self {
            widgets: Widgets {
                detach_item: detach_item.downgrade(),
            },
            on_detach: None,
            attached: false,
        }));

        detach_item.connect_activate(clone!(slf => move |_| {
            let on_detach = optional!(slf.borrow_mut().on_detach.take());
            on_detach();
            slf.borrow_mut().on_detach = Some(on_detach);
        }));

        slf.borrow_mut().update_components();
        slf
    }

    /// Register the on detach handle.
    pub fn on_detach(&mut self, on_detach: (impl Fn() + 'static)) {
        self.on_detach = Some(Box::new(on_detach));
    }

    pub fn set_attached(&mut self, attached: bool) {
        self.attached = attached;
        self.update_components();
    }

    fn update_components(&mut self) {
        let detach_item = upgrade!(self.widgets.detach_item);
        detach_item.set_sensitive(self.attached);
    }
}
