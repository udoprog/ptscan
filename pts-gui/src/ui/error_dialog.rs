use self::Orientation::*;
use crate::prelude::*;
use chrono::{DateTime, Utc};
use std::{cell::RefCell, rc::Rc};

struct ErrorInfo {
    created_at: DateTime<Utc>,
    error_text: String,
}

#[derive(Default)]
struct State {
    errors: Vec<ErrorInfo>,
}

struct Widgets {
    container: glib::WeakRef<gtk::Box>,
}

pub struct ErrorDialog {
    state: State,
    widgets: Widgets,
}

impl ErrorDialog {
    /// Construct a new connect dialog.
    pub fn new() -> (Rc<RefCell<Self>>, Window) {
        let window = cascade! {
            Window::new(WindowType::Toplevel);
            ..set_title("Error Information");
            ..set_position(WindowPosition::Center);
            ..set_size_request(400, 400);
        };

        let container = cascade! {
            gtk::Box::new(Vertical, 10);
        };

        let dialog = Rc::new(RefCell::new(Self {
            widgets: Widgets {
                container: container.downgrade(),
            },
            state: State::default(),
        }));

        let container = cascade! {
            gtk::ScrolledWindow::new(gtk::NONE_ADJUSTMENT, gtk::NONE_ADJUSTMENT);
            ..set_policy(gtk::PolicyType::Automatic, gtk::PolicyType::Automatic);
            ..add(&container);
            ..show();
        };

        let weak_window = window.downgrade();

        let close_button = cascade! {
            gtk::Button::new_from_icon_name(Some("edit-delete"), IconSize::Button);
            ..set_label("Close");
            ..connect_clicked(clone!(weak_window => move |_| {
                upgrade!(weak_window).hide();
            }));
            ..show();
        };

        let buttons = cascade! {
            gtk::Box::new(Horizontal, 10);
                ..pack_start(&close_button, false, false, 0);
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
            dialog.borrow_mut().destroy_errors();
        }));

        window.connect_show(clone!(dialog => move |_| {
            dialog.borrow_mut().render_errors();
        }));

        (dialog, window)
    }

    /// Add an error to the state of the error dialog.
    pub fn add_error(&mut self, created_at: DateTime<Utc>, error: anyhow::Error) {
        use std::fmt::Write as _;

        let mut error_text = String::new();

        writeln!(error_text, "{}", error).unwrap();

        for e in error.chain().skip(1) {
            writeln!(error_text, "caused by: {}", e).unwrap();
        }

        let info = ErrorInfo {
            created_at,
            error_text,
        };

        {
            let container = upgrade!(self.widgets.container);

            if container.is_visible() {
                Self::render_error(&container, &info);
            }
        }

        self.state.errors.push(info);
    }

    /// Render error information into the dialog window.
    fn render_errors(&mut self) {
        let container = upgrade!(self.widgets.container);

        for info in &self.state.errors {
            Self::render_error(&container, info);
        }

        container.show();
    }

    /// Render a single error into the container.
    fn render_error(container: &impl gtk::ContainerExt, info: &ErrorInfo) {
        container.add(&cascade! {
            gtk::Box::new(Vertical, 0);
            ..pack_start(&cascade! {
                Label::new(None);
                ..set_halign(Align::Start);
                ..set_valign(Align::Start);
                ..set_markup(&format!("<b>{}:</b>", info.created_at));
                ..show();
            }, false, false, 0);
            ..pack_start(&cascade! {
                Label::new(None);
                ..set_halign(Align::Start);
                ..set_valign(Align::Start);
                ..set_markup(&info.error_text);
                ..show();
            }, false, false, 0);
            ..show();
        });
    }

    /// Destroy all errors in the container.
    fn destroy_errors(&mut self) {
        let container = upgrade!(self.widgets.container);

        container.hide();

        for child in container.get_children() {
            container.remove(&child);
        }
    }
}
