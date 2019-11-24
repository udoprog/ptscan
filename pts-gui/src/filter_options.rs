use self::Orientation::*;
use crate::prelude::*;
use ptscan::{FilterExpr, ProcessHandle, ValueExpr};
use std::{cell::RefCell, rc::Rc, sync::Arc};

pub struct State {
    /// The current filter.
    pub filter_expr: Option<FilterExpr>,
    /// The current content of the filter textbox.
    pub filter_expr_text: String,
    /// The current value expression in use.
    pub value_expr: ValueExpr,
    /// Current text for the value expression.
    pub value_expr_text: String,
}

struct Widgets {
    filter_expr_text: glib::WeakRef<Entry>,
    filter_expr_error: glib::WeakRef<Label>,
    value_expr_text: glib::WeakRef<Entry>,
    value_expr_error: glib::WeakRef<Label>,
    reset_button: glib::WeakRef<Button>,
    scan_button: glib::WeakRef<Button>,
    refresh_button: glib::WeakRef<Button>,
}

#[derive(Default)]
pub struct Runtime {
    /// Runtime state.
    process: Option<Arc<ProcessHandle>>,
}

pub struct FilterOptions {
    pub state: State,
    runtime: Runtime,
    widgets: Widgets,
}

impl FilterOptions {
    /// Construct a widget representing the current state.
    pub fn new<Scan, Reset>(scan: Scan, reset: Reset) -> (Rc<RefCell<Self>>, Frame)
    where
        Scan: 'static + Fn(),
        Reset: 'static + Fn(),
    {
        // TODO: make serializable.
        let state = State {
            filter_expr: None,
            filter_expr_text: String::from(""),
            value_expr: ValueExpr::Value,
            value_expr_text: String::from("value"),
        };

        let filter_expr_text = cascade! {
            Entry::new();
            ..set_hexpand(true);
            ..set_tooltip_text(Some("Filter to apply to a scan"));
            ..set_text(&state.filter_expr_text);
        };

        let scan_button = cascade! {
            Button::new();
            ..set_label("Scan / Filter");
            ..connect_clicked(move |_| {
                scan();
            });
        };

        let reset_button = cascade! {
            Button::new();
            ..set_label("Reset");
            ..connect_clicked(move |_| {
                reset();
            });
        };

        let value_expr_text = cascade! {
            Entry::new();
            ..set_hexpand(true);
            ..set_tooltip_text(Some("Value expression to use for each scan result"));
            ..set_text(&state.value_expr_text);
        };

        let refresh_button = cascade! {
            Button::new();
            ..set_label("Refresh");
            ..connect_clicked(|_| {
                println!("REFRESH");
            });
        };

        let filter_expr_error = cascade! {
            Label::new(None);
            ..set_hexpand(true);
            ..set_halign(Align::Start);
        };

        let value_expr_error = cascade! {
            Label::new(None);
            ..set_hexpand(true);
            ..set_halign(Align::Start);
        };

        let slf = Rc::new(RefCell::new(FilterOptions {
            state,
            widgets: Widgets {
                filter_expr_text: filter_expr_text.downgrade(),
                filter_expr_error: filter_expr_error.downgrade(),
                value_expr_text: value_expr_text.downgrade(),
                value_expr_error: value_expr_error.downgrade(),
                reset_button: reset_button.downgrade(),
                scan_button: scan_button.downgrade(),
                refresh_button: refresh_button.downgrade(),
            },
            runtime: Default::default(),
        }));

        filter_expr_text.connect_changed(clone!(slf => move |expr| {
            let mut slf = slf.borrow_mut();
            slf.state.filter_expr_text = expr.get_buffer().get_text();
            slf.parse_filter_expr();
        }));

        value_expr_text.connect_changed(clone!(slf => move |expr| {
            let mut slf = slf.borrow_mut();
            slf.state.value_expr_text = expr.get_buffer().get_text();
            slf.parse_value_expr();
        }));

        let frame: Frame = cascade! {
            Frame::new(Some("Options"));
            ..add(&cascade! {
                gtk::Box::new(Vertical, 10);
                ..set_border_width(10);
                ..pack_start(&cascade! {
                    gtk::Box::new(Horizontal, 10);
                    ..pack_start(&filter_expr_text, true, true, 0);
                    ..pack_start(&scan_button, false, false, 0);
                    ..pack_start(&reset_button, false, false, 0);
                }, false, false, 0);
                ..pack_start(&filter_expr_error, false, false, 0);
                ..pack_start(&cascade! {
                    gtk::Box::new(Horizontal, 10);
                    ..pack_start(&value_expr_text, true, true, 0);
                    ..pack_start(&refresh_button, false, false, 0);
                }, false, false, 0);
                ..pack_start(&value_expr_error, false, false, 0);
            });
        };

        frame.show_all();
        value_expr_error.hide();
        filter_expr_error.hide();
        (slf, frame)
    }

    pub fn sync_state(&mut self) {
        if let Some(entry) = self.widgets.filter_expr_text.upgrade() {
            entry.set_text(&self.state.filter_expr_text);
        }

        if let Some(entry) = self.widgets.value_expr_text.upgrade() {
            entry.set_text(&self.state.value_expr_text);
        }
    }

    /// Parse the current filter expression.
    pub fn parse_filter_expr(&mut self) {
        let process = optional!(&self.runtime.process);

        if self.state.filter_expr_text.is_empty() {
            if let Some(error) = self.widgets.filter_expr_error.upgrade() {
                error.hide();
            }

            return;
        }

        match FilterExpr::parse(&self.state.filter_expr_text, &process.process) {
            Ok(filter_expr) => {
                self.state.filter_expr = Some(filter_expr);

                if let Some(error) = self.widgets.filter_expr_error.upgrade() {
                    error.hide();
                }
            }
            Err(..) => {
                if let Some(error) = self.widgets.filter_expr_error.upgrade() {
                    error.set_text("bad filter, try something like: `value == 42`");
                    error.show();
                }
            }
        }
    }

    /// Parse the current global value expression.
    pub fn parse_value_expr(&mut self) {
        let process = optional!(&self.runtime.process);

        if self.state.value_expr_text.is_empty() {
            if let Some(error) = self.widgets.value_expr_error.upgrade() {
                error.hide();
            }
        }

        match ValueExpr::parse(&self.state.value_expr_text, &process.process) {
            Ok(value_expr) => {
                self.state.value_expr = value_expr;

                if let Some(error) = self.widgets.value_expr_error.upgrade() {
                    error.hide();
                }
            }
            Err(..) => {
                if let Some(error) = self.widgets.value_expr_error.upgrade() {
                    error.set_text("bad expression, try something like: `value`");
                    error.show();
                }
            }
        }
    }

    /// Update the current process.
    pub fn set_process(&mut self, process: Arc<ProcessHandle>) {
        self.runtime.process = Some(process);
        self.parse_filter_expr();
        self.parse_value_expr();
    }

    /// Disable all controls.
    pub fn disable(&mut self) {
        if let Some(button) = self.widgets.scan_button.upgrade() {
            button.set_sensitive(false);
        }

        if let Some(button) = self.widgets.reset_button.upgrade() {
            button.set_sensitive(false);
        }

        if let Some(button) = self.widgets.refresh_button.upgrade() {
            button.set_sensitive(false);
        }
    }

    /// Enable controls.
    pub fn enable(&mut self) {
        if let Some(button) = self.widgets.scan_button.upgrade() {
            button.set_sensitive(true);
        }

        if let Some(button) = self.widgets.reset_button.upgrade() {
            button.set_sensitive(true);
        }

        if let Some(button) = self.widgets.refresh_button.upgrade() {
            button.set_sensitive(true);
        }
    }
}
