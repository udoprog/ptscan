use crate::prelude::*;
use parking_lot::RwLock;
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
    /// Only scan in modules.
    pub modules_only: bool,
}

struct Widgets {
    filter_expr_text: glib::WeakRef<Entry>,
    filter_expr_error: glib::WeakRef<Image>,
    value_expr_text: glib::WeakRef<Entry>,
    value_expr_error: glib::WeakRef<Image>,
    reset_button: glib::WeakRef<Button>,
    scan_button: glib::WeakRef<Button>,
    refresh_button: glib::WeakRef<Button>,
    #[allow(unused)]
    modules_only: glib::WeakRef<CheckButton>,
}
pub struct FilterOptions {
    pub state: State,
    widgets: Widgets,
    on_scan: Option<Box<dyn Fn()>>,
    on_reset: Option<Box<dyn Fn()>>,
    on_refresh: Option<Box<dyn Fn()>>,
    handle: Option<Arc<RwLock<ProcessHandle>>>,
    enabled: bool,
    has_results: bool,
}

impl FilterOptions {
    /// Construct a widget representing the current state.
    pub fn new(frame: &gtk::Box) -> Rc<RefCell<Self>> {
        let builder = resource("filter_options.glade").into_builder();

        // TODO: make serializable.
        let state = State {
            filter_expr: None,
            filter_expr_text: String::from(""),
            value_expr: ValueExpr::Value,
            value_expr_text: String::from("value"),
            modules_only: false,
        };

        let filter_expr_text = cascade! {
            builder.get_object::<Entry>("filter_expr_text");
            ..set_text(&state.filter_expr_text);
        };

        let scan_button = cascade! {
            builder.get_object::<Button>("scan_button");
        };

        let reset_button = cascade! {
            builder.get_object::<Button>("reset_button");
        };

        let value_expr_text = cascade! {
            builder.get_object::<Entry>("value_expr_text");
            ..set_text(&state.value_expr_text);
        };

        let modules_only = cascade! {
            builder.get_object::<CheckButton>("modules_only");
            ..set_active(state.modules_only);
        };

        let refresh_button = cascade! {
            builder.get_object::<Button>("refresh_button");
        };

        let filter_expr_error = builder.get_object::<Image>("filter_expr_error");
        let value_expr_error = builder.get_object::<Image>("value_expr_error");

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
                modules_only: modules_only.downgrade(),
            },
            on_reset: None,
            on_refresh: None,
            on_scan: None,
            handle: None,
            enabled: true,
            has_results: false,
        }));

        scan_button.connect_clicked(clone!(slf => move |_| {
            let on_scan = optional!(slf.borrow_mut().on_scan.take());
            on_scan();
            slf.borrow_mut().on_scan = Some(on_scan);
        }));

        reset_button.connect_clicked(clone!(slf => move |_| {
            let on_reset = optional!(slf.borrow_mut().on_reset.take());
            on_reset();
            slf.borrow_mut().on_reset = Some(on_reset);
        }));

        refresh_button.connect_clicked(clone!(slf => move |_| {
            let on_refresh = optional!(slf.borrow_mut().on_refresh.take());
            on_refresh();
            slf.borrow_mut().on_refresh = Some(on_refresh);
        }));

        filter_expr_text.connect_activate(clone!(slf => move |_| {
            let on_scan = optional!(slf.borrow_mut().on_scan.take());
            on_scan();
            slf.borrow_mut().on_scan = Some(on_scan);
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

        modules_only.connect_toggled(clone!(slf => move |btn| {
            let mut slf = slf.borrow_mut();
            slf.state.modules_only = btn.get_active();
        }));

        let root = builder.get_object::<gtk::Box>("root");

        root.foreach(|child| {
            root.remove(child);
            frame.add(child);
        });

        slf.borrow_mut().update_components();
        slf
    }

    /// Handler to fire when scan is clicked.
    pub fn on_scan(&mut self, on_scan: (impl Fn() + 'static)) {
        self.on_scan = Some(Box::new(on_scan));
    }

    /// Handler to fire when reset is clicked.
    pub fn on_reset(&mut self, on_reset: (impl Fn() + 'static)) {
        self.on_reset = Some(Box::new(on_reset));
    }

    /// Handler to fire when refresh is clicked.
    pub fn on_refresh(&mut self, on_refresh: (impl Fn() + 'static)) {
        self.on_refresh = Some(Box::new(on_refresh));
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
        let handle = optional!(&self.handle);
        let handle = optional!(handle.try_read());

        if self.state.filter_expr_text.is_empty() {
            if let Some(error) = self.widgets.filter_expr_error.upgrade() {
                error.hide();
            }

            return;
        }

        match FilterExpr::parse(&self.state.filter_expr_text, &handle.process) {
            Ok(filter_expr) => {
                self.state.filter_expr = Some(filter_expr);

                if let Some(error) = self.widgets.filter_expr_error.upgrade() {
                    error.hide();
                }
            }
            Err(..) => {
                if let Some(error) = self.widgets.filter_expr_error.upgrade() {
                    error.show();
                }
            }
        }
    }

    /// Parse the current global value expression.
    pub fn parse_value_expr(&mut self) {
        let handle = optional!(&self.handle);
        let handle = optional!(handle.try_read());

        if self.state.value_expr_text.is_empty() {
            if let Some(error) = self.widgets.value_expr_error.upgrade() {
                error.hide();
            }
        }

        match ValueExpr::parse(&self.state.value_expr_text, &handle.process) {
            Ok(value_expr) => {
                self.state.value_expr = value_expr;

                if let Some(error) = self.widgets.value_expr_error.upgrade() {
                    error.hide();
                }
            }
            Err(..) => {
                if let Some(error) = self.widgets.value_expr_error.upgrade() {
                    error.show();
                }
            }
        }
    }

    /// Update the current process.
    pub fn set_handle(&mut self, handle: Option<Arc<RwLock<ProcessHandle>>>) {
        self.handle = handle;
        self.parse_filter_expr();
        self.parse_value_expr();
        self.update_components();
    }

    /// Indicate if there are results, so that the appropriate buttons and
    /// options can be enabled.
    pub fn has_results(&mut self, has_results: bool) {
        self.has_results = has_results;
        self.update_components();
    }

    /// Disable all controls.
    pub fn disable(&mut self) {
        self.enabled = false;
        self.update_components();
    }

    /// Enable controls.
    pub fn enable(&mut self) {
        self.enabled = true;
        self.update_components();
    }

    fn update_components(&mut self) {
        if let Some(button) = self.widgets.scan_button.upgrade() {
            button.set_sensitive(self.handle.is_some() && self.enabled);
        }

        if let Some(button) = self.widgets.reset_button.upgrade() {
            button.set_sensitive(self.handle.is_some() && self.enabled && self.has_results);
        }

        if let Some(button) = self.widgets.refresh_button.upgrade() {
            button.set_sensitive(self.handle.is_some() && self.enabled && self.has_results);
        }
    }
}
