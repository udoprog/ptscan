use crate::prelude::*;
use parking_lot::RwLock;
use std::{cell::RefCell, rc::Rc, sync::Arc, time::Instant};

use anyhow::{anyhow, Context as _};
use chrono::Utc;
use ptscan::{InitialScanConfig, ProcessId, Scan, TypeHint};

struct Widgets {
    #[allow(unused)]
    error_dialog_window: Window,
    #[allow(unused)]
    connect_dialog_window: Window,
    #[allow(unused)]
    edit_scan_result_dialog_window: Window,
    #[allow(unused)]
    show_scan_result_dialog_window: Window,
    attached_label: glib::WeakRef<Label>,
    scan_progress: glib::WeakRef<ProgressBar>,
    scan_progress_container: glib::WeakRef<gtk::Box>,
    status_label: glib::WeakRef<Label>,
    attached_image: glib::WeakRef<Image>,
    detached_image: glib::WeakRef<Image>,
    scan_results_status: glib::WeakRef<Label>,
}

pub struct MainWindow {
    thread_pool: Arc<rayon::ThreadPool>,
    scan: Arc<RwLock<Scan>>,
    widgets: Widgets,
    handle: Option<Arc<RwLock<ptscan::ProcessHandle>>>,
    open_process_task: Option<task::Handle>,
    filter_options: Rc<RefCell<ui::FilterOptions>>,
    current_major_task: Option<task::Handle>,
    error_dialog: Rc<RefCell<ui::ErrorDialog>>,
    scan_results: Rc<RefCell<ui::ScanResults>>,
    scratch_results: Rc<RefCell<ui::ScratchResults>>,
    edit_scan_result_dialog: Rc<RefCell<ui::EditScanResultDialog>>,
    show_scan_result_dialog: Rc<RefCell<ui::ShowScanResultDialog>>,
    main_menu: Rc<RefCell<ui::MainMenu>>,
}

impl MainWindow {
    /// Construct a new main window and assocaite it with the given application.
    pub fn build(
        settings: Arc<Settings>,
        thread_pool: Arc<rayon::ThreadPool>,
        application: &gtk::Application,
        error: impl ErrorHandler,
    ) {
        let builder = resource("main_window.glade").into_builder();

        let accel_group = AccelGroup::new();

        let window = cascade! {
            builder.get_object::<ApplicationWindow>("window");
            ..add_accel_group(&accel_group);
        };

        let attached_label = builder.get_object::<Label>("attached_label");
        let attached_image = builder.get_object::<Image>("attached_image");
        let detached_image = builder.get_object::<Image>("detached_image");
        let scan_progress = builder.get_object::<ProgressBar>("scan_progress");
        let scan_cancel = builder.get_object::<Button>("scan_cancel");
        let scan_progress_container = builder.get_object::<gtk::Box>("scan_progress_container");
        let scan_results_tree = builder.get_object::<TreeView>("scan_results_tree");
        let filter_options_container = builder.get_object::<gtk::Box>("filter_options_container");
        let scan_results_status = builder.get_object::<Label>("scan_results_status");

        let status_label = cascade! {
            Label::new(Some("Welcome!"));
            ..show();
        };

        cascade! {
            builder.get_object::<Statusbar>("status");
            ..add(&status_label);
        };

        let scan = Arc::new(RwLock::new(Scan::new()));

        let (error_dialog, error_dialog_window) = ui::ErrorDialog::new();
        let (connect_dialog, connect_dialog_window) = ui::ConnectDialog::new(error);

        let (show_scan_result_dialog, show_scan_result_dialog_window) =
            ui::ShowScanResultDialog::new(settings.clone());

        let scan_results = ui::ScanResults::new(
            settings.clone(),
            &scan_results_tree,
            scan.clone(),
            thread_pool.clone(),
            show_scan_result_dialog.clone(),
            show_scan_result_dialog_window.downgrade(),
        );

        let (edit_scan_result_dialog, edit_scan_result_dialog_window) =
            ui::EditScanResultDialog::new(settings.clone());

        let scratch_results = ui::ScratchResults::new(
            &builder,
            thread_pool.clone(),
            edit_scan_result_dialog.clone(),
            edit_scan_result_dialog_window.downgrade(),
        );

        let filter_options = ui::FilterOptions::new(&filter_options_container);

        let main_menu = ui::MainMenu::new(
            &builder,
            window.downgrade(),
            &accel_group,
            connect_dialog_window.downgrade(),
            error_dialog_window.downgrade(),
        );

        let slf = Rc::new(RefCell::new(MainWindow {
            thread_pool,
            scan,
            widgets: Widgets {
                error_dialog_window,
                connect_dialog_window,
                edit_scan_result_dialog_window,
                show_scan_result_dialog_window,
                attached_label: attached_label.downgrade(),
                scan_progress: scan_progress.downgrade(),
                scan_progress_container: scan_progress_container.downgrade(),
                status_label: status_label.downgrade(),
                attached_image: attached_image.downgrade(),
                detached_image: detached_image.downgrade(),
                scan_results_status: scan_results_status.downgrade(),
            },
            handle: None,
            open_process_task: None,
            filter_options: filter_options.clone(),
            current_major_task: None,
            error_dialog,
            scan_results: scan_results.clone(),
            scratch_results: scratch_results.clone(),
            edit_scan_result_dialog,
            show_scan_result_dialog,
            main_menu: main_menu.clone(),
        }));

        main_menu.borrow_mut().on_detach(clone!(slf => move || {
            slf.borrow_mut().detach();
        }));

        scan_results
            .borrow_mut()
            .on_add_results(clone!(slf => move |_, result| {
                let slf = slf.borrow();
                slf.scratch_results.borrow_mut().add_result(result);
            }));

        connect_dialog
            .borrow_mut()
            .on_attach(clone!(slf => move |_, process_id| {
                Self::attach_process_id(&slf, process_id);
            }));

        status_label.connect_activate_link(clone!(slf => move |_, link| {
            slf.borrow_mut().handle_link(link)
        }));

        scan_cancel.connect_clicked(clone!(slf => move |_| {
            slf.borrow_mut().cancel_major_task();
        }));

        filter_options.borrow_mut().on_scan(clone!(slf => move || {
            Self::scan(&slf);
        }));

        filter_options
            .borrow_mut()
            .on_refresh(clone!(slf => move || {
                Self::refresh(&slf);
            }));

        filter_options.borrow_mut().on_reset(clone!(slf => move || {
            Self::reset(&slf);
        }));

        slf.borrow_mut().update_components();

        window.set_application(Some(application));
        window.show();
    }

    /// Handle when a link is clicked.
    pub fn handle_link(&mut self, link: &str) -> Inhibit {
        match link {
            "#last-error" => {
                self.widgets.error_dialog_window.show();
                self.widgets.error_dialog_window.present();
            }
            _ => return Inhibit(false),
        }

        Inhibit(true)
    }

    /// Cancel the current major task.
    pub fn cancel_major_task(&mut self) {
        self.current_major_task.take();
    }

    /// Reset the current scan.
    pub fn reset(main: &Rc<RefCell<Self>>) {
        let mut slf = main.borrow_mut();

        if slf.current_major_task.is_some() {
            return;
        }

        let scan = slf.scan.clone();

        let task = cascade! {
            task::Task::oneshot(main, move |_| {
                scan.write().clear();
                Ok(())
            });
            ..then(|main, _| {
                main.end_major_task();
                main.scan_results.borrow_mut().refresh();
                main.filter_options.borrow_mut().has_results(false);
            });
        };

        slf.start_major_task(task.run());
    }

    /// Perform a scan.
    pub fn scan(main: &Rc<RefCell<Self>>) {
        let mut slf = main.borrow_mut();

        if slf.current_major_task.is_some() {
            return;
        }

        let handle = optional!(&slf.handle).clone();

        let scan = slf.scan.clone();
        let thread_pool = slf.thread_pool.clone();
        let started = Instant::now();

        let mut task = if scan.read().initial {
            let filter_options = slf.filter_options.borrow();
            let value_expr = filter_options.state.value_expr.clone();
            let filter_expr = optional!(&filter_options.state.filter_expr).clone();
            let mut config = InitialScanConfig::default();
            config.modules_only = filter_options.state.modules_only;

            task::Task::new(main, move |s| {
                let value_type = value_expr
                    .value_type_of(TypeHint::NoHint)?
                    .unwrap_or(filter_expr.value_type_of(TypeHint::NoHint)?)
                    .ok_or_else(|| anyhow!("cannot determine type of value"))?;

                {
                    let mut handle = handle.write();
                    handle
                        .refresh_threads()
                        .context("error when refreshing threads")?;
                    handle
                        .refresh_modules()
                        .context("error when refreshing modules")?;
                }

                let handle = handle.read();
                let mut scan = scan.write();

                let result = scan.initial_scan(
                    &*thread_pool,
                    &*handle,
                    &filter_expr,
                    value_type,
                    Some(s.as_token()),
                    ContextProgress::new(s),
                    config,
                );

                scan.initial = false;
                result?;
                Ok(s.is_stopped())
            })
        } else {
            let filter_options = slf.filter_options.borrow();
            let value_expr = filter_options.state.value_expr.clone();
            let filter_expr = optional!(&filter_options.state.filter_expr).clone();

            task::Task::new(main, move |s| {
                let value_type = value_expr
                    .value_type_of(TypeHint::NoHint)?
                    .unwrap_or(filter_expr.value_type_of(TypeHint::NoHint)?)
                    .option();

                let mut scan = scan.write();

                let handle = handle.read();

                scan.scan(
                    &*thread_pool,
                    &*handle,
                    value_type,
                    Some(s.as_token()),
                    ContextProgress::new(s),
                    &filter_expr,
                )?;

                Ok(s.is_stopped())
            })
        };

        task.emit(|main, (percentage, results)| {
            if let Some(progress) = main.widgets.scan_progress.upgrade() {
                let fraction = (percentage as f64) / 100f64;
                progress.set_fraction(fraction);
                progress.set_text(Some(&format!("Found {} result(s)", results)));
            }
        });

        task.then(move |main, cancelled| {
            if let Some(status_label) = main.widgets.status_label.upgrade() {
                let diff = Instant::now().duration_since(started);

                let (text, error) = match cancelled {
                    Err(e) => (
                        format!(
                            "Scan failed after {:.3?} (<a href=\"#last-error\">show error</a>)",
                            diff
                        ),
                        Some(e),
                    ),
                    Ok(true) => (
                        format!(
                            "Scan cancelled after {:.3?} (<a href=\"#last-error\">show error</a>)",
                            diff
                        ),
                        Some(anyhow!("task was cancelled")),
                    ),
                    Ok(false) => (format!("Scan completed in {:.3?}", diff), None),
                };

                status_label.set_markup(&text);

                if let Some(error) = error {
                    main.error_dialog.borrow_mut().add_error(Utc::now(), error);
                }
            }

            main.scan_results.borrow_mut().refresh();
            main.filter_options.borrow_mut().has_results(true);
            main.end_major_task();
            main.update_components();
        });

        slf.start_major_task(task.run());
    }

    /// Perform a refresh.
    pub fn refresh(main: &Rc<RefCell<Self>>) {
        let mut slf = main.borrow_mut();

        if slf.current_major_task.is_some() {
            return;
        }

        let handle = optional!(&slf.handle).clone();

        let scan = slf.scan.clone();
        let thread_pool = slf.thread_pool.clone();
        let started = Instant::now();

        let (mut task, value_expr) = {
            let filter_options = slf.filter_options.borrow();
            let value_expr = filter_options.state.value_expr.clone();

            let value_expr2 = value_expr.clone();

            let task = task::Task::new(main, move |s| {
                let value_type = value_expr.value_type_of(TypeHint::NoHint)?.option();
                let handle = handle.read();
                let mut scan = scan.write();

                handle.refresh_values(
                    &*thread_pool,
                    &mut scan.results,
                    Some(s.as_token()),
                    value_type,
                    ContextProgress::new(s),
                    &value_expr,
                )?;

                Ok(s.is_stopped())
            });

            (task, value_expr2)
        };

        task.emit(|slf, (percentage, results)| {
            if let Some(progress) = slf.widgets.scan_progress.upgrade() {
                let fraction = (percentage as f64) / 100f64;
                progress.set_fraction(fraction);
                progress.set_text(Some(&format!("Refreshed {} result(s)", results)));
            }
        });

        task.then(move |slf, cancelled| {
            if let Some(status_label) = slf.widgets.status_label.upgrade() {
                let diff = Instant::now().duration_since(started);

                let (text, error) = match cancelled {
                    Err(e) => (
                        format!(
                            "Refresh failed after {:.3?} (<a href=\"#last-error\">show error</a>)",
                            diff
                        ),
                        Some(e),
                    ),
                    Ok(true) => (
                        format!(
                            "Refresh cancelled after {:.3?} (<a href=\"#last-error\">show error</a>)",
                            diff
                        ),
                        Some(anyhow!("task was cancelled")),
                    ),
                    Ok(false) => (format!("Refresh completed in {:.3?}", diff), None),
                };

                status_label.set_markup(&text);

                if let Some(error) = error {
                    slf.error_dialog.borrow_mut().add_error(Utc::now(), error);
                }
            }

            slf.scan_results.borrow_mut().refresh();
            slf.filter_options.borrow_mut().has_results(true);
            slf.end_major_task();
            slf.update_components();
        });

        slf.start_major_task(task.run());

        slf.scan_results.borrow_mut().set_value_expr(value_expr);
    }

    /// Start the current major task.
    fn start_major_task(&mut self, handle: task::Handle) {
        if let (Some(progress), Some(container)) = (
            self.widgets.scan_progress.upgrade(),
            self.widgets.scan_progress_container.upgrade(),
        ) {
            progress.set_fraction(1f64);
            progress.set_text(None);
            container.show();
        }

        self.filter_options.borrow_mut().disable();
        self.current_major_task = Some(handle);
    }

    /// End the current major task.
    fn end_major_task(&mut self) {
        if let Some(container) = self.widgets.scan_progress_container.upgrade() {
            container.hide();
        }

        self.filter_options.borrow_mut().enable();
        self.current_major_task = None;
    }

    /// Attach to the specified process id.
    pub fn attach_process_id(main: &Rc<RefCell<Self>>, process_id: ProcessId) {
        let mut slf = main.borrow_mut();

        let task = cascade! {
            task::Task::oneshot(main, move |_| {
                let handle = ptscan::ProcessHandle::open(process_id)?;
                let mut handle = handle.ok_or_else(|| anyhow!("failed to attach, no such process"))?;
                handle.refresh_threads().context("failed to refresh process threads")?;
                handle.refresh_modules().context("failed to refresh process modules")?;
                Ok(handle)
            });
            ..then(|main, handle| {
                main.open_process_task = None;

                match handle {
                    Ok(handle) => {
                        if let Some(label) = main.widgets.attached_label.upgrade() {
                            label.set_text(&format!("Attached to: {}", handle.name));
                        }

                        main.handle = Some(Arc::new(RwLock::new(handle)));
                        main.update_components();
                    }
                    Err(e) => {
                        if let Some(label) = main.widgets.attached_label.upgrade() {
                            label.set_text(&format!("Failed to attach: {}", e));
                        }
                    }
                }
            });
        };

        slf.open_process_task = Some(task.run());
    }

    /// Detach the current handle.
    fn detach(&mut self) {
        self.handle = None;
        self.update_components();
    }

    /// Updates all relevant components.
    fn update_components(&mut self) {
        let label = upgrade!(self.widgets.attached_label);
        let attached = upgrade!(self.widgets.attached_image);
        let detached = upgrade!(self.widgets.detached_image);

        let scan_results_status = upgrade!(self.widgets.scan_results_status);

        if let Some(scan) = self.scan.try_read() {
            if scan.initial {
                scan_results_status.set_text("No scan in progress");
            } else {
                scan_results_status
                    .set_text(&format!("Scan with {} result(s)", scan.results.len()));
            }
        } else {
            scan_results_status.set_text("Failed to lock scan");
        }

        if let Some(handle) = self.handle.as_ref().and_then(|h| h.try_read()) {
            label.set_text(&format!(
                "Attached to {} ({})",
                handle.name, handle.process.process_id
            ));
        } else {
            label.set_text("Not Attached");
        }

        attached.set_visible(self.handle.is_some());
        detached.set_visible(self.handle.is_none());

        self.filter_options
            .borrow_mut()
            .set_handle(self.handle.clone());
        self.scan_results
            .borrow_mut()
            .set_handle(self.handle.clone());
        self.scratch_results
            .borrow_mut()
            .set_handle(self.handle.clone());
        self.edit_scan_result_dialog
            .borrow_mut()
            .set_handle(self.handle.clone());
        self.show_scan_result_dialog
            .borrow_mut()
            .set_handle(self.handle.clone());
        self.main_menu
            .borrow_mut()
            .set_attached(self.handle.is_some());
    }
}

struct ContextProgress<'a, T> {
    context: &'a task::Context<(usize, u64), T>,
}

impl<'a, T> ContextProgress<'a, T> {
    pub fn new(context: &'a task::Context<(usize, u64), T>) -> Self {
        Self { context }
    }
}

impl<'a, T> ptscan::ScanProgress for ContextProgress<'a, T> {
    fn report_bytes(&mut self, _: ptscan::Size) -> anyhow::Result<()> {
        Ok(())
    }

    fn report(&mut self, percentage: usize, results: u64) -> anyhow::Result<()> {
        let _ = self.context.emit((percentage, results));
        Ok(())
    }
}