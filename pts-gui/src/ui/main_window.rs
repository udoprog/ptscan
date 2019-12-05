use crate::prelude::*;
use parking_lot::{RwLock, RwLockUpgradableReadGuard, RwLockWriteGuard};
use std::{cell::RefCell, rc::Rc, sync::Arc, time::Instant};

use anyhow::{anyhow, Context as _};
use chrono::Utc;
use ptscan::{Addresses, InitialScanConfig, PointerInfo as _, ProcessId, TypeHint, Values};

// 100 MB undo threshold
const UNDO_THRESHOLD_BYTES: usize = 100_000_000;

struct Widgets {
    #[allow(unused)]
    error_dialog_window: Window,
    #[allow(unused)]
    connect_dialog_window: Window,
    #[allow(unused)]
    edit_scan_result_dialog_window: Window,
    #[allow(unused)]
    show_scan_result_dialog_window: Window,
    #[allow(unused)]
    process_information_window: Window,
    attached_label: glib::WeakRef<Label>,
    scan_progress: glib::WeakRef<ProgressBar>,
    scan_progress_container: glib::WeakRef<gtk::Box>,
    status_label: glib::WeakRef<Label>,
    attached_image: glib::WeakRef<Image>,
    detached_image: glib::WeakRef<Image>,
    scan_status: glib::WeakRef<Label>,
}

pub struct MainWindow {
    thread_pool: Arc<rayon::ThreadPool>,
    session: Rc<RefCell<scan::Session>>,
    widgets: Widgets,
    handle: Option<Arc<RwLock<ptscan::ProcessHandle>>>,
    open_process_task: Option<task::Handle>,
    filter_options: Rc<RefCell<ui::FilterOptions>>,
    current_major_task: Option<task::Handle>,
    error_dialog: Rc<RefCell<ui::ErrorDialog>>,
    ui_scan: Rc<RefCell<ui::Scan>>,
    scratch_results: Rc<RefCell<ui::Scratch>>,
    edit_scan_result_dialog: Rc<RefCell<ui::EditScanResultDialog>>,
    show_scan_result_dialog: Rc<RefCell<ui::ShowScanResultDialog>>,
    process_information: Rc<RefCell<ui::ProcessInformation>>,
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

        let clipboard = Rc::new(Clipboard::new());

        let accel_group = builder.get_object::<AccelGroup>("accel_group");

        let window = cascade! {
            builder.get_object::<ApplicationWindow>("window");
        };

        let attached_label = builder.get_object::<Label>("attached_label");
        let attached_image = builder.get_object::<Image>("attached_image");
        let detached_image = builder.get_object::<Image>("detached_image");
        let scan_progress = builder.get_object::<ProgressBar>("scan_progress");
        let scan_cancel = builder.get_object::<Button>("scan_cancel");
        let scan_progress_container = builder.get_object::<gtk::Box>("scan_progress_container");
        let scan_status = builder.get_object::<Label>("scan_status");

        let status_label = cascade! {
            Label::new(Some("Welcome!"));
            ..show();
        };

        cascade! {
            builder.get_object::<Statusbar>("status");
            ..add(&status_label);
        };

        let session = Rc::new(RefCell::new(scan::Session::new()));

        let (error_dialog, error_dialog_window) = ui::ErrorDialog::new();
        let (connect_dialog, connect_dialog_window) = ui::ConnectDialog::new(error);

        let (show_scan_result_dialog, show_scan_result_dialog_window) =
            ui::ShowScanResultDialog::new(settings.clone());

        let (process_information, process_information_window) =
            ui::ProcessInformation::new(&accel_group, clipboard.clone());

        let ui_scan = ui::Scan::new(
            &builder,
            clipboard.clone(),
            settings.clone(),
            session.clone(),
            thread_pool.clone(),
            show_scan_result_dialog.clone(),
            show_scan_result_dialog_window.downgrade(),
        );

        let (edit_scan_result_dialog, edit_scan_result_dialog_window) =
            ui::EditScanResultDialog::new(settings.clone());

        let scratch_results = ui::Scratch::new(
            &builder,
            clipboard.clone(),
            thread_pool.clone(),
            edit_scan_result_dialog.clone(),
            edit_scan_result_dialog_window.downgrade(),
        );

        let filter_options = ui::FilterOptions::new(clipboard.clone(), &builder);

        let main_menu = ui::MainMenu::new(
            &builder,
            window.downgrade(),
            clipboard.clone(),
            connect_dialog_window.downgrade(),
            error_dialog_window.downgrade(),
            process_information_window.downgrade(),
        );

        let slf = Rc::new(RefCell::new(MainWindow {
            thread_pool,
            session,
            widgets: Widgets {
                error_dialog_window,
                connect_dialog_window,
                edit_scan_result_dialog_window,
                show_scan_result_dialog_window,
                process_information_window,
                attached_label: attached_label.downgrade(),
                scan_progress: scan_progress.downgrade(),
                scan_progress_container: scan_progress_container.downgrade(),
                status_label: status_label.downgrade(),
                attached_image: attached_image.downgrade(),
                detached_image: detached_image.downgrade(),
                scan_status: scan_status.downgrade(),
            },
            handle: None,
            open_process_task: None,
            filter_options: filter_options.clone(),
            current_major_task: None,
            error_dialog,
            ui_scan: ui_scan.clone(),
            scratch_results: scratch_results.clone(),
            edit_scan_result_dialog,
            show_scan_result_dialog,
            process_information,
            main_menu: main_menu.clone(),
        }));

        main_menu.borrow_mut().on_detach(clone!(slf => move || {
            slf.borrow_mut().detach();
        }));

        main_menu.borrow_mut().on_undo(clone!(slf => move || {
            slf.borrow_mut().undo();
        }));

        main_menu.borrow_mut().on_redo(clone!(slf => move || {
            slf.borrow_mut().redo();
        }));

        ui_scan
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

        attached_label.connect_activate_link(clone!(slf => move |_, link| {
            slf.borrow_mut().handle_link(link)
        }));

        status_label.connect_activate_link(clone!(slf => move |_, link| {
            slf.borrow_mut().handle_link(link)
        }));

        scan_cancel.connect_clicked(clone!(slf => move |_| {
            slf.borrow_mut().cancel_major_task();
        }));

        cascade! {
            filter_options.borrow_mut();
            ..on_scan(clone!(slf => move || {
                Self::scan(&slf);
            }));
            ..on_refresh(clone!(slf => move || {
                Self::refresh(&slf);
            }));
            ..on_reset(clone!(slf => move || {
                Self::reset(&slf);
            }));
            ..on_filter_expr_changed(clone!(slf => move |value_expr| {
                slf.borrow().ui_scan.borrow_mut().set_filter_expr(value_expr);
            }));
            ..on_value_expr_changed(clone!(slf => move |value_expr| {
                slf.borrow().ui_scan.borrow_mut().set_value_expr(value_expr);
            }));
        };

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

        let all_scans = slf.session.borrow().all();

        let task = cascade! {
            task::Task::oneshot(main, move |_| {
                for scan in all_scans {
                    scan.write().clear();
                }

                Ok(())
            });
            ..then(|slf, _| {
                slf.session.borrow_mut().clear();
                slf.end_major_task();
                slf.update_components();
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

        let last_scan = slf.session.borrow().last().cloned();
        let thread_pool = slf.thread_pool.clone();
        let started = Instant::now();

        let (value_expr, filter_expr, config, suspend) = {
            let filter_options = slf.filter_options.borrow();
            let value_expr = filter_options.state.value_expr.clone();
            let filter_expr = optional!(&filter_options.state.filter_expr).clone();
            let mut config = InitialScanConfig::default();
            config.modules_only = filter_options.state.modules_only;
            config.alignment = filter_options.state.alignment;
            let suspend = filter_options.state.suspend;
            (value_expr, filter_expr, config, suspend)
        };

        let mut task = task::Task::new(main, move |s| {
            if let Some(last_scan) = last_scan {
                let mut write_lock;
                let mut new_scan = None;

                let scan = if last_scan.read().bytes() < UNDO_THRESHOLD_BYTES {
                    let mut new = (*last_scan.read()).clone();
                    new.id = uuid::Uuid::new_v4();
                    new_scan.get_or_insert(new)
                } else {
                    write_lock = last_scan.write();
                    &mut *write_lock
                };

                let handle = handle.read();

                if suspend {
                    handle
                        .process
                        .suspend()
                        .context("failed to suspend process")?;
                }

                let result = handle.rescan_values(
                    &*thread_pool,
                    &mut scan.addresses,
                    &mut scan.initial,
                    &mut scan.last,
                    Some(s.as_token()),
                    ContextProgress::new(s),
                    &filter_expr,
                );

                if suspend {
                    handle
                        .process
                        .resume()
                        .context("failed to resume process")?;
                }

                result?;
                return Ok((new_scan, s.is_stopped()));
            }

            let handle = RwLockWriteGuard::downgrade_to_upgradable(handle.write());

            let value_type = value_expr
                .value_type_of(&*handle, TypeHint::NoHint)?
                .unwrap_or(filter_expr.value_type_of(&*handle, TypeHint::NoHint)?)
                .ok_or_else(|| anyhow!("cannot determine type of value"))?;

            let modules = handle
                .get_modules()
                .context("error when refreshing modules")?;

            let threads = handle
                .get_threads_with_modules(&modules)
                .context("error when refreshing threads")?;

            let mut handle = RwLockUpgradableReadGuard::upgrade(handle);
            handle.update_modules(modules);
            handle.update_threads(threads);
            let handle = RwLockWriteGuard::downgrade(handle);

            let mut addresses = Addresses::new(handle.width());
            let mut values = Values::new(value_type);

            if suspend {
                handle
                    .process
                    .suspend()
                    .context("failed to suspend process")?;
            }

            let result = handle.initial_scan(
                &*thread_pool,
                &filter_expr,
                &mut addresses,
                &mut values,
                Some(s.as_token()),
                ContextProgress::new(s),
                config,
            );

            if suspend {
                handle
                    .process
                    .resume()
                    .context("failed to resume process")?;
            }

            let new_scan = scan::Scan {
                id: uuid::Uuid::new_v4(),
                addresses,
                initial: values.clone(),
                last: values,
            };

            result?;
            Ok((Some(new_scan), s.is_stopped()))
        });

        task.emit(|main, (percentage, results)| {
            if let Some(progress) = main.widgets.scan_progress.upgrade() {
                let fraction = (percentage as f64) / 100f64;
                progress.set_fraction(fraction);
                progress.set_text(Some(&format!("Found {} result(s)", results)));
            }
        });

        task.then(move |main, result| {
            if let Some(status_label) = main.widgets.status_label.upgrade() {
                let diff = Instant::now().duration_since(started);

                let (new_scan, text, error) = match result {
                    Err(e) => (
                        None,
                        format!(
                            "Scan failed after {:.3?} (<a href=\"#last-error\">error details</a>)",
                            diff
                        ),
                        Some(e),
                    ),
                    Ok((new_scan, true)) => (
                        new_scan,
                        format!(
                            "Scan cancelled after {:.3?} (<a href=\"#last-error\">error details</a>)",
                            diff
                        ),
                        Some(anyhow!("task was cancelled")),
                    ),
                    Ok((new_scan, false)) => (new_scan, format!("Scan completed in {:.3?}", diff), None),
                };

                status_label.set_markup(&text);

                if let Some(new_scan) = new_scan {
                    main.session.borrow_mut().push(new_scan);
                }

                if let Some(error) = error {
                    main.error_dialog.borrow_mut().add_error(Utc::now(), error);
                }
            }

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

        let scan = slf.session.borrow().last().cloned();
        let thread_pool = slf.thread_pool.clone();
        let started = Instant::now();

        let (filter_expr, value_expr) = {
            let filter_options = slf.filter_options.borrow();
            let filter_expr = filter_options.state.filter_expr.clone();
            let value_expr = filter_options.state.value_expr.clone();
            (filter_expr, value_expr)
        };

        let task = cascade! {
            task::Task::new(main, move |s| {
                let handle = handle.read();

                let value_type = match filter_expr {
                    Some(filter_expr) => filter_expr.value_type_of(&*handle, TypeHint::NoHint)?,
                    None => TypeHint::NoHint,
                };

                let value_type = value_expr.value_type_of(&*handle, TypeHint::NoHint)?.unwrap_or(value_type);

                let scan = optional!(scan, Ok(None));
                let scan = &mut *scan.write();

                let value_type = value_type.option().unwrap_or(scan.last.ty()).unsize(scan.last.ty());

                if value_type != scan.last.ty() {
                    scan.last.convert_in_place(&*handle, value_type);
                }

                handle.refresh_values(
                    &*thread_pool,
                    &scan.addresses,
                    &mut scan.last,
                    Some(s.as_token()),
                    ContextProgress::new(s),
                    &value_expr,
                )?;

                Ok(Some(s.is_stopped()))
            });
            ..emit(|slf, (percentage, results)| {
                if let Some(progress) = slf.widgets.scan_progress.upgrade() {
                    let fraction = (percentage as f64) / 100f64;
                    progress.set_fraction(fraction);
                    progress.set_text(Some(&format!("Refreshed {} result(s)", results)));
                }
            });
            ..then(move |slf, cancelled| {
                if let Some(status_label) = slf.widgets.status_label.upgrade() {
                    let diff = Instant::now().duration_since(started);

                    let (text, error) = match cancelled {
                        Err(e) => (
                            format!(
                                "Refresh failed after {:.3?} (<a href=\"#last-error\">error details</a>)",
                                diff
                            ),
                            Some(e),
                        ),
                        Ok(Some(true)) => (
                            format!(
                                "Refresh cancelled after {:.3?} (<a href=\"#last-error\">error details</a>)",
                                diff
                            ),
                            Some(anyhow!("task was cancelled")),
                        ),
                        Ok(Some(false)) => (format!("Refresh completed in {:.3?}", diff), None),
                        Ok(None) => (String::from("Scan could not be performed (missing values)"), None),
                    };

                    status_label.set_markup(&text);

                    if let Some(error) = error {
                        slf.error_dialog.borrow_mut().add_error(Utc::now(), error);
                    }
                }

                slf.end_major_task();
                slf.update_components();
            });
        };

        slf.start_major_task(task.run());
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
                            label.set_text(&format!("Attached to: {}", handle.name.to_string_lossy()));
                        }

                        main.handle = Some(Arc::new(RwLock::new(handle)));
                        main.update_components();
                    }
                    Err(e) => {
                        if let Some(label) = main.widgets.attached_label.upgrade() {
                            label.set_markup(&format!("Failed to attach: {} (<a href=\"#last-error\">error details</a>)", e));
                            main.error_dialog.borrow_mut().add_error(Utc::now(), e);
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

    /// An undo action was triggered.
    fn undo(&mut self) {
        self.session.borrow_mut().undo();
        self.update_components();
    }

    /// A redo action was triggered
    fn redo(&mut self) {
        self.session.borrow_mut().redo();
        self.update_components();
    }

    /// Updates all relevant components.
    fn update_components(&mut self) {
        let session = self.session.borrow();
        let label = upgrade!(self.widgets.attached_label);
        let attached = upgrade!(self.widgets.attached_image);
        let detached = upgrade!(self.widgets.detached_image);

        let scan_status = upgrade!(self.widgets.scan_status);

        if let Some(scan) = session.last() {
            match scan.try_read() {
                Some(scan) => {
                    let size = (scan.bytes() as f64) / 1_000_000f64;
                    scan_status.set_text(&format!(
                        "Scan ({}) with {} result(s) (size: {:.2}M)",
                        scan.id,
                        scan.len(),
                        size
                    ));
                }
                None => {
                    scan_status.set_text("Failed to lock scan");
                }
            }
        } else {
            scan_status.set_text("No scan in progress");
        }

        if let Some(handle) = self.handle.as_ref().and_then(|h| h.try_read()) {
            label.set_text(&format!(
                "Attached to {} ({})",
                handle.name.to_string_lossy(),
                handle.process.process_id
            ));
        } else {
            label.set_text("Not Attached");
        }

        attached.set_visible(self.handle.is_some());
        detached.set_visible(self.handle.is_none());

        {
            let mut ui_scan = self.ui_scan.borrow_mut();
            ui_scan.set_handle(self.handle.clone());
            ui_scan.refresh();
        }

        {
            let session = self.session.borrow();
            self.filter_options
                .borrow_mut()
                .has_results(!session.is_empty());
        }

        self.scratch_results
            .borrow_mut()
            .set_handle(self.handle.clone());
        self.edit_scan_result_dialog
            .borrow_mut()
            .set_handle(self.handle.clone());
        self.show_scan_result_dialog
            .borrow_mut()
            .set_handle(self.handle.clone());

        {
            let session = self.session.borrow();

            cascade! {
                self.main_menu.borrow_mut();
                ..set_attached(self.handle.is_some());
                ..set_can_undo(session.can_undo());
                ..set_can_redo(session.can_redo());
            };
        }

        let filter_options = &self.filter_options;
        let process_information = &self.process_information;
        let handle = &self.handle;

        gtk::idle_add(
            clone!(filter_options, process_information, handle => move || {
                ui::FilterOptions::set_handle(&filter_options, handle.clone());
                ui::ProcessInformation::set_handle(&process_information, handle.clone());
                Continue(false)
            }),
        );
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
