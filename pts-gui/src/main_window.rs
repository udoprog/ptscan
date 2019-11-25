use crate::{
    prelude::*, ConnectDialog, ErrorDialog, FilterOptions, MainMenu, ScanResultDialog, ScanResults,
    ScratchResults,
};
use parking_lot::RwLock;
use std::{cell::RefCell, rc::Rc, sync::Arc, time::Instant};

use self::Orientation::*;
use anyhow::anyhow;
use chrono::Utc;
use ptscan::{ProcessId, Scan, TypeHint};

struct Widgets {
    #[allow(unused)]
    error_dialog_window: Window,
    #[allow(unused)]
    connect_dialog_window: Window,
    #[allow(unused)]
    scan_result_dialog_window: Window,
    attached_label: glib::WeakRef<Label>,
    scan_progress: glib::WeakRef<ProgressBar>,
    scan_progress_container: glib::WeakRef<gtk::Box>,
    status_label: glib::WeakRef<Label>,
}

pub struct MainWindow {
    thread_pool: Arc<rayon::ThreadPool>,
    scan: Arc<RwLock<Scan>>,
    widgets: Widgets,
    attached: Option<Arc<ptscan::ProcessHandle>>,
    open_process_task: Option<task::Handle>,
    filter_options: Option<Rc<RefCell<FilterOptions>>>,
    current_major_task: Option<task::Handle>,
    error_dialog: Rc<RefCell<ErrorDialog>>,
    scan_results: Rc<RefCell<ScanResults>>,
    scratch_results: Rc<RefCell<ScratchResults>>,
    scan_result_dialog: Rc<RefCell<ScanResultDialog>>,
}

impl MainWindow {
    /// Construct a new main window and assocaite it with the given application.
    pub fn build(
        thread_pool: Arc<rayon::ThreadPool>,
        application: &gtk::Application,
        error: impl ErrorHandler,
    ) {
        let attached_label = cascade! {
            Label::new(Some("Not attached"));
            ..show();
        };

        let scan_progress = cascade! {
            ProgressBar::new();
            ..set_show_text(true);
            ..show();
        };

        let scan_cancel = cascade! {
            Button::new();
            ..set_label("Cancel");
            ..show();
        };

        let scan_progress_container = cascade! {
            gtk::Box::new(Horizontal, 10);
            ..pack_start(&scan_progress, true, true, 0);
            ..pack_start(&scan_cancel, true, true, 0);
        };

        let status_label = cascade! {
            Label::new(Some("Welcome!"));
            ..show();
        };

        let status = cascade! {
            Statusbar::new();
            ..add(&status_label);
            ..show_all();
        };

        let scan = Arc::new(RwLock::new(Scan::new()));

        let (error_dialog, error_dialog_window) = ErrorDialog::new();
        let (connect_dialog, connect_dialog_window) = ConnectDialog::new(error);
        let (scan_results, scan_results_window) =
            ScanResults::new(scan.clone(), thread_pool.clone());

        let (scan_result_dialog, scan_result_dialog_window) = ScanResultDialog::new();
        let (scratch_results, scratch_results_window) = ScratchResults::new(
            thread_pool.clone(),
            scan_result_dialog.clone(),
            scan_result_dialog_window.downgrade(),
        );

        let weak_error_dialog_window = error_dialog_window.downgrade();
        let weak_connect_dialog_window = connect_dialog_window.downgrade();

        let main = Rc::new(RefCell::new(MainWindow {
            thread_pool,
            scan,
            widgets: Widgets {
                error_dialog_window,
                connect_dialog_window,
                scan_result_dialog_window,
                attached_label: attached_label.downgrade(),
                scan_progress: scan_progress.downgrade(),
                scan_progress_container: scan_progress_container.downgrade(),
                status_label: status_label.downgrade(),
            },
            attached: None,
            open_process_task: None,
            filter_options: None,
            current_major_task: None,
            error_dialog,
            scan_results: scan_results.clone(),
            scratch_results: scratch_results.clone(),
            scan_result_dialog: scan_result_dialog.clone(),
        }));

        scan_results
            .borrow_mut()
            .on_add_results(clone!(scratch_results => move |_, result| {
                scratch_results.borrow_mut().add_result(result);
            }));

        connect_dialog
            .borrow_mut()
            .on_attach(clone!(main => move |_, process_id| {
                Self::attach_process_id(&main, process_id);
            }));

        status_label.connect_activate_link(clone!(main => move |_, link| {
            main.borrow_mut().handle_link(link)
        }));

        scan_cancel.connect_clicked(clone!(main => move |_| {
            main.borrow_mut().cancel_major_task();
        }));

        let (filter_options, filter_options_frame) = FilterOptions::new(
            clone!(main => move || {
                Self::scan(&main);
            }),
            clone!(main => move || {
                Self::reset(&main);
            }),
        );

        main.borrow_mut().filter_options = Some(filter_options);

        let accel_group = AccelGroup::new();

        let window = cascade! {
            ApplicationWindow::new(application);
            ..set_title("ptscan");
            ..set_position(WindowPosition::Center);
            ..set_size_request(400, 400);
            ..add_accel_group(&accel_group);
        };

        let main_menu = MainMenu::new(
            window.downgrade(),
            &accel_group,
            weak_connect_dialog_window,
            weak_error_dialog_window,
        );

        let workspace = cascade! {
            gtk::Paned::new(Horizontal);
            ..add(&cascade! {
                scan_results_window;
                ..set_border_width(10);
                ..set_size_request(400, -1);
            });
            ..add(&cascade! {
                filter_options_frame;
                ..set_border_width(10);
                ..set_size_request(-1, -1);
            });
            ..show();
        };

        let workspace = cascade! {
            gtk::Paned::new(Vertical);
            ..add(&workspace);
            ..add(&cascade! {
                scratch_results_window;
                ..set_border_width(10);
                ..set_size_request(400, -1);
            });
            ..show();
        };

        window.add(&cascade! {
            gtk::Box::new(Vertical, 10);
            ..pack_start(&main_menu, false, false, 0);
            ..pack_start(&cascade! {
                gtk::Box::new(Vertical, 10);
                ..set_border_width(10);
                ..pack_start(&attached_label, false, false, 0);
                ..pack_start(&workspace, true, true, 0);
                ..pack_start(&scan_progress_container, false, false, 0);
                ..show();
            }, true, true, 0);
            ..pack_start(&status, false, false, 0);
            ..show();
        });

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
            });
        };

        slf.start_major_task(task.run());
        slf.scan_results.borrow_mut().refresh();
    }

    /// Perform a scan.
    pub fn scan(main: &Rc<RefCell<Self>>) {
        let mut slf = main.borrow_mut();

        if slf.current_major_task.is_some() {
            return;
        }

        let handle = optional!(&slf.attached).clone();
        let filter_options = match slf.filter_options.as_ref() {
            Some(filter_options) => filter_options,
            None => return,
        };

        let filter_expr = optional!(&filter_options.borrow().state.filter_expr).clone();

        let scan = slf.scan.clone();
        let thread_pool = slf.thread_pool.clone();
        let started = Instant::now();

        let mut task = if scan.read().initial {
            task::Task::new(main, move |s| {
                let value_type = filter_expr
                    .value_type_of(TypeHint::NoHint)?
                    .ok_or_else(|| anyhow!("cannot determine type of value"))?;

                let mut scan = scan.write();

                let result = scan.initial_scan(
                    &*thread_pool,
                    &*handle,
                    &filter_expr,
                    value_type,
                    Some(s.as_token()),
                    ContextProgress::new(s),
                    Default::default(),
                );

                scan.initial = false;
                result?;
                Ok(s.is_stopped())
            })
        } else {
            task::Task::new(main, move |s| {
                let value_type = filter_expr.value_type_of(TypeHint::NoHint)?;
                let mut scan = scan.write();

                scan.scan(
                    &*thread_pool,
                    &*handle,
                    value_type.option(),
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

                main.scan_results.borrow_mut().refresh();
            }

            main.end_major_task();
        });

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

        if let Some(filter_options) = &self.filter_options {
            filter_options.borrow_mut().disable();
        }

        self.current_major_task = Some(handle);
    }

    /// End the current major task.
    fn end_major_task(&mut self) {
        if let Some(container) = self.widgets.scan_progress_container.upgrade() {
            container.hide();
        }

        if let Some(filter_options) = &self.filter_options {
            filter_options.borrow_mut().enable();
        }

        self.current_major_task = None;
    }

    /// Attach to the specified process id.
    pub fn attach_process_id(main: &Rc<RefCell<Self>>, process_id: ProcessId) {
        let mut slf = main.borrow_mut();

        let task = cascade! {
            task::Task::oneshot(main, move |_| {
                let handle = ptscan::ProcessHandle::open(process_id)?;
                let handle = handle.ok_or_else(|| anyhow!("failed to attach, no such process"))?;
                Ok(handle)
            });
            ..then(|main, handle| {
                main.open_process_task = None;

                match handle {
                    Ok(handle) => {
                        if let Some(label) = main.widgets.attached_label.upgrade() {
                            label.set_text(&format!("Attached to: {}", handle.name));
                        }

                        let handle = Arc::new(handle);
                        main.attached = Some(handle.clone());

                        if let Some(filter_options) = &main.filter_options {
                            filter_options.borrow_mut().set_handle(handle.clone());
                        }

                        main.scan_results.borrow_mut().set_handle(Some(handle.clone()));
                        main.scratch_results.borrow_mut().set_handle(Some(handle.clone()));
                        main.scan_result_dialog.borrow_mut().set_handle(Some(handle.clone()));
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
