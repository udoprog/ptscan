use crate::{prelude::*, FilterOptions, MainMenu};
use parking_lot::RwLock;
use std::{cell::RefCell, rc::Rc, sync::Arc};

use self::Orientation::*;
use anyhow::anyhow;
use ptscan::{ProcessId, Scan, TypeHint};

struct Widgets {
    attached_label: glib::WeakRef<Label>,
    scan_progress: glib::WeakRef<ProgressBar>,
}

pub struct MainWindow {
    thread_pool: Arc<rayon::ThreadPool>,
    scan: Arc<RwLock<Scan>>,
    widgets: Widgets,
    attached: Option<Arc<ptscan::ProcessHandle>>,
    open_process_task: Option<task::Handle>,
    filter_options: Option<Rc<RefCell<FilterOptions>>>,
    current_major_task: Option<task::Handle>,
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
        };

        let main = Rc::new(RefCell::new(MainWindow {
            thread_pool,
            scan: Arc::new(RwLock::new(Scan::new())),
            widgets: Widgets {
                attached_label: attached_label.downgrade(),
                scan_progress: scan_progress.downgrade(),
            },
            attached: None,
            open_process_task: None,
            filter_options: None,
            current_major_task: None,
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

        let attach = clone!(main => move |process_id| {
            Self::attach_process_id(&main, process_id);
        });

        let main_menu = MainMenu::new(&window, &accel_group, attach, error);

        let address_cell = CellRendererText::new();
        let value_cell = CellRendererText::new();
        let current_cell = CellRendererText::new();

        let scan_results = cascade! {
            TreeView::new();
            ..set_headers_visible(true);
            ..append_column(&cascade! {
                TreeViewColumn::new();
                    ..set_title("address");
                    ..pack_start(&address_cell, false);
                    ..add_attribute(&address_cell, "text", 0);
            });
            ..append_column(&cascade! {
                TreeViewColumn::new();
                    ..set_title("value");
                    ..pack_start(&value_cell, true);
                    ..add_attribute(&value_cell, "text", 1);
            });
            ..append_column(&cascade! {
                TreeViewColumn::new();
                    ..set_title("current");
                    ..pack_start(&current_cell, true);
                    ..add_attribute(&current_cell, "text", 2);
            });
            ..show_all();
        };

        let model = ListStore::new(&[ptscan::ProcessId::static_type(), String::static_type()]);
        scan_results.set_model(Some(&model));

        let scan_results = cascade! {
            gtk::ScrolledWindow::new(gtk::NONE_ADJUSTMENT, gtk::NONE_ADJUSTMENT);
            ..set_policy(gtk::PolicyType::Automatic, gtk::PolicyType::Automatic);
            ..add(&scan_results);
            ..show();
        };

        window.add(&cascade! {
            gtk::Box::new(Vertical, 10);
            ..pack_start(&main_menu, false, false, 0);
            ..pack_start(&cascade! {
                gtk::Box::new(Vertical, 10);
                ..set_border_width(10);
                ..pack_start(&attached_label, false, false, 0);
                ..pack_start(&filter_options_frame, false, false, 0);
                ..pack_start(&scan_results, true, true, 0);
                ..show();
            }, true, true, 0);
            ..pack_start(&scan_progress, false, false, 0);
            ..show();
        });

        window.show();
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
                main.current_major_task = None;
            });
        };

        slf.current_major_task = Some(task.run());
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

        if let Some(progress) = slf.widgets.scan_progress.upgrade() {
            progress.set_fraction(0f64);
            progress.set_text(None);
            progress.show();
        }

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
                result
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
                )
            })
        };

        task.emit(|main, (percentage, results)| {
            if let Some(progress) = main.widgets.scan_progress.upgrade() {
                let fraction = (percentage as f64) / 100f64;
                progress.set_fraction(fraction);
                progress.set_text(Some(&format!("found {} result(s)", results)));
            }
        });

        task.then(|main, _| {
            main.current_major_task = None;

            if let Some(progress) = main.widgets.scan_progress.upgrade() {
                progress.hide();
            }
        });

        slf.current_major_task = Some(task.run());
    }

    /// Attach to the specified process id.
    pub fn attach_process_id(main: &Rc<RefCell<Self>>, process_id: ProcessId) {
        let mut slf = main.borrow_mut();

        let task = cascade! {
            task::Task::oneshot(main, move |_| {
                let process = ptscan::ProcessHandle::open(process_id)?;
                let process = process.ok_or_else(|| anyhow!("failed to attach, no such process"))?;
                Ok(process)
            });
            ..then(|main, process| {
                main.open_process_task = None;

                match process {
                    Ok(process) => {
                        if let Some(label) = main.widgets.attached_label.upgrade() {
                            label.set_text(&format!("Attached to: {}", process.name));
                        }

                        let process = Arc::new(process);
                        main.attached = Some(process.clone());

                        if let Some(filter_options) = &main.filter_options {
                            filter_options.borrow_mut().set_process(process);
                        }
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
        self.context.emit((percentage, results));
        Ok(())
    }
}
