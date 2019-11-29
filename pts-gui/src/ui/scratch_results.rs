use crate::{prelude::*, ui::EditScanResultDialog};
use anyhow::anyhow;
use parking_lot::RwLock;
use ptscan::{
    Address, FilterExpr, Pointer, PointerScan, PointerScanBackreferenceProgress,
    PointerScanInitialProgress, ProcessHandle, Size, Type, Value, ValueExpr, Values,
};
use std::{
    cell::RefCell,
    rc::Rc,
    sync::Arc,
    time::{Duration, Instant},
};

const SECOND: Duration = Duration::from_secs(1);

struct Widgets {
    model: glib::WeakRef<ListStore>,
    tree: glib::WeakRef<TreeView>,
    remove_item: glib::WeakRef<MenuItem>,
    edit_item: glib::WeakRef<MenuItem>,
    pointer_scan_item: glib::WeakRef<MenuItem>,
    scan_result_dialog_window: glib::WeakRef<Window>,
    scan_progress_container: glib::WeakRef<gtk::Box>,
    scan_progress: glib::WeakRef<ProgressBar>,
}

#[derive(Default)]
struct State {
    results: Vec<ScanResult>,
}

pub struct ScratchResults {
    thread_pool: Arc<rayon::ThreadPool>,
    state: State,
    widgets: Widgets,
    timer: Option<glib::SourceId>,
    refresh_task: Option<task::Handle>,
    handle: Option<Arc<RwLock<ProcessHandle>>>,
    /// Keep track of the generation of result being used since it might change
    /// during a refresh.
    results_generation: usize,
    /// Reference to context menu to keep it alive.
    context_menu: Menu,
    /// Menu for editing scan results.
    scan_result_dialog: Rc<RefCell<EditScanResultDialog>>,
    /// A currently running pointer scan.
    pointer_scan_task: Option<task::Handle>,
}

impl Drop for ScratchResults {
    fn drop(&mut self) {
        if let Some(timer) = self.timer.take() {
            glib::source_remove(timer);
        }
    }
}

impl ScratchResults {
    /// Construct a new container for scan results.
    pub fn new(
        builder: &Builder,
        clipboard: Rc<Clipboard>,
        thread_pool: Arc<rayon::ThreadPool>,
        scan_result_dialog: Rc<RefCell<EditScanResultDialog>>,
        scan_result_dialog_window: glib::WeakRef<Window>,
    ) -> Rc<RefCell<Self>> {
        let model = builder.get_object::<ListStore>("scratch_model");

        let tree = builder.get_object::<TreeView>("scratch_tree");
        let remove_item = builder.get_object::<MenuItem>("scratch_remove_item");
        let context_menu = builder.get_object::<Menu>("scratch_context_menu");
        let add_item = builder.get_object::<MenuItem>("scratch_add_item");
        let edit_item = builder.get_object::<MenuItem>("scratch_edit_item");
        let pointer_scan_item = builder.get_object::<MenuItem>("scratch_pointer_scan_item");

        let scan_progress_container =
            builder.get_object::<gtk::Box>("scratch_scan_progress_container");
        let scan_progress = builder.get_object::<ProgressBar>("scratch_scan_progress");

        let slf = Rc::new(RefCell::new(Self {
            thread_pool,
            widgets: Widgets {
                model: model.downgrade(),
                remove_item: remove_item.downgrade(),
                edit_item: edit_item.downgrade(),
                pointer_scan_item: pointer_scan_item.downgrade(),
                tree: tree.downgrade(),
                scan_result_dialog_window,
                scan_progress_container: scan_progress_container.downgrade(),
                scan_progress: scan_progress.downgrade(),
            },
            state: State::default(),
            timer: None,
            refresh_task: None,
            handle: None,
            results_generation: 0,
            context_menu,
            scan_result_dialog,
            pointer_scan_task: None,
        }));

        let clip = clipboard.handle("scratch");

        remove_item.connect_activate(clone!(slf => move |_| {
            let mut slf = slf.borrow_mut();
            let tree = upgrade!(slf.widgets.tree);
            let model = optional!(slf.widgets.model.upgrade());

            let (selected, _) = tree.get_selection().get_selected_rows();

            let mut to_remove = Vec::new();

            for s in selected {
                let index = match s.get_indices().into_iter().next() {
                    Some(index) => index as usize,
                    None => continue,
                };

                to_remove.push(index);
            }

            if to_remove.is_empty() {
                return;
            }

            to_remove.sort_by(|a, b| b.cmp(a));

            for index in to_remove {
                let iter = match model.iter_nth_child(None, index as i32) {
                    Some(iter) => iter,
                    None => continue,
                };

                model.remove(&iter);
                slf.state.results.swap_remove(index);
            }

            slf.results_generation += 1;
        }));

        edit_item.connect_activate(clone!(slf => move |_| {
            let tree = upgrade!(slf.borrow().widgets.tree);
            let path = optional!(tree.get_selection().get_selected_rows().0.into_iter().next());
            let index = optional!(path.get_indices().into_iter().next()) as usize;
            Self::open_result_editor(&slf, index);
        }));

        pointer_scan_item.connect_activate(clone!(slf => move |_| {
            let tree = upgrade!(slf.borrow().widgets.tree);
            let path = optional!(tree.get_selection().get_selected_rows().0.into_iter().next());
            let index = optional!(path.get_indices().into_iter().next()) as usize;
            Self::pointer_scan_for(&slf, index);
        }));

        cascade! {
            add_item;
            ..connect_activate(clone!(slf => move |_| {
                let s = slf.borrow();

                {
                    let mut scan_result_dialog = s.scan_result_dialog.borrow_mut();

                    scan_result_dialog.set_result(ScanResult {
                        address: Address::null(),
                        pointer: Pointer::null(),
                        initial_type: Type::default(),
                        initial: Value::default(),
                        last_type: Type::default(),
                        last: Value::default(),
                    });

                    scan_result_dialog.on_save(clone!(slf => move |_, result| {
                        let mut s = slf.borrow_mut();
                        s.add_result(result);
                    }));
                }

                let scan_result_dialog_window = upgrade!(s.widgets.scan_result_dialog_window);
                scan_result_dialog_window.show();
                scan_result_dialog_window.present();
            }));
        };

        slf.borrow_mut().timer = Some(glib::source::timeout_add_local(
            500,
            clone!(slf => move || {
                Self::refresh_current(&slf);
                glib::Continue(true)
            }),
        ));

        let tree = cascade! {
            builder.get_object::<TreeView>("scratch_tree");
            ..connect_row_activated(clone!(slf => move |_, path, _| {
                let index = optional!(path.get_indices().into_iter().next()) as usize;
                Self::open_result_editor(&slf, index);
            }));
            ..connect_button_press_event(clone!(slf => move |_, e| {
                match (e.get_event_type(), e.get_button()) {
                    (EventType::ButtonPress, 3) => {
                        let slf = slf.borrow();
                        let tree = upgrade!(slf.widgets.tree, Inhibit(false));
                        let remove_item = upgrade!(slf.widgets.remove_item, Inhibit(false));
                        let edit_item = upgrade!(slf.widgets.edit_item, Inhibit(false));
                        let pointer_scan_item = upgrade!(slf.widgets.pointer_scan_item, Inhibit(false));

                        let any_selected = tree.get_selection().count_selected_rows() > 0;
                        remove_item.set_sensitive(any_selected);
                        edit_item.set_sensitive(any_selected);
                        pointer_scan_item.set_sensitive(any_selected && slf.handle.is_some());

                        slf.context_menu.popup_at_pointer(Some(&e));
                        slf.context_menu.show();
                        Inhibit(true)
                    }
                    _ => Inhibit(false),
                }
            }));
        };

        clip.hook_tree(
            &tree,
            clone!(slf => move |selection| {
                let slf = slf.borrow();

                let (paths, model) = selection.get_selected_rows();

                let mut results = Vec::new();

                for path in paths {
                    let iter = model.get_iter(&path)?;
                    let index = model.get_value(&iter, 0).get::<u64>().ok()?? as usize;
                    let result = slf.state.results.get(index)?.clone();
                    results.push(result);
                }

                Some(ClipboardBuffer::Results(results))
            }),
        );

        slf
    }

    /// Refresh the current set of results.
    pub fn refresh_current(slf_rc: &Rc<RefCell<Self>>) {
        let mut slf = slf_rc.borrow_mut();

        // waiting for refresh already in progress.
        if slf.refresh_task.is_some() {
            return;
        }

        // no results to refresh.
        if slf.state.results.is_empty() {
            return;
        }

        let handle = optional!(slf.handle.clone());

        let mut results = slf.state.results.clone();
        let thread_pool = slf.thread_pool.clone();
        let results_generation = slf.results_generation;

        let task = cascade! {
            task::Task::oneshot(slf_rc, move |c| {
                let handle = match handle.try_read() {
                    Some(handle) => handle,
                    None => return Ok(None),
                };

                handle.refresh_dynamic_values(
                    &*thread_pool,
                    &mut results,
                    Some(c.as_token()),
                    NoopProgress,
                    &ValueExpr::Value,
                )?;

                let values = results.into_iter().map(|r| r.last.unwrap_or(r.initial)).collect::<Vec<_>>();

                Ok(Some(values))
            });
            ..then(move |scratch_results, values| {
                if let Ok(Some(values)) = values {
                    scratch_results.refresh_visible_diff(results_generation, values);
                }

                scratch_results.refresh_task = None;
            });
        };

        slf.refresh_task = Some(task.run());
    }

    /// Refresh the collection of scan results being showed.
    pub fn add_result(&mut self, result: ScanResult) {
        let model = upgrade!(self.widgets.model);

        let ty = result.last_type.to_string();
        let address = result.address.to_string();
        let initial = result.initial.to_string();
        let last = result.last.to_string();

        let iter = model.insert_with_values(None, &[1, 2, 3, 4], &[&ty, &address, &initial, &last]);
        model.set_value(&iter, 0, &(self.state.results.len() as u64).to_value());

        self.state.results.push(result);
        self.results_generation += 1;
    }

    /// Refresh the collection of scan results being showed.
    pub fn edit_result(&mut self, results_generation: usize, index: usize, result: ScanResult) {
        if self.results_generation != results_generation {
            return;
        }

        let model = upgrade!(self.widgets.model);

        let ty = result.last_type.to_string();
        let address = result.address.to_string();
        let initial = result.initial.to_string();
        let last = result.last.to_string();

        if let Some(iter) = model.iter_nth_child(None, index as i32) {
            model.set(&iter, &[1, 2, 3, 4], &[&ty, &address, &initial, &last]);
        }

        if let Some(r) = self.state.results.get_mut(index) {
            *r = result;
        }

        self.results_generation += 1;
    }

    /// Refresh the visible results if the differ from the stored ones.
    pub fn refresh_visible_diff(&mut self, results_generation: usize, values: Vec<Value>) {
        if self.results_generation != results_generation {
            return;
        }

        let model = upgrade!(self.widgets.model);

        for (index, (result, update)) in self.state.results.iter_mut().zip(values).enumerate() {
            if result.last == update {
                continue;
            }

            if let Some(iter) = model.iter_nth_child(None, index as i32) {
                model.set_value(&iter, 4, &update.to_string().to_value());
            }

            result.last = update;
        }
    }

    /// Set the current process handle.
    pub fn set_handle(&mut self, handle: Option<Arc<RwLock<ProcessHandle>>>) {
        self.handle = handle;
    }

    /// Open the result editor for editing the result at the given location.
    fn open_result_editor(scratch_results: &Rc<RefCell<Self>>, index: usize) {
        let slf = scratch_results.borrow();

        let result = optional!(slf.state.results.get(index));

        {
            let mut scan_result_dialog = slf.scan_result_dialog.borrow_mut();

            scan_result_dialog.set_result(result.clone());
            let results_generation = slf.results_generation;

            scan_result_dialog.on_save(clone!(scratch_results => move |_, result| {
                let mut scratch_results = scratch_results.borrow_mut();
                scratch_results.edit_result(results_generation, index, result);
            }));
        }

        let scan_result_dialog_window = upgrade!(slf.widgets.scan_result_dialog_window);
        scan_result_dialog_window.show();
        scan_result_dialog_window.present();
    }

    /// Open the result editor for editing the result at the given location.
    fn pointer_scan_for(slf_rc: &Rc<RefCell<Self>>, index: usize) {
        let mut slf = slf_rc.borrow_mut();

        if slf.pointer_scan_task.is_some() {
            return;
        }

        let scan_progress_container = upgrade!(slf.widgets.scan_progress_container);

        let handle = optional!(&slf.handle).clone();
        let thread_pool = slf.thread_pool.clone();
        let result = optional!(slf.state.results.get(index)).clone();

        scan_progress_container.show();

        let mut current = "";

        let task = cascade! {
            task::Task::new(slf_rc, move |c| {
                let handle = handle.read();

                let needle = result.address;
                let pointer_filter = FilterExpr::pointer(ValueExpr::Value, &handle.process)?;

                let token = c.as_token();

                let mut addresses = Vec::new();
                let mut values = Values::new(Type::Pointer, &handle.process);

                handle.initial_scan(
                    &*thread_pool,
                    &pointer_filter,
                    &mut addresses,
                    &mut values,
                    Some(token),
                    PointerScanProgress::new(c, "Initial pointer scan"),
                    Default::default(),
                )?;

                let mut pointer_scan = PointerScan::new(&*thread_pool, &*handle, token);
                pointer_scan.max_depth = 7;
                pointer_scan.max_offset = Size::new(0x1000);

                pointer_scan.build_references(addresses.into_iter(), values.iter())?;

                if c.is_stopped() {
                    return Err(anyhow!("pointer scan cancelled"));
                }

                let mut results = Vec::new();
                pointer_scan.scan(needle, &mut results, &mut InitialProgress::new(c, "Scanning for paths"))?;

                if c.is_stopped() {
                    return Err(anyhow!("pointer scan cancelled"));
                }

                pointer_scan.backreference_scan(
                    &mut results,
                    &mut BackreferenceProgress::new(c, "Picking up trailing backreferences"),
                )?;

                Ok(results)
            });
            ..emit(move |main, progress| {
                match progress {
                    Progress::StartTask { name, pulse } => {
                        current = name;
                        let progress = upgrade!(main.widgets.scan_progress);

                        if pulse {
                            progress.set_pulse_step(1.0);
                        } else {
                            progress.set_fraction(0.0);
                        }
                    }
                    Progress::Tick { percentage, results } => {
                        let progress = upgrade!(main.widgets.scan_progress);
                        let fraction = (percentage as f64) / 100f64;
                        progress.set_fraction(fraction);
                        progress.set_text(Some(&format!("{}: Found {} pointer(s)", current, results)));
                    }
                    Progress::QueueTick { queue_len, results } => {
                        let progress = upgrade!(main.widgets.scan_progress);
                        progress.set_text(Some(&format!("{}: Queue length: {}, Results: {}", current, queue_len, results)));

                        progress.pulse();
                    }
                    Progress::BackreferenceTick { remaining, results } => {
                        let progress = upgrade!(main.widgets.scan_progress);
                        progress.set_fraction(100.0);
                        progress.set_text(Some(&format!("{}: Remaining: {}, Results: {}", current, remaining, results)));

                        progress.pulse();
                    }
                }
            });
            ..then(|slf, results| {
                let scan_progress_container = upgrade!(slf.widgets.scan_progress_container);
                scan_progress_container.hide();

                if let Ok(_results) = results {
                    /* ignore */
                }

                slf.pointer_scan_task = None;
            });
        };

        slf.pointer_scan_task = Some(task.run());
    }
}

struct NoopProgress;

impl ptscan::ScanProgress for NoopProgress {
    fn report_bytes(&mut self, _: ptscan::Size) -> anyhow::Result<()> {
        Ok(())
    }

    fn report(&mut self, _: usize, _: u64) -> anyhow::Result<()> {
        Ok(())
    }
}

pub enum Progress {
    StartTask { name: &'static str, pulse: bool },
    Tick { percentage: usize, results: u64 },
    QueueTick { queue_len: usize, results: usize },
    BackreferenceTick { remaining: usize, results: usize },
}

struct PointerScanProgress<'a, T> {
    context: &'a task::Context<Progress, T>,
}

impl<'a, T> PointerScanProgress<'a, T> {
    pub fn new(context: &'a task::Context<Progress, T>, name: &'static str) -> Self {
        let _ = context.emit(Progress::StartTask { name, pulse: false });
        Self { context }
    }
}

impl<'a, T> ptscan::ScanProgress for PointerScanProgress<'a, T> {
    fn report_bytes(&mut self, _: ptscan::Size) -> anyhow::Result<()> {
        Ok(())
    }

    fn report(&mut self, percentage: usize, results: u64) -> anyhow::Result<()> {
        let _ = self.context.emit(Progress::Tick {
            percentage,
            results,
        });
        Ok(())
    }
}

struct InitialProgress<'a, T> {
    context: &'a task::Context<Progress, T>,
    last: Instant,
}

impl<'a, T> InitialProgress<'a, T> {
    pub fn new(context: &'a task::Context<Progress, T>, name: &'static str) -> Self {
        let _ = context.emit(Progress::StartTask { name, pulse: true });
        Self {
            context,
            last: Instant::now(),
        }
    }
}

impl<'a, T> PointerScanInitialProgress for InitialProgress<'a, T> {
    fn report(&mut self, queue_len: usize, results: usize) -> anyhow::Result<()> {
        if queue_len % 1000 != 0 {
            return Ok(());
        }

        let now = Instant::now();

        if now.duration_since(self.last) < SECOND {
            return Ok(());
        }

        let _ = self
            .context
            .emit(Progress::QueueTick { queue_len, results });

        self.last = now;
        Ok(())
    }
}

struct BackreferenceProgress<'a, T> {
    context: &'a task::Context<Progress, T>,
    last: Instant,
}

impl<'a, T> BackreferenceProgress<'a, T> {
    pub fn new(context: &'a task::Context<Progress, T>, name: &'static str) -> Self {
        let _ = context.emit(Progress::StartTask { name, pulse: true });
        Self {
            context,
            last: Instant::now(),
        }
    }
}

impl<'a, T> PointerScanBackreferenceProgress for BackreferenceProgress<'a, T> {
    fn report(&mut self, remaining: usize, results: usize) -> anyhow::Result<()> {
        if remaining % 1000 != 0 {
            return Ok(());
        }

        let now = Instant::now();

        if now.duration_since(self.last) < SECOND {
            return Ok(());
        }

        let _ = self
            .context
            .emit(Progress::BackreferenceTick { remaining, results });

        self.last = now;
        Ok(())
    }
}
