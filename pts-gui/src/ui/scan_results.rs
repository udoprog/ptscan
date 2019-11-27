use crate::prelude::*;
use parking_lot::RwLock;
use ptscan::{ProcessHandle, Scan, ScanResult, Value, ValueExpr};
use std::{cell::RefCell, rc::Rc, sync::Arc};

struct Widgets {
    model: glib::WeakRef<ListStore>,
    tree: glib::WeakRef<TreeView>,
    add_item: glib::WeakRef<MenuItem>,
    info_item: glib::WeakRef<MenuItem>,
    show_scan_result_dialog_window: glib::WeakRef<Window>,
    context_menu: Menu,
}

struct Visible {
    results: Vec<Box<ScanResult>>,
    values: Vec<Value>,
}

pub struct ScanResults {
    scan: Arc<RwLock<Scan>>,
    thread_pool: Arc<rayon::ThreadPool>,
    widgets: Widgets,
    settings: Arc<Settings>,
    visible: Option<Visible>,
    timer: Option<glib::SourceId>,
    refresh_task: Option<task::Handle>,
    handle: Option<Arc<RwLock<ProcessHandle>>>,
    /// Keep track of the generation of result being used since it might change
    /// during a refresh.
    results_generation: usize,
    /// Handler to call when it is requested that we add a result.
    on_add_results: Option<Box<dyn Fn(&Self, &Box<ScanResult>)>>,
    /// Menu for editing scan results.
    show_scan_result_dialog: Rc<RefCell<ui::ShowScanResultDialog>>,
    /// Current value expression in use.
    value_expr: ValueExpr,
}

impl Drop for ScanResults {
    fn drop(&mut self) {
        if let Some(timer) = self.timer.take() {
            glib::source_remove(timer);
        }
    }
}

impl ScanResults {
    /// Construct a new container for scan results.
    pub fn new(
        builder: &Builder,
        clipboard: Rc<Clipboard>,
        settings: Arc<Settings>,
        scan: Arc<RwLock<Scan>>,
        thread_pool: Arc<rayon::ThreadPool>,
        show_scan_result_dialog: Rc<RefCell<ui::ShowScanResultDialog>>,
        show_scan_result_dialog_window: glib::WeakRef<Window>,
    ) -> Rc<RefCell<Self>> {
        let model = builder.get_object::<ListStore>("scan_results_model");
        let tree = builder.get_object::<TreeView>("scan_results_tree");

        let context_menu = builder.get_object::<Menu>("scan_results_context_menu");
        let add_item = builder.get_object::<MenuItem>("scan_results_add_item");
        let info_item = builder.get_object::<MenuItem>("scan_results_info_item");

        let slf = Rc::new(RefCell::new(Self {
            scan,
            thread_pool,
            widgets: Widgets {
                model: model.downgrade(),
                tree: tree.downgrade(),
                add_item: add_item.downgrade(),
                info_item: info_item.downgrade(),
                show_scan_result_dialog_window,
                context_menu,
            },
            settings,
            visible: None,
            timer: None,
            refresh_task: None,
            handle: None,
            results_generation: 0,
            on_add_results: None,
            show_scan_result_dialog,
            value_expr: ValueExpr::Value,
        }));

        let paste = Rc::new(RefCell::new(ClipboardHandle::default()));

        add_item.connect_activate(clone!(slf => move |_| {
            let slf = slf.borrow();
            let tree = upgrade!(slf.widgets.tree);

            let on_add_results = optional!(&slf.on_add_results);
            let visible = optional!(&slf.visible);

            let (selected, _) = tree.get_selection().get_selected_rows();

            for s in selected {
                let index = match s.get_indices().into_iter().next() {
                    Some(index) => index as usize,
                    None => continue,
                };

                if let Some(result) = visible.results.get(index) {
                    on_add_results(&*slf, result);
                }
            }
        }));

        info_item.connect_activate(clone!(slf => move |_| {
            let slf = slf.borrow();
            let visible = optional!(&slf.visible);
            let tree = upgrade!(slf.widgets.tree);
            let path = optional!(tree.get_selection().get_selected_rows().0.into_iter().next());
            let index = optional!(path.get_indices().into_iter().next());
            let result = optional!(visible.results.get(index as usize)).clone();
            let window = upgrade!(slf.widgets.show_scan_result_dialog_window);
            slf.show_scan_result_dialog.borrow_mut().set_result(result);
            window.show();
            window.present();
        }));

        slf.borrow_mut().timer = Some(glib::source::timeout_add_local(
            500,
            clone!(slf => move || {
                Self::refresh_current(&slf);
                glib::Continue(true)
            }),
        ));

        cascade! {
            tree;
            ..connect_row_activated(clone!(slf => move |_, path, _| {
                let slf = slf.borrow();

                let on_add_results = optional!(&slf.on_add_results);
                let visible = optional!(&slf.visible);
                let index = optional!(path.get_indices().into_iter().next());
                let result = optional!(visible.results.get(index as usize));

                on_add_results(&*slf, result);
            }));
            ..connect_button_press_event(clone!(slf => move |_, e| {
                match (e.get_event_type(), e.get_button()) {
                    (EventType::ButtonPress, 3) => {
                        let slf = slf.borrow();

                        let tree = upgrade!(slf.widgets.tree, Inhibit(false));
                        let add_item = upgrade!(slf.widgets.add_item, Inhibit(false));
                        let info_item = upgrade!(slf.widgets.info_item, Inhibit(false));

                        let any_selected = tree.get_selection().count_selected_rows() > 0;
                        add_item.set_sensitive(any_selected);
                        info_item.set_sensitive(any_selected);

                        slf.widgets.context_menu.popup_at_pointer(Some(&e));
                        slf.widgets.context_menu.show();
                        Inhibit(true)
                    }
                    _ => Inhibit(false),
                }
            }));
            ..get_selection().connect_changed(clone!(paste, clipboard, slf => move |selection| {
                *paste.borrow_mut() = clipboard.from_selection(selection, clone!(slf => move |selection| {
                    let slf = slf.borrow();
                    let visible = slf.visible.as_ref()?;

                    let (paths, model) = selection.get_selected_rows();

                    let mut results = Vec::new();

                    for path in paths {
                        let iter = model.get_iter(&path)?;
                        let index = model.get_value(&iter, 0).get::<u64>().ok()?? as usize;
                        let mut result = visible.results.get(index)?.clone();
                        result.last = visible.values.get(index).cloned();
                        results.push(result);
                    }

                    if results.len() <= 1 {
                        return results.into_iter().next().map(ClipboardBuffer::Result);
                    }

                    Some(ClipboardBuffer::Results(results))
                }));
            }));
        };

        slf
    }

    /// Set the current value expression.
    pub fn set_value_expr(&mut self, value_expr: ValueExpr) {
        self.value_expr = value_expr;
    }

    /// Request when the component wants a result to be added to the scratch pad.
    pub fn on_add_results<T>(&mut self, on_add_results: T)
    where
        T: 'static + Fn(&Self, &Box<ScanResult>),
    {
        self.on_add_results = Some(Box::new(on_add_results));
    }

    /// Refresh the collection of scan results being showed.
    pub fn refresh(&mut self) {
        let scan = optional!(self.scan.try_read());
        let model = upgrade!(self.widgets.model);

        model.clear();

        let results = scan.results.iter().take(1000).cloned().collect::<Vec<_>>();

        for (index, result) in results.iter().enumerate() {
            let last = result.last();
            let ty = last.ty().to_string();
            let pointer = result.pointer.to_string();
            let initial = result.initial.to_string();
            let last = result.last().to_string();

            let iter = model.insert_with_values(
                None,
                &[1, 2, 3, 4, 5, 6],
                &[
                    &ty,
                    &pointer,
                    &initial,
                    &last,
                    &last,
                    &self.settings.default_cell_background.to_value(),
                ],
            );

            model.set_value(&iter, 0, &(index as u64).to_value());
        }

        let values = results.iter().map(|r| r.last().clone()).collect();
        self.visible = Some(Visible { results, values });
        self.results_generation += 1;
    }

    /// Refresh the current set of results.
    fn refresh_current(slf_rc: &Rc<RefCell<Self>>) {
        let mut slf = slf_rc.borrow_mut();

        // do not refresh
        if slf.refresh_task.is_some() {
            return;
        }

        let handle = optional!(slf.handle.clone());
        let mut results = optional!(&slf.visible).results.clone();
        let thread_pool = slf.thread_pool.clone();
        let results_generation = slf.results_generation;
        let value_expr = slf.value_expr.clone();

        let task = cascade! {
            task::Task::oneshot(slf_rc, move |c| {
                let handle = match handle.try_read() {
                    Some(handle) => handle,
                    None => return Ok(None),
                };

                handle.refresh_values(
                    &*thread_pool,
                    &mut results,
                    Some(c.as_token()),
                    None,
                    NoopProgress,
                    &value_expr,
                )?;

                let values = results.into_iter().map(|result| result.last.unwrap_or(result.initial)).collect::<Vec<_>>();

                Ok(Some(values))
            });
            ..then(move |slf, values| {
                if let Ok(Some(values)) = values {
                    slf.refresh_visible_diff(results_generation, values);
                }

                slf.refresh_task = None;
            });
        };

        slf.refresh_task = Some(task.run());
    }

    /// Refresh the visible results if the differ from the stored ones.
    fn refresh_visible_diff(&mut self, results_generation: usize, values: Vec<Value>) {
        if self.results_generation != results_generation {
            return;
        }

        let model = upgrade!(self.widgets.model);

        let visible = optional!(&mut self.visible);

        for (index, (current, (result, update))) in visible
            .values
            .iter_mut()
            .zip(visible.results.iter().zip(values))
            .enumerate()
        {
            if *current == update {
                continue;
            }

            if let Some(iter) = model.iter_nth_child(None, index as i32) {
                model.set_value(&iter, 5, &update.to_string().to_value());

                let background = if *result.last() == update {
                    &self.settings.default_cell_background
                } else {
                    &self.settings.highlight_cell_background
                };

                model.set_value(&iter, 6, &background.to_value());
            }

            *current = update;
        }
    }

    /// Set the current process handle.
    pub fn set_handle(&mut self, handle: Option<Arc<RwLock<ProcessHandle>>>) {
        self.handle = handle;
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
