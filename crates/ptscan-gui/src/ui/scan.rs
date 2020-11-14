use crate::{prelude::*, CurrentScanResult};
use parking_lot::RwLock;
use ptscan::{Addresses, FilterExpr, ProcessHandle, TypeHint, ValueExpr, Values};
use std::{cell::RefCell, rc::Rc, sync::Arc};

struct Widgets {
    model: glib::WeakRef<ListStore>,
    tree: glib::WeakRef<TreeView>,
    add_item: glib::WeakRef<MenuItem>,
    info_item: glib::WeakRef<MenuItem>,
    remove_item: glib::WeakRef<MenuItem>,
    show_scan_result_dialog_window: glib::WeakRef<Window>,
    context_menu: Menu,
}

struct Visible {
    scan: scan::Scan,
    current: Values,
}

pub struct Scan {
    session: Rc<RefCell<scan::Session>>,
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
    on_add_results: Option<Box<dyn Fn(&Self, ScanResult)>>,
    /// Menu for editing scan results.
    show_scan_result_dialog: Rc<RefCell<ui::ShowScanResultDialog>>,
    /// Current value expression in use.
    filter_expr: Option<FilterExpr>,
    value_expr: ValueExpr,
}

impl Drop for Scan {
    fn drop(&mut self) {
        if let Some(timer) = self.timer.take() {
            glib::source_remove(timer);
        }
    }
}

impl Scan {
    /// Construct a new container for scan results.
    pub fn new(
        builder: &Builder,
        clipboard: Rc<Clipboard>,
        settings: Arc<Settings>,
        session: Rc<RefCell<scan::Session>>,
        thread_pool: Arc<rayon::ThreadPool>,
        show_scan_result_dialog: Rc<RefCell<ui::ShowScanResultDialog>>,
        show_scan_result_dialog_window: glib::WeakRef<Window>,
    ) -> Rc<RefCell<Self>> {
        let model = builder.get_object::<ListStore>("scan_model");
        let tree = builder.get_object::<TreeView>("scan_tree");

        let context_menu = builder.get_object::<Menu>("scan_context_menu");
        let add_item = builder.get_object::<MenuItem>("scan_add_item");
        let info_item = builder.get_object::<MenuItem>("scan_info_item");
        let remove_item = builder.get_object::<MenuItem>("scan_remove_item");

        let slf = Rc::new(RefCell::new(Self {
            session,
            thread_pool,
            widgets: Widgets {
                model: model.downgrade(),
                tree: tree.downgrade(),
                add_item: add_item.downgrade(),
                info_item: info_item.downgrade(),
                remove_item: remove_item.downgrade(),
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
            filter_expr: None,
            value_expr: ValueExpr::Value,
        }));

        let clip = clipboard.handle("scan");

        add_item.connect_activate(clone!(slf => move |_| {
            let slf = slf.borrow_mut();
            let tree = upgrade!(slf.widgets.tree);
            let handle = optional!(optional!(slf.handle.as_ref()).try_read());

            let visible = optional!(&slf.visible);
            let on_add_results = optional!(&slf.on_add_results);

            let (selected, _) = tree.get_selection().get_selected_rows();

            for s in selected {
                let index = match s.get_indices().into_iter().next() {
                    Some(index) => index as usize,
                    None => continue,
                };

                if let Some(result) = visible.scan.get(&*handle, index) {
                    on_add_results(&*slf, result);
                }
            }
        }));

        info_item.connect_activate(clone!(slf => move |_| {
            let slf = slf.borrow();
            let visible = optional!(&slf.visible);
            let handle = optional!(optional!(slf.handle.as_ref()).try_read());
            let tree = upgrade!(slf.widgets.tree);
            let path = optional!(tree.get_selection().get_selected_rows().0.into_iter().next());
            let index = optional!(path.get_indices().into_iter().next());
            let result = optional!(visible.scan.get(&*handle, index as usize)).clone();
            let window = upgrade!(slf.widgets.show_scan_result_dialog_window);
            slf.show_scan_result_dialog.borrow_mut().set_result(result);
            window.show();
            window.present();
        }));

        remove_item.connect_activate(clone!(slf => move |_| {
            let mut slf = slf.borrow_mut();
            let tree = upgrade!(slf.widgets.tree);
            slf.remove_selection(tree.get_selection());
        }));

        slf.borrow_mut().timer = Some(glib::source::timeout_add_local(
            500,
            clone!(slf => move || {
                Self::refresh_current(&slf);
                glib::Continue(true)
            }),
        ));

        let tree = cascade! {
            tree;
            ..connect_row_activated(clone!(slf => move |_, path, _| {
                let slf = slf.borrow();

                let handle = optional!(optional!(slf.handle.as_ref()).try_read());
                let on_add_results = optional!(&slf.on_add_results);
                let visible = optional!(&slf.visible);
                let index = optional!(path.get_indices().into_iter().next());
                let result = optional!(visible.scan.get(&*handle, index as usize));

                on_add_results(&*slf, result);
            }));
            ..connect_key_press_event(clone!(slf => move |tree, e| {
                match (e.get_event_type(), e.get_keycode()) {
                    (EventType::KeyPress, Some(46)) => {
                        slf.borrow_mut().remove_selection(tree.get_selection());
                    }
                    _ => (),
                }

                Inhibit(false)
            }));
            ..connect_button_press_event(clone!(slf => move |_, e| {
                match (e.get_event_type(), e.get_button()) {
                    (EventType::ButtonPress, 3) => {
                        let slf = slf.borrow();

                        let tree = upgrade!(slf.widgets.tree, Inhibit(false));
                        let add_item = upgrade!(slf.widgets.add_item, Inhibit(false));
                        let info_item = upgrade!(slf.widgets.info_item, Inhibit(false));
                        let remove_item = upgrade!(slf.widgets.remove_item, Inhibit(false));

                        let any_selected = tree.get_selection().count_selected_rows() > 0;
                        add_item.set_sensitive(any_selected);
                        info_item.set_sensitive(any_selected);
                        remove_item.set_sensitive(any_selected);

                        slf.widgets.context_menu.popup_at_pointer(Some(&e));
                        slf.widgets.context_menu.show();
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
                let visible = slf.visible.as_ref()?;
                let handle = slf.handle.as_ref()?.try_read()?;

                let (paths, model) = selection.get_selected_rows();

                let mut results = Vec::new();

                for path in paths {
                    let iter = model.get_iter(&path)?;
                    let index = model.get_value(&iter, 0).get::<u64>().ok()?? as usize;
                    let result = visible.scan.get(&*handle, index)?.clone();
                    let current = visible.current.get(index).clone().map(|v| (visible.current.ty(), v));
                    results.push(CurrentScanResult { result, current });
                }

                Some(ClipboardBuffer::CurrentResults(results))
            }),
        );

        slf
    }

    /// Set the current filter expression.
    pub fn set_filter_expr(&mut self, filter_expr: Option<FilterExpr>) {
        self.filter_expr = filter_expr;
    }

    /// Set the current value expression.
    pub fn set_value_expr(&mut self, value_expr: ValueExpr) {
        self.value_expr = value_expr;
    }

    /// Clear the current value expression.
    pub fn clear_value_expr(&mut self) {
        self.value_expr = ValueExpr::Value;
    }

    /// Request when the component wants a result to be added to the scratch pad.
    pub fn on_add_results<T>(&mut self, on_add_results: T)
    where
        T: 'static + Fn(&Self, ScanResult),
    {
        self.on_add_results = Some(Box::new(on_add_results));
    }

    /// Remove the given selection.
    fn remove_selection(&mut self, selection: TreeSelection) {
        let (paths, model) = selection.get_selected_rows();

        let mut to_remove = Vec::new();

        for path in paths {
            if let Some(iter) = model.get_iter(&path) {
                let index =
                    optional!(optional!(model.get_value(&iter, 0).get::<u64>().ok())) as usize;
                to_remove.push(index);
            }
        }

        if to_remove.is_empty() {
            return;
        }

        to_remove.sort_by(|a, b| b.cmp(a));

        {
            let session = self.session.borrow();
            let scan = optional!(session.last());
            let mut scan = optional!(scan.try_write());

            for index in to_remove {
                scan.swap_remove(index);
            }
        }

        self.refresh();
    }

    /// Refresh the collection of scan results being showed.
    pub fn refresh(&mut self) {
        let session = self.session.borrow();
        let model = upgrade!(self.widgets.model);

        // NB: make sure to clear the model regardless if a scan is available or not.
        model.clear();

        let last_scan = optional!(session.last());
        let scan = optional!(last_scan.try_read());

        let mut initial = Values::with_capacity(scan.initial.ty(), 100);
        initial.extend(scan.initial.iter().take(100));

        let mut last = Values::with_capacity(scan.last.ty(), 100);
        last.extend(scan.last.iter().take(100));

        let mut addresses = Addresses::with_capacity(scan.addresses.ty(), 100);
        addresses.extend(scan.addresses.iter().take(100));

        let it = addresses.iter_copied().zip(initial.iter().zip(last.iter()));

        let handle = match &self.handle {
            Some(handle) => Some(handle.read()),
            None => None,
        };

        for (index, (address, (initial, last))) in it.enumerate() {
            let ty = scan.initial.ty().to_string();

            let pointer = match &handle {
                Some(handle) => handle.address_to_portable_base(address).to_string(),
                None => address.to_string(),
            };

            let initial = initial.read().to_string();
            let last = last.read().to_string();

            let iter = model.insert_with_values(
                None,
                &[1, 2, 3, 4, 5, 6],
                &[
                    &ty,
                    &pointer,
                    &initial,
                    &last.to_string(),
                    &last,
                    &self.settings.default_cell_background.to_value(),
                ],
            );

            model.set_value(&iter, 0, &(index as u64).to_value());
        }

        self.visible = Some(Visible {
            scan: scan::Scan {
                id: scan.id,
                addresses,
                initial,
                last: last.clone(),
            },
            current: last,
        });

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
        let visible = optional!(&slf.visible);
        let last_type = visible.scan.last.ty();
        let mut current = visible.current.clone();
        let addresses = visible.scan.addresses.clone();

        let thread_pool = slf.thread_pool.clone();
        let results_generation = slf.results_generation;
        let filter_expr = slf.filter_expr.clone();
        let value_expr = slf.value_expr.clone();

        let task = cascade! {
            task::Task::oneshot(slf_rc, move |c| {
                let handle = match handle.try_read() {
                    Some(handle) => handle,
                    None => return Ok(None),
                };

                let value_type = match filter_expr {
                    Some(filter_expr) => filter_expr.value_type_of(&*handle, TypeHint::NoHint)?.into_implicit(),
                    None => TypeHint::NoHint,
                };

                let value_type = value_expr.value_type_of(&*handle, TypeHint::NoHint)?.solve(value_type)?;
                let value_type = value_type.option().unwrap_or(last_type).unsize(last_type);

                // NB: need to convert the value storage in case current type differs.
                let type_change = if value_type != current.ty() {
                    current.convert_in_place(&*handle, value_type);
                    true
                } else {
                    false
                };

                handle.refresh_values(
                    &*thread_pool,
                    &addresses,
                    &mut current,
                    Some(c.as_token()),
                    NoopProgress,
                    &value_expr,
                )?;

                Ok(Some((current, type_change)))
            });
            ..then(move |slf, values| {
                if let Ok(Some((values, type_change))) = values {
                    if type_change {
                        if let Some(visible) = &mut slf.visible {
                            visible.current = values;
                        }

                        slf.refresh_visible();
                    } else {
                        slf.refresh_visible_diff(results_generation, values);
                    }
                }

                slf.refresh_task = None;
            });
        };

        slf.refresh_task = Some(task.run());
    }

    /// Refresh the visible results if the differ from the stored ones.
    fn refresh_visible_diff(&mut self, results_generation: usize, values: Values) {
        if self.results_generation != results_generation {
            return;
        }

        let model = upgrade!(self.widgets.model);
        let visible = optional!(&mut self.visible);

        if visible.current.ty() != values.ty() {
            return;
        }

        for (index, (last, (mut prev, new))) in visible
            .scan
            .last
            .iter()
            .zip(visible.current.iter_mut().zip(values.iter()))
            .enumerate()
        {
            {
                let prev = prev.read();
                let new = new.read();

                if prev == new {
                    continue;
                }

                if let Some(iter) = model.iter_nth_child(None, index as i32) {
                    model.set_value(&iter, 5, &new.to_string().to_value());

                    let background = if new == last.read() {
                        &self.settings.default_cell_background
                    } else {
                        &self.settings.highlight_cell_background
                    };

                    model.set_value(&iter, 6, &background.to_value());
                }
            }

            prev.write(new.read());
        }
    }

    /// Refresh all visible values under the assumption that they have changed.
    fn refresh_visible(&mut self) {
        let model = upgrade!(self.widgets.model);
        let visible = optional!(&self.visible);

        for (index, (last, current)) in visible
            .scan
            .last
            .iter()
            .zip(visible.current.iter())
            .enumerate()
        {
            {
                let current = current.read();

                if let Some(iter) = model.iter_nth_child(None, index as i32) {
                    model.set_value(&iter, 5, &current.to_string().to_value());

                    let background = if current == last.read() {
                        &self.settings.default_cell_background
                    } else {
                        &self.settings.highlight_cell_background
                    };

                    model.set_value(&iter, 6, &background.to_value());
                }
            }
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
