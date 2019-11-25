use self::Orientation::*;
use crate::prelude::*;
use parking_lot::RwLock;
use ptscan::{ProcessHandle, Scan, ScanResult, ValueExpr};
use std::{cell::RefCell, rc::Rc, sync::Arc};

struct Widgets {
    model: glib::WeakRef<ListStore>,
    tree: glib::WeakRef<TreeView>,
    context_add_to_work: glib::WeakRef<MenuItem>,
}

pub struct ScanResults {
    scan: Arc<RwLock<Scan>>,
    thread_pool: Arc<rayon::ThreadPool>,
    widgets: Widgets,
    visible_results: Option<Vec<Box<ScanResult>>>,
    timer: Option<glib::SourceId>,
    refresh_task: Option<task::Handle>,
    handle: Option<Arc<ProcessHandle>>,
    /// Keep track of the generation of result being used since it might change
    /// during a refresh.
    results_generation: usize,
    /// Handler to call when it is requested that we add a result.
    on_add_results: Option<Box<dyn Fn(&Self, &Box<ScanResult>)>>,
    context_menu: Menu,
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
        scan: Arc<RwLock<Scan>>,
        thread_pool: Arc<rayon::ThreadPool>,
    ) -> (Rc<RefCell<Self>>, ScrolledWindow) {
        let model = ListStore::new(&[
            String::static_type(),
            String::static_type(),
            String::static_type(),
            String::static_type(),
            String::static_type(),
        ]);

        let tree = TreeView::new();

        let context_add_to_work = cascade! {
            MenuItem::new();
            ..add(&cascade! {
                gtk::Box::new(Horizontal, 0);
                ..pack_start(&Image::new_from_icon_name(Some("list-add"), IconSize::Menu), false, false, 0);
                ..pack_start(&Label::new(Some("Add to work area")), true, true, 0);
            });
        };

        let context_menu = cascade! {
            Menu::new();
            ..append(&context_add_to_work);
            ..show_all();
        };

        let scan_results = Rc::new(RefCell::new(Self {
            scan,
            thread_pool,
            widgets: Widgets {
                model: model.downgrade(),
                tree: tree.downgrade(),
                context_add_to_work: context_add_to_work.downgrade(),
            },
            visible_results: None,
            timer: None,
            refresh_task: None,
            handle: None,
            results_generation: 0,
            on_add_results: None,
            context_menu,
        }));

        context_add_to_work.connect_activate(clone!(scan_results => move |_| {
            let scan_results = scan_results.borrow();
            let tree = upgrade_weak!(scan_results.widgets.tree);

            let on_add_results = optional!(&scan_results.on_add_results);
            let visible_results = optional!(&scan_results.visible_results);

            let (selected, _) = tree.get_selection().get_selected_rows();

            for s in selected {
                let index = match s.get_indices().into_iter().next() {
                    Some(index) => index as usize,
                    None => continue,
                };

                if let Some(result) = visible_results.get(index) {
                    on_add_results(&*scan_results, result);
                }
            }
        }));

        scan_results.borrow_mut().timer = Some(glib::source::timeout_add_local(
            500,
            clone!(scan_results => move || {
                Self::refresh_current(&scan_results);
                glib::Continue(true)
            }),
        ));

        let type_cell = CellRendererText::new();
        let pointer_cell = CellRendererText::new();
        let value_cell = CellRendererText::new();
        let last_cell = CellRendererText::new();
        let current_cell = CellRendererText::new();

        let tree = cascade! {
            tree;
            ..set_headers_visible(true);
            ..append_column(&cascade! {
                TreeViewColumn::new();
                    ..set_title("type");
                    ..pack_start(&type_cell, true);
                    ..add_attribute(&type_cell, "text", 0);
            });
            ..append_column(&cascade! {
                TreeViewColumn::new();
                    ..set_title("address");
                    ..pack_start(&pointer_cell, false);
                    ..add_attribute(&pointer_cell, "text", 1);
            });
            ..append_column(&cascade! {
                TreeViewColumn::new();
                    ..set_title("initial");
                    ..pack_start(&value_cell, true);
                    ..add_attribute(&value_cell, "text", 2);
            });
            ..append_column(&cascade! {
                TreeViewColumn::new();
                    ..set_title("last");
                    ..pack_start(&last_cell, true);
                    ..add_attribute(&last_cell, "text", 3);
            });
            ..append_column(&cascade! {
                TreeViewColumn::new();
                    ..set_title("value");
                    ..pack_start(&current_cell, true);
                    ..add_attribute(&current_cell, "text", 4);
            });
            ..connect_row_activated(clone!(scan_results => move |_, path, _| {
                let scan_results = scan_results.borrow();

                if let Some(on_add_results) = &scan_results.on_add_results {
                    let visible_results = optional!(&scan_results.visible_results);
                    let index = optional!(path.get_indices().into_iter().next());
                    let result = optional!(visible_results.get(index as usize));
                    on_add_results(&*scan_results, result);
                }
            }));
            ..connect_button_press_event(clone!(scan_results => move |_, e| {
                match (e.get_event_type(), e.get_button()) {
                    (EventType::ButtonPress, 3) => {
                        let scan_results = scan_results.borrow();

                        let tree = upgrade_weak!(scan_results.widgets.tree, Inhibit(false));
                        let context_add_to_work = upgrade_weak!(scan_results.widgets.context_add_to_work, Inhibit(false));

                        if tree.get_selection().count_selected_rows() == 0 {
                            context_add_to_work.set_sensitive(false);
                        } else {
                            context_add_to_work.set_sensitive(true);
                        }

                        scan_results.context_menu.popup_at_pointer(Some(&e));
                        scan_results.context_menu.show();
                        Inhibit(true)
                    }
                    _ => Inhibit(false),
                }
            }));
            ..get_selection().set_mode(SelectionMode::Multiple);
            ..set_model(Some(&model));
            ..show_all();
        };

        let scrolled_window = cascade! {
            ScrolledWindow::new(gtk::NONE_ADJUSTMENT, gtk::NONE_ADJUSTMENT);
            ..set_policy(gtk::PolicyType::Automatic, gtk::PolicyType::Automatic);
            ..add(&tree);
            ..show();
        };

        (scan_results, scrolled_window)
    }

    /// Request when the component wants a result to be added to the scratch pad.
    pub fn on_add_results<T>(&mut self, on_add_results: T)
    where
        T: 'static + Fn(&Self, &Box<ScanResult>),
    {
        self.on_add_results = Some(Box::new(on_add_results));
    }

    /// Refresh the current set of results.
    pub fn refresh_current(scan_results: &Rc<RefCell<Self>>) {
        let mut slf = scan_results.borrow_mut();

        // do not refresh
        if slf.refresh_task.is_some() {
            return;
        }

        let handle = optional!(slf.handle.clone());
        let mut new_results = optional!(slf.visible_results.clone());
        let thread_pool = slf.thread_pool.clone();
        let results_generation = slf.results_generation;

        let task = cascade! {
            task::Task::oneshot(scan_results, move |c| {
                handle.refresh_values(
                    &*thread_pool,
                    &mut new_results,
                    Some(c.as_token()),
                    None,
                    NoopProgress,
                    &ValueExpr::Value,
                )?;

                Ok(new_results)
            });
            ..then(move |scan_results, new_results| {
                if let Ok(new_results) = new_results {
                    scan_results.refresh_visible_diff(results_generation, new_results);
                }

                scan_results.refresh_task = None;
            });
        };

        slf.refresh_task = Some(task.run());
    }

    /// Refresh the collection of scan results being showed.
    pub fn refresh(&mut self) {
        let scan = optional!(self.scan.try_read());
        let model = upgrade_weak!(self.widgets.model);

        model.clear();

        let visible = scan.results.iter().take(1000).cloned().collect::<Vec<_>>();

        for result in &visible {
            let last = result.last();
            let ty = last.ty().to_string();
            let pointer = result.pointer.to_string();
            let initial = result.initial.to_string();
            let last = result.last().to_string();

            model.insert_with_values(
                None,
                &[0, 1, 2, 3, 4],
                &[&ty, &pointer, &initial, &last, &last],
            );
        }

        self.visible_results = Some(visible);
        self.results_generation += 1;
    }

    /// Refresh the visible results if the differ from the stored ones.
    pub fn refresh_visible_diff(
        &mut self,
        results_generation: usize,
        new_results: Vec<Box<ScanResult>>,
    ) {
        if self.results_generation != results_generation {
            return;
        }

        let model = upgrade_weak!(self.widgets.model);

        let visible_results = optional!(&mut self.visible_results);

        for (index, (current, update)) in visible_results.iter_mut().zip(new_results).enumerate() {
            if current.last() == update.last() {
                continue;
            }

            let value = update.last().to_string();

            if let Some(iter) = model.iter_nth_child(None, index as i32) {
                model.set_value(&iter, 4, &value.to_value());
            }

            *current = update;
        }
    }

    /// Set the current process handle.
    pub fn set_handle(&mut self, handle: Option<Arc<ProcessHandle>>) {
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
