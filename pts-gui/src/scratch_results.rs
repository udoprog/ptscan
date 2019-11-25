use self::Orientation::*;
use crate::{prelude::*, ScanResultDialog};
use ptscan::{Pointer, ProcessHandle, ScanResult, Value, ValueExpr};
use std::{cell::RefCell, rc::Rc, sync::Arc};

struct Widgets {
    model: glib::WeakRef<ListStore>,
    tree: glib::WeakRef<TreeView>,
    context_remove_item: glib::WeakRef<MenuItem>,
    context_edit_item: glib::WeakRef<MenuItem>,
    scan_result_dialog_window: glib::WeakRef<Window>,
}

#[derive(Default)]
struct State {
    results: Vec<Box<ScanResult>>,
}

pub struct ScratchResults {
    thread_pool: Arc<rayon::ThreadPool>,
    state: State,
    widgets: Widgets,
    timer: Option<glib::SourceId>,
    refresh_task: Option<task::Handle>,
    handle: Option<Arc<ProcessHandle>>,
    /// Keep track of the generation of result being used since it might change
    /// during a refresh.
    results_generation: usize,
    /// Reference to context menu to keep it alive.
    context_menu: Menu,
    /// Menu for editing scan results.
    scan_result_dialog: Rc<RefCell<ScanResultDialog>>,
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
        thread_pool: Arc<rayon::ThreadPool>,
        scan_result_dialog: Rc<RefCell<ScanResultDialog>>,
        scan_result_dialog_window: glib::WeakRef<Window>,
    ) -> (Rc<RefCell<Self>>, ScrolledWindow) {
        let model = ListStore::new(&[
            String::static_type(),
            String::static_type(),
            String::static_type(),
            String::static_type(),
            String::static_type(),
        ]);

        let tree = TreeView::new();

        let context_remove_item = cascade! {
            MenuItem::new();
            ..add(&cascade! {
                gtk::Box::new(Horizontal, 0);
                ..pack_start(&Image::new_from_icon_name(Some("list-remove"), IconSize::Menu), false, false, 0);
                ..pack_start(&Label::new(Some("Remove")), true, true, 0);
            });
        };

        let context_edit_item = cascade! {
            MenuItem::new();
            ..add(&cascade! {
                gtk::Box::new(Horizontal, 0);
                ..pack_start(&Image::new_from_icon_name(Some("list-edit"), IconSize::Menu), false, false, 0);
                ..pack_start(&Label::new(Some("Edit")), true, true, 0);
            });
        };

        let context_add_item = cascade! {
            MenuItem::new();
            ..add(&cascade! {
                gtk::Box::new(Horizontal, 0);
                ..pack_start(&Image::new_from_icon_name(Some("list-add"), IconSize::Menu), false, false, 0);
                ..pack_start(&Label::new(Some("Add")), true, true, 0);
            });
        };

        let context_menu = cascade! {
            Menu::new();
            ..append(&context_remove_item);
            ..append(&context_edit_item);
            ..append(&context_add_item);
            ..show_all();
        };

        let scratch_results = Rc::new(RefCell::new(Self {
            thread_pool,
            widgets: Widgets {
                model: model.downgrade(),
                context_remove_item: context_remove_item.downgrade(),
                context_edit_item: context_edit_item.downgrade(),
                tree: tree.downgrade(),
                scan_result_dialog_window,
            },
            state: State::default(),
            timer: None,
            refresh_task: None,
            handle: None,
            results_generation: 0,
            context_menu,
            scan_result_dialog,
        }));

        context_remove_item.connect_activate(clone!(scratch_results => move |_| {
            let mut scratch_results = scratch_results.borrow_mut();
            let tree = upgrade_weak!(scratch_results.widgets.tree);
            let model = optional!(scratch_results.widgets.model.upgrade());

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
                scratch_results.state.results.swap_remove(index);
            }

            scratch_results.results_generation += 1;
        }));

        context_edit_item.connect_activate(clone!(scratch_results => move |_| {
            let tree = upgrade_weak!(scratch_results.borrow().widgets.tree);
            let path = optional!(tree.get_selection().get_selected_rows().0.into_iter().next());
            let index = optional!(path.get_indices().into_iter().next()) as usize;
            Self::open_result_editor(&scratch_results, index);
        }));

        context_add_item.connect_activate(clone!(scratch_results => move |_| {
            let slf = scratch_results.borrow();

            {
                let mut scan_result_dialog = slf.scan_result_dialog.borrow_mut();

                scan_result_dialog.set_result(Box::new(ScanResult::new(Pointer::null(), Value::default())));

                scan_result_dialog.on_save(clone!(scratch_results => move |_, result| {
                    let mut scratch_results = scratch_results.borrow_mut();
                    scratch_results.add_result(&result);
                }));
            }

            let scan_result_dialog_window = upgrade_weak!(slf.widgets.scan_result_dialog_window);
            scan_result_dialog_window.show();
            scan_result_dialog_window.present();
        }));

        scratch_results.borrow_mut().timer = Some(glib::source::timeout_add_local(
            500,
            clone!(scratch_results => move || {
                Self::refresh_current(&scratch_results);
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
            ..set_model(Some(&model));
            ..get_selection().set_mode(SelectionMode::Multiple);
            ..connect_row_activated(clone!(scratch_results => move |_, path, _| {
                let index = optional!(path.get_indices().into_iter().next()) as usize;
                Self::open_result_editor(&scratch_results, index);
            }));
            ..connect_button_press_event(clone!(scratch_results => move |_, e| {
                match (e.get_event_type(), e.get_button()) {
                    (EventType::ButtonPress, 3) => {
                        let scratch_results = scratch_results.borrow();
                        let tree = upgrade_weak!(scratch_results.widgets.tree, Inhibit(false));
                        let context_remove_item = upgrade_weak!(scratch_results.widgets.context_remove_item, Inhibit(false));
                        let context_edit_item = upgrade_weak!(scratch_results.widgets.context_edit_item, Inhibit(false));

                        if tree.get_selection().count_selected_rows() == 0 {
                            context_remove_item.set_sensitive(false);
                            context_edit_item.set_sensitive(false);
                        } else {
                            context_remove_item.set_sensitive(true);
                            context_edit_item.set_sensitive(true);
                        }

                        scratch_results.context_menu.popup_at_pointer(Some(&e));
                        scratch_results.context_menu.show();
                        Inhibit(true)
                    }
                    _ => Inhibit(false),
                }
            }));
            ..show_all();
        };

        let scrolled_window = cascade! {
            ScrolledWindow::new(gtk::NONE_ADJUSTMENT, gtk::NONE_ADJUSTMENT);
            ..set_policy(gtk::PolicyType::Automatic, gtk::PolicyType::Automatic);
            ..add(&tree);
            ..show();
        };

        (scratch_results, scrolled_window)
    }

    /// Refresh the current set of results.
    pub fn refresh_current(scratch_results: &Rc<RefCell<Self>>) {
        let mut slf = scratch_results.borrow_mut();

        // waiting for refresh already in progress.
        if slf.refresh_task.is_some() {
            return;
        }

        // no results to refresh.
        if slf.state.results.is_empty() {
            return;
        }

        let handle = optional!(slf.handle.clone());

        let mut new_results = slf.state.results.clone();
        let thread_pool = slf.thread_pool.clone();
        let results_generation = slf.results_generation;

        let task = cascade! {
            task::Task::oneshot(scratch_results, move |c| {
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
            ..then(move |scratch_results, new_results| {
                if let Ok(new_results) = new_results {
                    scratch_results.refresh_visible_diff(results_generation, new_results);
                }

                scratch_results.refresh_task = None;
            });
        };

        slf.refresh_task = Some(task.run());
    }

    /// Refresh the collection of scan results being showed.
    pub fn add_result(&mut self, result: &Box<ScanResult>) {
        let model = upgrade_weak!(self.widgets.model);

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

        self.state.results.push(result.clone());
        self.results_generation += 1;
    }

    /// Refresh the collection of scan results being showed.
    pub fn edit_result(
        &mut self,
        results_generation: usize,
        index: usize,
        result: Box<ScanResult>,
    ) {
        if self.results_generation != results_generation {
            return;
        }

        let model = upgrade_weak!(self.widgets.model);

        let last = result.last();
        let ty = last.ty().to_string();
        let pointer = result.pointer.to_string();
        let initial = result.initial.to_string();
        let last = result.last().to_string();

        if let Some(iter) = model.iter_nth_child(None, index as i32) {
            model.set(
                &iter,
                &[0, 1, 2, 3, 4],
                &[&ty, &pointer, &initial, &last, &last],
            );
        }

        if let Some(r) = self.state.results.get_mut(index) {
            *r = result;
        }

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

        for (index, (current, update)) in self.state.results.iter_mut().zip(new_results).enumerate()
        {
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

        let scan_result_dialog_window = upgrade_weak!(slf.widgets.scan_result_dialog_window);
        scan_result_dialog_window.show();
        scan_result_dialog_window.present();
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
