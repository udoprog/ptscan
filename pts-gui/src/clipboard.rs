use crate::MemoryInfo;
use ptscan::{ModuleInfo, ProcessThread, ScanResult};
use serde::{Deserialize, Serialize};
use std::{
    cell::{Ref, RefCell},
    rc::{Rc, Weak},
};

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type", content = "value", rename_all = "snake_case")]
pub enum ClipboardBuffer {
    String(String),
    Modules(Vec<ModuleInfo>),
    Threads(Vec<ProcessThread>),
    MemoryList(Vec<MemoryInfo>),
    Results(Vec<Box<ScanResult>>),
}

struct Provider {
    id: usize,
    /// Callback to get a clipboard value from the widget.
    get: Box<dyn Fn() -> Option<ClipboardBuffer>>,
    /// Callback to set a value from the clipboard buffer to the widget.
    set: Box<dyn Fn(Option<&ClipboardBuffer>)>,
}

struct InnerClipboardHandle {
    id: usize,
    current: Weak<RefCell<Option<Provider>>>,
}

impl Drop for InnerClipboardHandle {
    fn drop(&mut self) {
        let current = match self.current.upgrade() {
            Some(current) => current,
            None => return,
        };

        let mut current = current.borrow_mut();

        if let Some(current) = &*current {
            if current.id != self.id {
                return;
            }
        }

        current.take();
    }
}

#[derive(Clone)]
pub struct ClipboardHandle {
    alloc: Rc<RefCell<usize>>,
    current: Rc<RefCell<Option<ClipboardBuffer>>>,
    provider: Weak<RefCell<Option<Provider>>>,
    inner: Rc<RefCell<Option<InnerClipboardHandle>>>,
}

impl ClipboardHandle {
    /// Set the paste provider.
    pub fn set<G, S>(&self, get: G, set: S)
    where
        G: 'static + Fn() -> Option<ClipboardBuffer>,
        S: 'static + Fn(Option<&ClipboardBuffer>),
    {
        let id = {
            let mut alloc = self.alloc.borrow_mut();
            let id = *alloc;
            *alloc += 1;
            id
        };

        let provider = match self.provider.upgrade() {
            Some(provider) => provider,
            None => return,
        };

        *provider.borrow_mut() = Some(Provider {
            id,
            get: Box::new(get),
            set: Box::new(set),
        });

        *self.inner.borrow_mut() = Some(InnerClipboardHandle {
            id,
            current: Rc::downgrade(&provider),
        });
    }

    /// Hook up the specified tree.
    pub fn hook_tree<G>(&self, tree: &gtk::TreeView, grab: G)
    where
        G: 'static + Clone + Fn(&gtk::TreeSelection) -> Option<ClipboardBuffer>,
    {
        use gtk::prelude::*;

        let handle = self;

        tree.connect_focus_in_event(clone!(handle => move |tree, _| {
            let tree = tree.downgrade();

            handle.set(
                clone!(grab, tree => move || {
                    let tree = tree.upgrade()?;
                    let selection = tree.get_selection();
                    grab(&selection)
                }),
                move |_| {

                },
            );

            Inhibit(false)
        }));
    }

    /// Hook an entry into the rich clipboard.
    pub fn hook_entry(&self, entry: &gtk::Entry) {
        use gtk::prelude::*;

        let handle = self;

        entry.connect_focus_in_event(clone!(handle => move |entry, _| {
            let entry = entry.downgrade();

            handle.set(
                clone!(entry => move || {
                    let entry = entry.upgrade()?;
                    let (start, end) = entry.get_selection_bounds()?;
                    let text = entry.get_chars(start, end)?;
                    Some(ClipboardBuffer::String(text.as_str().to_string()))
                }),
                clone!(entry => move |buffer| {
                    let entry = upgrade!(entry);
                    entry.delete_selection();

                    if let Some(ClipboardBuffer::String(string)) = buffer {
                        let mut pos = entry.get_position();
                        entry.insert_text(string, &mut pos);
                        entry.set_position(pos);
                    }
                }),
            );

            Inhibit(false)
        }));

        // entry.get_selection()
    }

    /// Clear the current handle.
    pub fn clear(&self) {
        let _ = self.inner.borrow_mut().take();
    }
}

pub struct Clipboard {
    alloc: Rc<RefCell<usize>>,
    current: Rc<RefCell<Option<ClipboardBuffer>>>,
    provider: Rc<RefCell<Option<Provider>>>,
}

impl Clipboard {
    pub fn new() -> Self {
        Self {
            alloc: Rc::new(RefCell::new(0)),
            current: Rc::new(RefCell::new(None)),
            provider: Rc::new(RefCell::new(None)),
        }
    }

    /// Build a new clipboard handle.
    pub fn handle(&self) -> ClipboardHandle {
        ClipboardHandle {
            alloc: self.alloc.clone(),
            current: self.current.clone(),
            provider: Rc::downgrade(&self.provider),
            inner: Default::default(),
        }
    }

    /// Get the current data for the paste provider.
    pub fn get(&self) -> Ref<'_, Option<ClipboardBuffer>> {
        let provider = self.provider.borrow();

        let buffer = match &*provider {
            Some(provider) => (provider.get)(),
            None => None,
        };

        *self.current.borrow_mut() = buffer;
        self.current.borrow()
    }

    /// Call the current paste handler.
    pub fn paste(&self, buffer: Option<&ClipboardBuffer>) {
        let provider = self.provider.borrow();

        if let Some(provider) = &*provider {
            (provider.set)(buffer);
        };
    }
}
