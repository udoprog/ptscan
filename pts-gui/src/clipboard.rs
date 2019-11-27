use crate::MemoryInfo;
use glib::ObjectExt as _;
use gtk::TreeSelection;
use ptscan::{ModuleInfo, ProcessThread, ScanResult};
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    rc::{Rc, Weak},
};

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type", content = "value", rename_all = "snake_case")]
pub enum ClipboardBuffer {
    Module(ModuleInfo),
    Modules(Vec<ModuleInfo>),
    Thread(ProcessThread),
    Threads(Vec<ProcessThread>),
    Memory(MemoryInfo),
    MemoryList(Vec<MemoryInfo>),
    Result(Box<ScanResult>),
    Results(Vec<Box<ScanResult>>),
}

struct Provider {
    id: usize,
    callback: Box<dyn Fn() -> Option<ClipboardBuffer>>,
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

pub struct ClipboardHandle(Option<InnerClipboardHandle>);

impl ClipboardHandle {
    /// Clear the current handle.
    pub fn clear(&mut self) {
        let _ = self.0.take();
    }
}

impl Default for ClipboardHandle {
    fn default() -> Self {
        ClipboardHandle(None)
    }
}

pub struct Clipboard {
    alloc: RefCell<usize>,
    provider: Rc<RefCell<Option<Provider>>>,
}

impl Clipboard {
    pub fn new() -> Self {
        Self {
            alloc: RefCell::new(0),
            provider: Rc::new(RefCell::new(None)),
        }
    }

    /// Create a selection manager from a selection.
    pub fn from_selection<T>(&self, selection: &TreeSelection, callback: T) -> ClipboardHandle
    where
        T: 'static + Fn(&TreeSelection) -> Option<ClipboardBuffer>,
    {
        let selection = selection.downgrade();

        self.set(move || {
            let selection = match selection.upgrade() {
                Some(selection) => selection,
                None => return None,
            };

            callback(&selection)
        })
    }

    /// Set the paste provider.
    pub fn set<T>(&self, provider: T) -> ClipboardHandle
    where
        T: 'static + Fn() -> Option<ClipboardBuffer>,
    {
        let id = {
            let mut alloc = self.alloc.borrow_mut();
            let id = *alloc;
            *alloc += 1;
            id
        };

        *self.provider.borrow_mut() = Some(Provider {
            id,
            callback: Box::new(provider),
        });

        ClipboardHandle(Some(InnerClipboardHandle {
            id,
            current: Rc::downgrade(&self.provider),
        }))
    }

    /// Get the current data for the paste provider.
    pub fn get(&self) -> Option<ClipboardBuffer> {
        let provider = self.provider.borrow();

        match &*provider {
            Some(provider) => (provider.callback)(),
            None => None,
        }
    }
}
