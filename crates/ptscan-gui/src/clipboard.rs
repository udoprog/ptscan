use crate::{CurrentScanResult, MemoryInfo, ScanResult};
use ptscan::{ModuleInfo, ProcessThread};
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    fmt,
    rc::{Rc, Weak},
};

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type", content = "value", rename_all = "snake_case")]
pub enum ClipboardBuffer {
    String(String),
    Modules(Vec<ModuleInfo>),
    Threads(Vec<ProcessThread>),
    MemoryList(Vec<MemoryInfo>),
    Results(Vec<ScanResult>),
    CurrentResults(Vec<CurrentScanResult>),
}

impl fmt::Display for ClipboardBuffer {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(string) => string.fmt(fmt)?,
            Self::Modules(modules) => {
                for module in modules {
                    writeln!(fmt, "{}", module)?;
                }
            }
            Self::Threads(threads) => {
                for thread in threads {
                    writeln!(fmt, "{}", thread)?;
                }
            }
            Self::MemoryList(list) => {
                for m in list {
                    writeln!(fmt, "{}", m)?;
                }
            }
            Self::Results(results) => {
                for result in results {
                    writeln!(fmt, "{}", result)?;
                }
            }
            Self::CurrentResults(results) => {
                for result in results {
                    writeln!(fmt, "{}", result)?;
                }
            }
        }

        Ok(())
    }
}

// Serialization structure for ClipboardBuffer which can serialize while borrowing.
#[derive(Debug, Serialize)]
#[serde(tag = "type", content = "value", rename_all = "snake_case")]
pub enum ClipboardBufferRef<'a> {
    String(&'a str),
}

pub enum OnCopy {
    None,
    Rich(Box<dyn Fn() -> Option<ClipboardBuffer>>),
    Text(Box<dyn Fn() -> Option<String>>),
}

pub enum Copied {
    None,
    Rich(ClipboardBuffer),
    Text(String),
}

pub enum OnPaste {
    None,
    Rich(Rc<dyn Fn(&ClipboardBuffer)>),
    Text(Rc<dyn Fn(&str)>),
}

pub struct Paster<Rich, Text>
where
    Rich: 'static + Fn(Weak<dyn Fn(&ClipboardBuffer)>),
    Text: 'static + Fn(Weak<dyn Fn(&str)>),
{
    pub rich: Rich,
    pub text: Text,
}

struct Provider {
    name: &'static str,
    /// Callback to get a clipboard value from the widget.
    on_copy: OnCopy,
    /// Callback to set a value from the clipboard buffer to the widget.
    on_paste: OnPaste,
}

struct InnerClipboardHandle {
    name: &'static str,
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
            // NB: only deallocate on drop of the current provider is the one that was dropped.
            if current.name != self.name {
                return;
            }
        }

        current.take();
    }
}

pub struct ProviderBuilder {
    provider: Rc<RefCell<Option<Provider>>>,
}

impl ProviderBuilder {
    /// Set the callback to use to copy a value into the clipboard.
    pub fn on_copy_rich<T>(&mut self, on_copy: T)
    where
        T: 'static + Fn() -> Option<ClipboardBuffer>,
    {
        if let Some(provider) = &mut *self.provider.borrow_mut() {
            provider.on_copy = OnCopy::Rich(Box::new(on_copy));
        }
    }

    /// Set the callback to use to copy a value into the clipboard.
    pub fn on_copy_text<T>(&mut self, on_copy: T)
    where
        T: 'static + Fn() -> Option<String>,
    {
        if let Some(provider) = &mut *self.provider.borrow_mut() {
            provider.on_copy = OnCopy::Text(Box::new(on_copy));
        }
    }

    /// Set the callback to call when the clipboard value is pasted.
    pub fn on_paste_rich<T>(&mut self, on_paste: T)
    where
        T: 'static + Fn(&ClipboardBuffer),
    {
        if let Some(provider) = &mut *self.provider.borrow_mut() {
            provider.on_paste = OnPaste::Rich(Rc::new(on_paste));
        }
    }

    /// Set the callback to call when the clipboard value is pasted.
    pub fn on_paste_text<T>(&mut self, on_paste: T)
    where
        T: 'static + Fn(&str),
    {
        if let Some(provider) = &mut *self.provider.borrow_mut() {
            provider.on_paste = OnPaste::Text(Rc::new(on_paste));
        }
    }
}

#[derive(Clone)]
pub struct ClipboardHandle {
    name: &'static str,
    provider: Weak<RefCell<Option<Provider>>>,
    inner: Rc<RefCell<Option<InnerClipboardHandle>>>,
}

impl ClipboardHandle {
    /// Set the paste provider.
    pub fn new(&self) -> Option<ProviderBuilder> {
        let provider = self.provider.upgrade()?;

        *self.inner.borrow_mut() = Some(InnerClipboardHandle {
            name: self.name,
            current: Rc::downgrade(&provider),
        });

        *provider.borrow_mut() = Some(Provider {
            name: self.name,
            on_copy: OnCopy::None,
            on_paste: OnPaste::None,
        });

        Some(ProviderBuilder { provider })
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

            let mut p = optional!(handle.new(), Inhibit(false));

            p.on_copy_rich(
                clone!(grab, tree => move || {
                    let tree = tree.upgrade()?;
                    let selection = tree.get_selection();
                    grab(&selection)
                })
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

            let mut p = optional!(handle.new(), Inhibit(false));

            p.on_copy_text(clone!(entry => move || {
                let entry = entry.upgrade()?;
                let (start, end) = entry.get_selection_bounds()?;
                let text = entry.get_chars(start, end)?;
                Some(text.as_str().to_string())
            }));

            p.on_paste_text(clone!(entry => move |text| {
                let entry = upgrade!(entry);
                entry.delete_selection();
                let mut pos = entry.get_position();
                entry.insert_text(text, &mut pos);
                entry.set_position(pos);
            }));

            Inhibit(false)
        }));
    }

    /// Clear the current handle.
    pub fn clear(&self) {
        let _ = self.inner.borrow_mut().take();
    }
}

pub struct Clipboard {
    provider: Rc<RefCell<Option<Provider>>>,
}

impl Clipboard {
    pub fn new() -> Self {
        Self {
            provider: Rc::new(RefCell::new(None)),
        }
    }

    /// Build a new clipboard handle.
    pub fn handle(&self, name: &'static str) -> ClipboardHandle {
        ClipboardHandle {
            name,
            provider: Rc::downgrade(&self.provider),
            inner: Default::default(),
        }
    }

    /// Get the current data for the paste provider.
    pub fn copy(&self) -> Copied {
        let provider = self.provider.borrow();

        if let Some(provider) = &*provider {
            match &provider.on_copy {
                OnCopy::None => (),
                OnCopy::Rich(rich) => {
                    return Copied::Rich(optional!(rich(), Copied::None));
                }
                OnCopy::Text(text) => {
                    return Copied::Text(optional!(text(), Copied::None));
                }
            }
        }

        Copied::None
    }

    /// Call the current paste handler.
    pub fn paste<Rich, Text>(&self, paster: &Paster<Rich, Text>)
    where
        Rich: 'static + Fn(Weak<dyn Fn(&ClipboardBuffer)>),
        Text: 'static + Fn(Weak<dyn Fn(&str)>),
    {
        let provider = self.provider.borrow();

        let provider = match provider.as_ref() {
            Some(provider) => provider,
            None => return,
        };

        match &provider.on_paste {
            OnPaste::None => (),
            OnPaste::Rich(cb) => {
                (paster.rich)(Rc::downgrade(cb));
            }
            OnPaste::Text(cb) => {
                (paster.text)(Rc::downgrade(cb));
            }
        }
    }
}
