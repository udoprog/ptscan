pub use crate::{
    error_handler::ErrorHandler,
    signal::{Block, BlockExt as _},
    task, ui, Clipboard, ClipboardBuffer, ClipboardBufferRef, ClipboardHandle, Settings,
};
pub use cascade::cascade;
pub use gdk::EventType;
pub use gio::prelude::*;
pub use gtk::prelude::*;
pub use gtk::{
    AboutDialog, AccelFlags, AccelGroup, Align, ApplicationWindow, Button, CellRendererText,
    CheckButton, Entry, Frame, IconSize, Image, Label, ListStore, Menu, MenuBar, MenuItem,
    Orientation, ProgressBar, ScrolledWindow, SelectionMode, Spinner, Statusbar, TreeModelFilter,
    TreeView, TreeViewColumn, Viewport, Window, WindowPosition, WindowType,
};
pub use pango::prelude::*;

use std::borrow::Cow;

#[derive(rust_embed::RustEmbed)]
#[folder = "$CARGO_MANIFEST_DIR/res"]
struct Resources;

pub struct Builder(&'static str, gtk::Builder);

impl Builder {
    /// Get the specified object, panicking if it doesn't exist with some
    /// diagnostics.
    pub fn get_object<T>(&self, name: &str) -> T
    where
        T: IsA<glib::Object>,
    {
        match self.1.get_object(name) {
            Some(object) => object,
            None => panic!("failed to get object: {}:{}", self.0, name),
        }
    }
}

pub struct Resource(&'static str, Cow<'static, [u8]>);

impl Resource {
    /// Treat resource as a string.
    pub fn as_str(&self) -> &str {
        std::str::from_utf8(&*self.1).expect("resource is not valid utf-8")
    }

    /// Convert resource into a builder.
    pub fn into_builder(&self) -> Builder {
        let builder = gtk::Builder::new_from_string(self.as_str());
        Builder(self.0, builder)
    }
}

/// Access the given resource by path.
pub fn resource(path: &'static str) -> Resource {
    match Resources::get(path) {
        Some(resource) => Resource(path, resource),
        None => panic!("missing resource: {}", path),
    }
}
