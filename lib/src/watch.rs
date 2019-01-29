//! Watch expression that watches a single location in memory.

use crate::{
    address::{Address, Offset},
    scan::{Type, Value},
};

#[derive(Debug)]
pub struct Watch {
    /// Location being watched.
    pub location: WatchLocation,
    /// The current value of the watch.
    pub value: Value,
    /// The type the location is being treated as.
    pub ty: Type,
}

/// An address in the process.
#[derive(Debug)]
pub enum WatchAddress {
    Absolute(Address),
    Relative { base_module: String, offset: Offset },
}

/// The location of the watch.
#[derive(Debug)]
pub enum WatchLocation {
    Address(WatchAddress),
}
