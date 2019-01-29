//! Watch expression that watches a single location in memory.

use crate::{
    pointer::Pointer,
    scan::{Type, Value},
};

#[derive(Debug)]
pub struct Watch {
    /// Location being watched.
    pub pointer: Pointer,
    /// The current value of the watch.
    pub value: Value,
    /// The type the location is being treated as.
    pub ty: Type,
}
