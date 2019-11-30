use ptscan::{
    Address, Base, FollowablePointer, PortablePointer, ProcessHandle, Type, Value, ValueHolder,
    ValueRef, Values,
};
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScanResult {
    pub pointer: PortablePointer,
    pub last_address: Option<Address>,
    pub initial_type: Type,
    pub initial: Value,
    pub last_type: Type,
    pub last: Value,
}

impl fmt::Display for ScanResult {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt,
            "{} {} {} {} {}",
            self.pointer, self.initial_type, self.initial, self.last_type, self.last
        )
    }
}

impl ValueHolder for ScanResult {
    type Pointer = PortablePointer;

    fn pointer(&self) -> &Self::Pointer {
        &self.pointer
    }

    fn initial_type(&self) -> Type {
        self.initial_type
    }

    fn initial(&self) -> ValueRef<'_> {
        self.initial.as_ref()
    }

    fn last_type(&self) -> Type {
        self.last_type
    }

    fn last(&self) -> ValueRef<'_> {
        self.last.as_ref()
    }

    fn insert(&mut self, value: Value) {
        self.last = value;
    }
}

pub struct Scan {
    pub bases: Vec<Base>,
    pub initial: Values,
    pub last: Values,
}

impl Scan {
    /// Get a single scan result.
    pub fn get(&self, handle: &ProcessHandle, index: usize) -> Option<ScanResult> {
        let base = self.bases.get(index)?;
        let last_address = base.follow_default(handle).ok().and_then(|v| v);
        let base = base.as_portable(handle)?;
        let pointer = PortablePointer::new(base, std::iter::empty());

        Some(ScanResult {
            pointer,
            last_address,
            initial_type: self.initial.ty,
            initial: self.initial.get(index)?,
            last_type: self.last.ty,
            last: self.last.get(index)?,
        })
    }

    /// Get the number of results in this scan.
    pub fn len(&self) -> usize {
        self.bases.len()
    }

    /// Remove the given index.
    pub fn swap_remove(&mut self, index: usize) -> bool {
        // NB: nothing to remove.
        if index >= self.bases.len() {
            return false;
        }

        self.bases.swap_remove(index);
        self.initial.swap_remove(index);
        self.last.swap_remove(index);
        return true;
    }
}
