use parking_lot::RwLock;
use ptscan::{
    Address, Addresses, PortablePointer, ProcessHandle, Type, Value, ValueHolder, ValueRef, Values,
};
use serde::{Deserialize, Serialize};
use std::{fmt, sync::Arc};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScanResult {
    pub pointer: PortablePointer,
    pub last_address: Option<Address>,
    pub value_type: Type,
    pub value: Value,
}

impl fmt::Display for ScanResult {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{} {} {}", self.pointer, self.value_type, self.value)
    }
}

impl ValueHolder for ScanResult {
    type Pointer = PortablePointer;

    fn pointer(&self) -> &Self::Pointer {
        &self.pointer
    }

    fn value_type(&self) -> Type {
        self.value_type
    }

    fn value(&self) -> ValueRef<'_> {
        self.value.as_ref()
    }

    fn insert(&mut self, value: Value) {
        self.value = value;
    }
}

#[derive(Clone)]
pub struct Scan {
    pub id: uuid::Uuid,
    pub addresses: Addresses,
    pub initial: Values,
    pub last: Values,
}

impl Scan {
    /// Get a single scan result.
    pub fn get(&self, handle: &ProcessHandle, index: usize) -> Option<ScanResult> {
        let address = self.addresses.get(index)?;
        let pointer = PortablePointer::from(handle.address_to_portable_base(address));
        let last_address = Some(address);

        Some(ScanResult {
            pointer,
            last_address,
            value_type: self.last.ty(),
            value: self.last.get(index)?,
        })
    }

    /// Calculate how many bytes this scan occupies.
    pub fn bytes(&self) -> usize {
        self.addresses.bytes() + self.initial.bytes() + self.last.bytes()
    }

    /// Get the number of results in this scan.
    pub fn len(&self) -> usize {
        self.addresses.len()
    }

    /// Remove the given index.
    pub fn swap_remove(&mut self, index: usize) -> bool {
        // NB: nothing to remove.
        if index >= self.addresses.len() {
            return false;
        }

        self.addresses.swap_remove(index);
        self.initial.swap_remove(index);
        self.last.swap_remove(index);
        return true;
    }

    /// Clear the current scan.
    pub fn clear(&mut self) {
        self.addresses.clear();
        self.initial.clear();
        self.last.clear();
    }
}

/// A single session.
pub struct Session {
    scans: Vec<Arc<RwLock<Scan>>>,
    undone: Vec<Arc<RwLock<Scan>>>,
}

impl Session {
    /// Construct a new session.
    pub fn new() -> Self {
        Self {
            scans: Vec::new(),
            undone: Vec::new(),
        }
    }

    /// Get an Arc for every scan in the session.
    pub fn all(&self) -> Vec<Arc<RwLock<Scan>>> {
        let mut out = Vec::new();
        out.extend(self.scans.iter().cloned());
        out.extend(self.undone.iter().cloned());
        out
    }

    pub fn push(&mut self, scan: Scan) {
        self.scans.push(Arc::new(RwLock::new(scan)));
        self.undone.clear();
    }

    /// Get the last scan.
    pub fn last(&self) -> Option<&Arc<RwLock<Scan>>> {
        self.scans.last()
    }

    /// Undo the last scan.
    pub fn undo(&mut self) {
        if let Some(undone) = self.scans.pop() {
            self.undone.push(undone);
        }
    }

    /// Redo the last scan that was undone.
    pub fn redo(&mut self) {
        if let Some(undone) = self.undone.pop() {
            self.scans.push(undone);
        }
    }

    /// Check if the session is currently empty.
    pub fn is_empty(&self) -> bool {
        self.scans.is_empty()
    }

    /// Check if we can undo the last scan.
    pub fn can_undo(&self) -> bool {
        self.scans.len() > 1
    }

    /// Check if we can redo an action in the session.
    pub fn can_redo(&self) -> bool {
        self.undone.len() > 0
    }

    /// Clear all scans.
    pub fn clear(&mut self) {
        self.scans.clear();
        self.undone.clear();
    }
}
