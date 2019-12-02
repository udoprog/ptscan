use ptscan::{
    Address, Addresses, PortablePointer, ProcessHandle, Type, Value, ValueHolder, ValueRef, Values,
};
use serde::{Deserialize, Serialize};
use std::fmt;

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

pub struct Scan {
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
            value_type: self.last.ty,
            value: self.last.get(index)?,
        })
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
}
