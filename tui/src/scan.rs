use hashbrown::HashMap;
use ptscan::{Address, Pointer, ProcessHandle, Type, Value, ValueHolder, ValueInfo, Values};
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScanResult {
    pub address: Address,
    pub pointer: Pointer,
    pub initial_type: Type,
    pub initial: Value,
    pub last_type: Type,
    pub last: Value,
}

impl fmt::Display for ScanResult {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt,
            "{} => {} {} {} {} {}",
            self.pointer, self.address, self.initial_type, self.initial, self.last_type, self.last
        )
    }
}

impl ValueHolder for ScanResult {
    fn pointer(&self) -> &Pointer {
        &self.pointer
    }

    fn initial_info(&self) -> ValueInfo<'_> {
        ValueInfo {
            ty: self.initial_type,
            value: &self.initial,
        }
    }

    fn last_info(&self) -> ValueInfo<'_> {
        ValueInfo {
            ty: self.last_type,
            value: &self.last,
        }
    }

    fn insert(&mut self, value: Value) {
        self.last = value;
    }
}

pub struct Scan {
    pub addresses: Vec<Address>,
    pub initial: Values,
    pub last: Values,
    pub comments: HashMap<Pointer, String>,
}

impl Scan {
    /// Get a single scan result.
    pub fn get(&self, handle: &ProcessHandle, index: usize) -> Option<ScanResult> {
        let address = self.addresses.get(index).copied()?;
        let base = handle.address_to_pointer_base(address);
        let pointer = Pointer::new(base, std::iter::empty());

        Some(ScanResult {
            address,
            pointer,
            initial_type: self.initial.ty,
            initial: self.initial.get(index)?,
            last_type: self.last.ty,
            last: self.last.get(index)?,
        })
    }

    /// Get the number of results in this scan.
    pub fn len(&self) -> usize {
        self.addresses.len()
    }
}
