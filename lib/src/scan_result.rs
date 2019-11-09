use crate::{Pointer, Value};
use serde::{Deserialize, Serialize};

/// A single scan result.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScanResult {
    /// Address where the scanned value lives.
    pub pointer: Pointer,
    /// The initial value in the scan. Is not rotated out.
    pub initial: Value,
    /// Subsequent values. Might be rotated out in length sequence.
    pub last: Option<Value>,
}

impl ScanResult {
    /// Construct a new scan result where the last value is specified.
    pub fn new(pointer: Pointer, value: Value) -> Self {
        Self {
            pointer,
            initial: value,
            last: None,
        }
    }

    /// Access the initial value of this result.
    pub fn initial(&self) -> &Value {
        &self.initial
    }

    /// Access the last value of the scan.
    pub fn last(&self) -> &Value {
        self.last.as_ref().unwrap_or(&self.initial)
    }
}
