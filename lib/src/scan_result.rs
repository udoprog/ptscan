use crate::{Pointer, Value};
use serde::{Deserialize, Serialize};
use std::fmt;

/// A single scan result.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScanResult {
    /// Address where the scanned value lives.
    pub pointer: Pointer,
    /// The initial value in the scan. Is not rotated out.
    pub initial: Value,
    /// Subsequent values. Might be rotated out in length sequence.
    pub last: Value,
}

impl ScanResult {
    /// Construct a new scan result where the last value is specified.
    pub fn new(pointer: Pointer, value: Value) -> Self {
        let last = Value::None(value.ty());

        Self {
            pointer,
            initial: value,
            last,
        }
    }

    /// Access the initial value of this result.
    pub fn initial(&self) -> &Value {
        &self.initial
    }

    /// Access the last value of the scan.
    pub fn last(&self) -> &Value {
        match &self.last {
            Value::None(..) => &self.initial,
            other => other,
        }
    }
}

impl fmt::Display for ScanResult {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt,
            "{} {} {} {} {}",
            self.pointer.fancy(),
            self.initial.ty(),
            self.initial,
            self.last.ty(),
            self.last,
        )?;

        Ok(())
    }
}
