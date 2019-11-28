use crate::{Pointer, Type, Value, ValueInfo};
use serde::{Deserialize, Serialize};
use std::fmt;

/// A single scan result.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScanResult {
    /// Address where the scanned value lives.
    pub pointer: Pointer,
    /// The type of the initial value.
    pub initial_type: Type,
    /// The initial value in the scan. Is not rotated out.
    pub initial: Value,
    /// The type of the last value.
    pub last_type: Type,
    /// Subsequent values. Might be rotated out in length sequence.
    pub last: Value,
}

impl ScanResult {
    /// Construct a new scan result where the last value is specified.
    pub fn new(pointer: Pointer, value_type: Type, value: Value) -> Self {
        Self {
            pointer,
            initial_type: value_type,
            initial: value,
            last_type: Type::None,
            last: Value::None,
        }
    }

    /// Get initial information.
    pub fn initial_info(&self) -> ValueInfo<'_> {
        ValueInfo {
            ty: self.initial_type,
            value: &self.initial,
        }
    }

    /// Get last information.
    pub fn last_info(&self) -> ValueInfo<'_> {
        ValueInfo {
            ty: self.last_type,
            value: &self.last,
        }
    }

    /// Get the initial type of the scan result.
    pub fn initial_type(&self) -> Type {
        self.initial_type
    }

    /// Access the initial value of this result.
    pub fn initial(&self) -> &Value {
        &self.initial
    }

    /// Get the last type.
    pub fn last_type(&self) -> Type {
        match &self.last_type {
            Type::None => self.initial_type,
            other => *other,
        }
    }

    /// Access the last value of the scan.
    pub fn last(&self) -> &Value {
        match &self.last {
            Value::None => &self.initial,
            other => other,
        }
    }
}

impl fmt::Display for ScanResult {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            fmt,
            "{} {} {}",
            self.pointer.fancy(),
            self.initial,
            self.last,
        )?;

        Ok(())
    }
}
