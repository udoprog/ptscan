use crate::ScanResult;
use ptscan::{MemoryInformation, ThreadId, Value};
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "value", rename_all = "snake_case")]
pub enum MemoryKind {
    None,
    Module(String),
    ThreadStack(ThreadId),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryInfo {
    #[serde(flatten)]
    pub base: MemoryInformation,
    pub kind: MemoryKind,
}

impl fmt::Display for MemoryInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.base)?;

        match &self.kind {
            MemoryKind::None => (),
            MemoryKind::Module(module) => {
                write!(fmt, " Module: {}", module)?;
            }
            MemoryKind::ThreadStack(thread_id) => {
                write!(fmt, " Thread Stack (id: {})", thread_id)?;
            }
        }

        Ok(())
    }
}

/// A scan result, coupled with a current value.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CurrentScanResult {
    #[serde(flatten)]
    pub result: ScanResult,
    pub current: Option<Value>,
}

impl fmt::Display for CurrentScanResult {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.result)?;

        if let Some(current) = &self.current {
            write!(fmt, " {}", current)?;
        }

        Ok(())
    }
}

impl MemoryInfo {
    /// Check if memory is in module.
    pub fn is_module(&self) -> bool {
        match self.kind {
            MemoryKind::Module(..) => true,
            _ => false,
        }
    }

    /// Check if memory is in module.
    pub fn is_threadstack(&self) -> bool {
        match self.kind {
            MemoryKind::ThreadStack(..) => true,
            _ => false,
        }
    }

    /// Check if memory is nothing special.
    pub fn is_none(&self) -> bool {
        match self.kind {
            MemoryKind::None => true,
            _ => false,
        }
    }
}
