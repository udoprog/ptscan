use ptscan::{MemoryInformation, ThreadId};
use serde::{Deserialize, Serialize};

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
