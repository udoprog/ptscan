use crate::{Address, Size};

pub use crate::platform::Module;

#[derive(Debug)]
pub struct ModuleInfo {
    pub base_of_dll: Address,
    pub size_of_image: Size,
    pub entry_point: Address,
}
