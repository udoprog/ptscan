//! Lightweight dynamic Address storage who's exact type is determined at runtime.
//!
//! This modules provides a collection which behaves remarkably similar to,
//!  Vec<T>, where T is an enum. Except, it can store the discriminator
//! separately from the data (which saves on _a lot_ of storage).
//!
//! T in this case is `Address`, and the discriminator is `PointerWidth`. Combined this
//! means we can store dynamically determined address with much less memory.

use crate::{Address, PointerWidth};
use dynamicvec::{DynamicType, DynamicVec};

impl DynamicType for PointerWidth {
    type Value = Address;

    fn element_size(self) -> usize {
        PointerWidth::size(self)
    }

    fn clone_data(self, data: &Vec<u8>, _: usize, _: usize) -> Vec<u8> {
        data.clone()
    }

    #[inline]
    unsafe fn read_unchecked(self, ptr: *const u8) -> Self::Value {
        PointerWidth::read_unchecked(self, ptr)
    }

    #[inline]
    unsafe fn write_unchecked(self, ptr: *mut u8, value: Self::Value) {
        PointerWidth::write_unchecked(self, ptr, value)
    }

    /// No need to drop any kind since it's all inlined.
    #[inline]
    unsafe fn drop_element(self, _: *mut u8) {}
    #[inline]
    fn drop(self, _: &mut Vec<u8>, _: &mut usize) {}
}

pub type Addresses = DynamicVec<PointerWidth>;
