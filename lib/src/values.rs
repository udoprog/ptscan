use crate::{Type, Value};
use std::{ptr, slice};

/// A dynamic collection of values, with the goal of supporting efficient delete
/// operations (through swap_remove), and as much contiguous memory as possible.
pub struct Values {
    ty: Type,
    element_size: usize,
    data: Vec<u8>,
    len: usize,
}

impl Values {
    /// Construct a new values collection.
    pub fn new(ty: Type, pointer_width: usize) -> Self {
        Self {
            ty,
            element_size: ty.element_size(pointer_width),
            data: Vec::new(),
            len: 0,
        }
    }

    /// Get the value at the given location.
    pub fn get(&self, index: usize) -> Option<Value> {
        if index >= self.len {
            return None;
        }

        let size = self.element_size;

        let value = unsafe {
            let ptr = self.data.as_ptr().add(index * size);

            match self.ty {
                Type::None => Value::None,
                Type::Pointer => Value::Pointer(ptr::read(ptr as *const _)),
                Type::U8 => Value::U8(ptr::read(ptr as *const _)),
                Type::I8 => Value::I8(ptr::read(ptr as *const _)),
                Type::U16 => Value::U16(ptr::read(ptr as *const _)),
                Type::I16 => Value::I16(ptr::read(ptr as *const _)),
                Type::U32 => Value::U32(ptr::read(ptr as *const _)),
                Type::I32 => Value::I32(ptr::read(ptr as *const _)),
                Type::U64 => Value::U64(ptr::read(ptr as *const _)),
                Type::I64 => Value::I64(ptr::read(ptr as *const _)),
                Type::U128 => Value::U128(ptr::read(ptr as *const _)),
                Type::I128 => Value::I128(ptr::read(ptr as *const _)),
                Type::F32 => Value::F32(ptr::read(ptr as *const _)),
                Type::F64 => Value::F64(ptr::read(ptr as *const _)),
                Type::String(..) => {
                    let string = Clone::clone(&*(ptr as *const String));
                    Value::String(string.clone())
                }
                Type::Bytes(len) => {
                    let mut data = Vec::with_capacity(len);
                    ptr::copy_nonoverlapping(ptr, data.as_mut_ptr(), len);
                    data.set_len(len);
                    Value::Bytes(data)
                }
            }
        };

        Some(value)
    }

    /// Push the given value into the collection.
    pub unsafe fn unsafe_push(&mut self, src: *const u8, len: usize) {
        self.len += 1;

        if self.element_size == 0 {
            return;
        }

        self.data.reserve(self.element_size);

        let pos = self.data.len();
        let ptr = self.data.as_mut_ptr().add(pos);

        match self.ty {
            Type::String(..) => {
                let value = slice::from_raw_parts(src, len).to_vec();
                let string = String::from_utf8_unchecked(value);
                ptr::write(ptr as *mut String, string);
                self.data.set_len(pos + self.element_size);
            }
            _ => {
                ptr::copy_nonoverlapping(src, ptr, self.element_size);
                self.data.set_len(pos + self.element_size);
            }
        }
    }

    /// Drop all elements in the vector.
    unsafe fn drop_string_vec(&mut self) {
        let len = self.data.len();
        let ptr = self.data.as_mut_ptr();
        let mut pos = 0usize;

        while pos < len {
            let ptr = ptr.add(pos);
            ptr::drop_in_place(ptr as *mut String);
            pos += self.element_size;
        }
    }
}

impl Drop for Values {
    fn drop(&mut self) {
        match self.ty {
            Type::String(..) => unsafe { self.drop_string_vec() },
            _ => (),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Value, Values};
    use crate::{Encoding, Type};

    #[test]
    fn test_values() {
        let encoding = Encoding::default();
        let mut values = Values::new(Type::String(encoding), 8);
        let string = String::from("hello world");

        unsafe {
            values.unsafe_push(string.as_ptr(), string.len());
        }

        assert_eq!(Some(Value::String(string)), values.get(0));
        assert_eq!(None, values.get(1));
    }
}
