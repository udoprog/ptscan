use crate::{
    filter_expr::special::{find_first_nonzero, is_all_zeros},
    Address, Alignment, Base, ProcessHandle, Special, Test, Token, Type, TypedFilterExpr, ValueRef,
    Values,
};
use anyhow::bail;
use std::{convert::TryInto as _, marker};

fn align(to_align: &mut usize, alignment: usize) {
    let rem = *to_align % alignment;

    if rem > 0 {
        *to_align += alignment - rem;
    }
}

pub trait Scanner: Send + Sync {
    fn scan(
        &self,
        base: Address,
        handle: &ProcessHandle,
        data: &[u8],
        bases: &mut Vec<Base>,
        values: &mut Values,
        hits: &mut u64,
        cancel: &Token,
    ) -> anyhow::Result<()>;
}

/// A default scanner with a filter.
pub struct DefaultScanner<'a> {
    filter: TypedFilterExpr<'a>,
    special: Option<&'a Special>,
    alignment: Option<usize>,
    step_size: usize,
    value_type: Type,
}

impl<'a> DefaultScanner<'a> {
    pub fn new(
        filter: TypedFilterExpr<'a>,
        special: Option<&'a Special>,
        alignment: Option<usize>,
        step_size: usize,
        value_type: Type,
    ) -> Self {
        Self {
            filter,
            special,
            alignment,
            step_size,
            value_type,
        }
    }
}

impl<'a> Scanner for DefaultScanner<'a> {
    fn scan(
        &self,
        base: Address,
        handle: &ProcessHandle,
        data: &[u8],
        bases: &mut Vec<Base>,
        values: &mut Values,
        hits: &mut u64,
        cancel: &Token,
    ) -> anyhow::Result<()> {
        let mut offset = match self.special {
            Some(special) => match special.test(data) {
                Some(offset) => offset,
                None => return Ok(()),
            },
            None => 0usize,
        };

        if let Some(size) = self.alignment {
            align(&mut offset, size);
        }

        let mut last_address = None;

        while offset < data.len() && !cancel.test() {
            let address = match base.checked_add(offset.try_into()?) {
                Some(address) => address,
                None => bail!("base `{}` + offset `{}` out of range", base, offset),
            };

            let mut proxy = handle.address_proxy(&address);

            // sanity check
            if let Some(last_address) = last_address {
                if last_address >= address {
                    bail!(
                        "BUG: address did not increase during scan: {} -> {}",
                        last_address,
                        address
                    )
                }
            }

            last_address = Some(address);

            if let Test::True = self
                .filter
                .test(ValueRef::None, ValueRef::None, &mut proxy)?
            {
                *hits += 1;
                let (value, advance) = proxy.eval(self.value_type)?;

                let base = handle.address_to_base(address);

                bases.push(base);
                values.push(value);

                if let Some(advance) = advance {
                    if advance == 0 {
                        bail!("BUG: attempt to advance by 0 bytes");
                    }

                    offset += advance;
                } else {
                    offset += self.step_size;
                }
            } else {
                offset += self.step_size;
            }

            if offset >= data.len() {
                break;
            }

            if let Some(special) = self.special {
                offset += match special.test(&data[offset..]) {
                    Some(o) => o,
                    None => return Ok(()),
                };

                if let Some(size) = self.alignment {
                    align(&mut offset, size);
                }
            }
        }

        Ok(())
    }
}

/// A highly specialized, aligned non-zero scanner.
pub struct NonZeroScanner<A, B>
where
    A: Alignment,
    B: Send + Sync + byteorder::ByteOrder,
{
    alignment: A,
    type_size: usize,
    value_type: Type,
    marker: marker::PhantomData<B>,
}

impl<A, B> NonZeroScanner<A, B>
where
    A: Alignment,
    B: Send + Sync + byteorder::ByteOrder,
{
    pub fn new(alignment: A, type_size: usize, value_type: Type) -> Self {
        Self {
            alignment,
            type_size,
            value_type,
            marker: marker::PhantomData,
        }
    }
}

impl<A, B> Scanner for NonZeroScanner<A, B>
where
    A: Alignment,
    B: Send + Sync + byteorder::ByteOrder,
{
    fn scan(
        &self,
        base: Address,
        handle: &ProcessHandle,
        data: &[u8],
        bases: &mut Vec<Base>,
        values: &mut Values,
        hits: &mut u64,
        cancel: &Token,
    ) -> anyhow::Result<()> {
        let mut offset = 0usize;

        while offset < data.len() && !cancel.test() {
            let o = match find_first_nonzero(&data[offset..]) {
                Some(o) => o,
                None => break,
            };

            offset += o;

            self.alignment.align(&mut offset);

            let d = &data[offset..];

            if d.len() < self.type_size {
                break;
            }

            if let Some(value) = self.value_type.decode_fixed::<B>(handle, d) {
                let address = match base.checked_add(o.try_into()?) {
                    Some(address) => address,
                    None => break,
                };

                bases.push(Base::from(address));
                values.push(value);
                *hits += 1;
                offset += self.alignment.step();
                continue;
            }

            break;
        }

        Ok(())
    }
}

/// A highly specialized, aligned zero scanner.
pub struct ZeroScanner<A, B>
where
    A: Alignment,
    B: Send + Sync + byteorder::ByteOrder,
{
    alignment: A,
    type_size: usize,
    value_type: Type,
    marker: marker::PhantomData<B>,
}

impl<A, B> ZeroScanner<A, B>
where
    A: Alignment,
    B: Send + Sync + byteorder::ByteOrder,
{
    pub fn new(alignment: A, type_size: usize, value_type: Type) -> Self {
        Self {
            alignment,
            type_size,
            value_type,
            marker: marker::PhantomData,
        }
    }
}

impl<A, B> Scanner for ZeroScanner<A, B>
where
    A: Alignment,
    B: Send + Sync + byteorder::ByteOrder,
{
    fn scan(
        &self,
        base: Address,
        handle: &ProcessHandle,
        data: &[u8],
        bases: &mut Vec<Base>,
        values: &mut Values,
        hits: &mut u64,
        cancel: &Token,
    ) -> anyhow::Result<()> {
        let mut offset = 0usize;

        while offset < data.len() && !cancel.test() {
            let o = match memchr::memchr(0, &data[offset..]) {
                Some(o) => o,
                None => break,
            };

            offset += o;

            self.alignment.align(&mut offset);

            let d = &data[offset..];

            if d.len() < self.type_size {
                break;
            }

            let d = &d[..self.type_size];

            if !is_all_zeros(d) {
                offset += self.alignment.step();
                continue;
            }

            if let Some(value) = self.value_type.decode_fixed::<B>(handle, d) {
                let address = match base.checked_add(o.try_into()?) {
                    Some(address) => address,
                    None => break,
                };

                bases.push(Base::from(address));
                values.push(value);
                *hits += 1;
                offset += self.alignment.step();
                continue;
            }

            break;
        }

        Ok(())
    }
}

/// A highly specialized, aligned zero scanner.
pub struct BufferScanner<B>
where
    B: Send + Sync + byteorder::ByteOrder,
{
    value_type: Type,
    buffer: Vec<u8>,
    marker: marker::PhantomData<B>,
}

impl<B> BufferScanner<B>
where
    B: Send + Sync + byteorder::ByteOrder,
{
    pub fn new(value_type: Type, buffer: Vec<u8>) -> Self {
        Self {
            value_type,
            buffer,
            marker: marker::PhantomData,
        }
    }
}

impl<B> Scanner for BufferScanner<B>
where
    B: Send + Sync + byteorder::ByteOrder,
{
    fn scan(
        &self,
        base: Address,
        handle: &ProcessHandle,
        data: &[u8],
        bases: &mut Vec<Base>,
        values: &mut Values,
        hits: &mut u64,
        cancel: &Token,
    ) -> anyhow::Result<()> {
        let mut offset = 0usize;

        while offset < data.len() && !cancel.test() {
            let o = match memchr::memchr(self.buffer[0], &data[offset..]) {
                Some(o) => o,
                None => break,
            };

            offset += o;

            let d = &data[offset..];

            if d.len() < self.buffer.len() {
                break;
            }

            let d = &d[..self.buffer.len()];

            if d != &self.buffer[..] {
                offset += self.buffer.len() + 1;
                continue;
            }

            if let Some(value) = self.value_type.decode_fixed::<B>(handle, d) {
                let address = match base.checked_add(o.try_into()?) {
                    Some(address) => address,
                    None => break,
                };

                bases.push(Base::from(address));
                values.push(value);
                *hits += 1;
                offset += self.buffer.len() + 1;
                continue;
            }

            break;
        }

        Ok(())
    }
}
