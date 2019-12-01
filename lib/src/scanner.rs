use crate::{
    Address, Base, ProcessHandle, Special, Test, Token, Type, TypedFilterExpr, ValueRef, Values,
};
use anyhow::bail;
use std::convert::TryInto as _;

pub trait Scanner {
    fn scan(
        &self,
        base: Address,
        value_type: Type,
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
}

impl<'a> DefaultScanner<'a> {
    pub fn new(
        filter: TypedFilterExpr<'a>,
        special: Option<&'a Special>,
        alignment: Option<usize>,
        step_size: usize,
    ) -> Self {
        Self {
            filter,
            special,
            alignment,
            step_size,
        }
    }
}

impl<'a> Scanner for DefaultScanner<'a> {
    fn scan(
        &self,
        base: Address,
        value_type: Type,
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
                let (value, advance) = proxy.eval(value_type)?;

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

        return Ok(());

        fn align(to_align: &mut usize, alignment: usize) {
            let rem = *to_align % alignment;

            if rem > 0 {
                *to_align += alignment - rem;
            }
        }
    }
}
