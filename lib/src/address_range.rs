use crate::{Address, Size};

/// A helper structure to define a range of addresses.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AddressRange {
    pub base: Address,
    pub size: Size,
}

impl AddressRange {
    pub fn contains(&self, value: Address) -> bool {
        self.base <= value && value <= self.base.saturating_add(self.size)
    }

    /// Helper function to find which memory region a given address is contained in.
    ///
    /// This assumes the memory regions are sorted by their `base`.
    pub fn find_in_range<T>(
        things: &[T],
        accessor: impl Fn(&T) -> &AddressRange,
        address: Address,
    ) -> Option<&T> {
        let thing = match things.binary_search_by(|m| accessor(m).base.cmp(&address)) {
            Ok(exact) => &things[exact],
            Err(closest) => {
                if closest == 0 {
                    return None;
                }

                &things[closest - 1]
            }
        };

        if accessor(thing).contains(address) {
            return Some(thing);
        }

        None
    }
}
