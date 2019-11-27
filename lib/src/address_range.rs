use crate::{Address, Size};
use serde::{Deserialize, Serialize};

/// A helper structure to define a range of addresses.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct AddressRange {
    pub base: Address,
    pub size: Size,
}

impl AddressRange {
    /// Check if the range contains the given address.
    pub fn contains(&self, value: Address) -> bool {
        self.base.0 <= value.0 && value.0 <= self.base.0.saturating_add(self.size.0)
    }

    /// Check if the range contains another range.
    pub fn contains_range(&self, other: &AddressRange) -> bool {
        self.base.0 <= other.base.0
            && other.base.0.saturating_add(other.size.0) <= self.base.0.saturating_add(self.size.0)
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
            Err(0) => return None,
            Err(n) => &things[n - 1],
        };

        if accessor(thing).contains(address) {
            return Some(thing);
        }

        None
    }

    /// Perform a binary search to check if `things` contains the given range.
    pub fn find_range_in_range<'a, T>(
        things: &'a [T],
        accessor: impl Fn(&T) -> &AddressRange,
        needle: &AddressRange,
    ) -> Option<&'a T> {
        let thing = match things.binary_search_by(|m| accessor(m).base.cmp(&needle.base)) {
            Ok(exact) => &things[exact],
            Err(0) => return None,
            Err(n) => &things[n - 1],
        };

        if accessor(thing).contains_range(needle) {
            return Some(thing);
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::AddressRange;

    #[test]
    fn test_find_in_range() {
        let mut ranges = Vec::new();

        let a = AddressRange {
            base: 0x100u64.into(),
            size: 0x10u64.into(),
        };

        let b = AddressRange {
            base: 0x110u64.into(),
            size: 0x10u64.into(),
        };

        ranges.push(a);
        ranges.push(b);

        assert_eq!(
            None,
            AddressRange::find_in_range(&ranges, |r| r, 0xffu64.into())
        );
        assert_eq!(
            Some(&a),
            AddressRange::find_in_range(&ranges, |r| r, 0x100u64.into())
        );
        assert_eq!(
            Some(&a),
            AddressRange::find_in_range(&ranges, |r| r, 0x10fu64.into())
        );
        assert_eq!(
            Some(&b),
            AddressRange::find_in_range(&ranges, |r| r, 0x110u64.into())
        );
        assert_eq!(
            Some(&b),
            AddressRange::find_in_range(&ranges, |r| r, 0x120u64.into())
        );
        assert_eq!(
            None,
            AddressRange::find_in_range(&ranges, |r| r, 0x121u64.into())
        );
    }
}
