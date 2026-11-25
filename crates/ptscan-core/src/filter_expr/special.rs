const ZERO: [u8; 16] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

/// test if the given slice contains all zeros.
/// Hopefully this gets vectorized (please check!).
pub(crate) fn is_all_zeros(mut data: &[u8]) -> bool {
    while !data.is_empty() {
        let len = usize::min(data.len(), ZERO.len());

        if data[..len] != ZERO[..len] {
            return false;
        }

        data = &data[len..];
    }

    true
}

/// Find first nonzero byte in a slice.
pub(crate) fn find_first_nonzero(mut data: &[u8]) -> Option<usize> {
    let mut local = 0;

    // quick, constant time checks.
    if data.is_empty() {
        return None;
    }

    if data[0] != 0 {
        return Some(0);
    }

    while !data.is_empty() {
        let len = usize::min(data.len(), ZERO.len());

        if data[..len] != ZERO[..len] {
            break;
        }

        data = &data[len..];
        local += len;
    }

    if data.is_empty() {
        return None;
    }

    let index = data.iter().position(|c| *c != 0)?;

    Some(local + index)
}

#[derive(Debug, Clone)]
pub enum Special {
    Bytes(Vec<u8>),
    NotBytes(Vec<u8>),
    Zero(usize),
    NonZero(Option<usize>),
    /// All the inner specials must match.
    All(Vec<Special>),
    /// Any of the inner specials match.
    Any(Vec<Special>),
    /// Special seek for a regular expression.
    Regex(regex::bytes::Regex),
}

impl Special {
    /// How many bytes before an offset found by [`Special::test`] a value
    /// containing that byte can start.
    ///
    /// A non-zero special finds the first non-zero byte, which is anywhere in
    /// the value.
    pub fn lookbehind(&self) -> usize {
        match *self {
            Self::NonZero(Some(width)) => width.saturating_sub(1),
            Self::All(ref inner) | Self::Any(ref inner) => {
                inner.iter().map(Self::lookbehind).max().unwrap_or_default()
            }
            _ => 0,
        }
    }

    pub fn test(&self, mut data: &[u8]) -> Option<usize> {
        match *self {
            Self::All(ref all) => all.iter().flat_map(|s| s.test(data)).max(),
            Self::Any(ref any) => any.iter().flat_map(|s| s.test(data)).min(),
            Self::Bytes(ref bytes) => {
                if bytes.is_empty() {
                    return None;
                }

                let first = bytes[0];
                let mut local = 0usize;

                while !data.is_empty() {
                    let index = match memchr::memchr(first, data) {
                        Some(index) => index,
                        None => {
                            return None;
                        }
                    };

                    data = &data[index..];
                    let len = usize::min(data.len(), bytes.len());

                    if data[..len] == bytes[..len] {
                        return Some(local + index);
                    }

                    local += index + 1;
                    data = &data[1..];
                }

                None
            }
            Self::NotBytes(..) => None,
            Self::NonZero(..) => find_first_nonzero(data),
            Self::Zero(width) => {
                if width == 0 {
                    return None;
                }

                let mut local = 0usize;

                while !data.is_empty() {
                    let index = match memchr::memchr(0, data) {
                        Some(index) => index,
                        None => {
                            return Some(local + data.len());
                        }
                    };

                    local += index;
                    data = &data[index..];

                    if data.len() < width {
                        return Some(local);
                    }

                    if is_all_zeros(&data[..width]) {
                        return Some(local);
                    }

                    let offset = match data[..width].iter().position(|c| *c != 0) {
                        Some(offset) => offset,
                        None => width,
                    };

                    local += offset;
                    data = &data[offset..];
                }

                None
            }
            Self::Regex(ref regex) => regex.find(data).map(|m| m.start()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{find_first_nonzero, is_all_zeros};
    use std::iter;

    #[test]
    fn test_find_first_nonzero() -> anyhow::Result<()> {
        assert_eq!(Some(4), find_first_nonzero(&[0, 0, 0, 0, 5, 0, 0]));
        assert_eq!(
            None,
            find_first_nonzero(&std::iter::repeat_n(0u8, 127).collect::<Vec<_>>())
        );
        let test = std::iter::repeat_n(0u8, 126)
            .chain(iter::once(8))
            .collect::<Vec<_>>();
        assert_eq!(Some(126), find_first_nonzero(&test));
        assert_ne!(0, test[126]);
        Ok(())
    }

    #[test]
    fn test_is_all_zeros() -> anyhow::Result<()> {
        assert!(!is_all_zeros(&[0, 0, 0, 0, 5, 0, 0]));
        assert!(is_all_zeros(
            &std::iter::repeat_n(0u8, 127).collect::<Vec<_>>()
        ));
        let test = std::iter::repeat_n(0u8, 126)
            .chain(iter::once(8))
            .collect::<Vec<_>>();
        assert!(!is_all_zeros(&test));
        Ok(())
    }
}
