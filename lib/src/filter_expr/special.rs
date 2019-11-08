const ZERO: [u8; 16] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

/// test if the given slice contains all zeros.
/// Hopefully this gets vectorized (please check!).
fn is_all_zeros(mut data: &[u8]) -> bool {
    while !data.is_empty() {
        let len = usize::min(data.len(), ZERO.len());

        if &data[..len] != &ZERO[..len] {
            return false;
        }

        data = &data[len..];
    }

    true
}

/// Find first nonzero byte in a slice.
fn find_first_nonzero(mut data: &[u8]) -> Option<usize> {
    let mut local = 0;

    while !data.is_empty() {
        let len = usize::min(data.len(), ZERO.len());

        if &data[..len] != &ZERO[..len] {
            break;
        }

        data = &data[len..];
        local += len;
    }

    if data.is_empty() {
        return None;
    }

    let index = match data.iter().position(|c| *c != 0) {
        Some(index) => index,
        None => return None,
    };

    Some(local + index)
}

pub enum Special {
    Bytes(Vec<u8>),
    NotBytes(Vec<u8>),
    Zero(usize),
    NonZero(usize),
    /// All the inner specials must match.
    All(Vec<Special>),
    /// Any of the inner specials match.
    Any(Vec<Special>),
    /// Special seek for a regular expression.
    Regex(regex::bytes::Regex),
}

impl Special {
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

                    if &data[..len] == &bytes[..len] {
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
            Self::Regex(ref regex) => match regex.find(data) {
                Some(m) => Some(m.start()),
                None => None,
            },
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
            find_first_nonzero(&iter::repeat(0u8).take(127).collect::<Vec<_>>())
        );
        let test = iter::repeat(0u8)
            .take(126)
            .chain(iter::once(8))
            .collect::<Vec<_>>();
        assert_eq!(Some(126), find_first_nonzero(&test));
        assert_ne!(0, test[126]);
        Ok(())
    }

    #[test]
    fn test_is_all_zeros() -> anyhow::Result<()> {
        assert_eq!(false, is_all_zeros(&[0, 0, 0, 0, 5, 0, 0]));
        assert_eq!(
            true,
            is_all_zeros(&iter::repeat(0u8).take(127).collect::<Vec<_>>())
        );
        let test = iter::repeat(0u8)
            .take(126)
            .chain(iter::once(8))
            .collect::<Vec<_>>();
        assert_eq!(false, is_all_zeros(&test));
        Ok(())
    }
}
