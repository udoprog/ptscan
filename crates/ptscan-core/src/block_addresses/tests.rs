use std::time::Instant;

use super::*;

/// A seeded xorshift64* generator, so failures reproduce.
struct Rng(u64);

impl Rng {
    fn new(seed: u64) -> Self {
        Self(seed.wrapping_mul(0x9e37_79b9_7f4a_7c15) | 1)
    }

    fn next(&mut self) -> u64 {
        self.0 ^= self.0 >> 12;
        self.0 ^= self.0 << 25;
        self.0 ^= self.0 >> 27;
        self.0.wrapping_mul(0x2545_f491_4f6c_dd1d)
    }

    fn below(&mut self, n: u64) -> u64 {
        self.next() % n
    }

    /// True with probability `percent / 100`.
    fn chance(&mut self, percent: f64) -> bool {
        (self.next() >> 11) as f64 / (1u64 << 53) as f64 * 100.0 < percent
    }
}

/// Sorted addresses over a few regions with `percent` of slots filled.
/// Regions start at random aligned addresses, so some meet inside of a block
/// and some straddle block boundaries.
fn oracle(rng: &mut Rng, step: u64, percent: f64) -> Vec<u64> {
    let mut out = Vec::new();
    let mut at = rng.below(1 << 20) * step;

    for _ in 0..1 + rng.below(5) {
        let slots = 1 + rng.below(3 * 65536 / step.min(65536) + 64);

        for slot in 0..slots {
            if rng.chance(percent) {
                out.push(at + slot * step);
            }
        }

        at += slots * step;

        // Leave a gap, sometimes small enough to share a block.
        at += match rng.below(3) {
            0 => step,
            1 => (1 + rng.below(64)) * step,
            _ => (1 + rng.below(1 << 14)) * step * 16,
        };
    }

    out
}

fn build(step: u64, values: &[u64]) -> BlockAddresses {
    BlockAddresses::from_sorted(step, values.iter().copied().map(Address)).unwrap()
}

fn collect(set: &BlockAddresses) -> Vec<u64> {
    set.iter().map(|a| a.0).collect()
}

/// Check every accessor of `set` against `expected`.
fn check(set: &BlockAddresses, expected: &[u64], rng: &mut Rng) {
    assert_eq!(set.len(), expected.len());
    assert_eq!(set.is_empty(), expected.is_empty());
    assert_eq!(set.iter().len(), expected.len());
    assert_eq!(collect(set), expected);
    assert_eq!(set.first().map(|a| a.0), expected.first().copied());
    assert_eq!(set.last().map(|a| a.0), expected.last().copied());
    assert_eq!(set.get(expected.len()), None);
    assert_eq!(set.iter_from(expected.len()).next(), None);
    assert_eq!(set.iter_from(expected.len() + 10).len(), 0);

    for _ in 0..64.min(expected.len()) {
        let i = rng.below(expected.len() as u64) as usize;
        assert_eq!(set.get(i).map(|a| a.0), Some(expected[i]), "get({})", i);

        let it = set.iter_from(i);
        assert_eq!(it.len(), expected.len() - i);
        let page: Vec<u64> = it.take(100).map(|a| a.0).collect();
        let end = (i + 100).min(expected.len());
        assert_eq!(page, &expected[i..end], "iter_from({})", i);
    }

    // Blocks: contiguous ranks, non-empty, the smaller encoding, and every
    // address inside of its block.
    let mut rank = 0;
    let mut flat = Vec::new();
    let mut previous_base = None;

    for block in set.blocks() {
        assert_eq!(block.rank(), rank);
        assert!(!block.is_empty());
        assert!(previous_base < Some(block.base().0));
        previous_base = Some(block.base().0);

        let offsets = data_size(Encoding::Offsets, block.len(), set.shift).unwrap();
        let bitmap = data_size(Encoding::Bitmap, block.len(), set.shift).unwrap();
        let expected_encoding = if bitmap < offsets {
            Encoding::Bitmap
        } else {
            Encoding::Offsets
        };
        assert_eq!(block.encoding(), expected_encoding);

        let addresses: Vec<u64> = block.iter().map(|a| a.0).collect();
        assert_eq!(addresses.len(), block.len());

        for (k, &a) in addresses.iter().enumerate() {
            assert_eq!(a >> BLOCK_SHIFT, block.base().0 >> BLOCK_SHIFT);
            assert_eq!(block.get(k).map(|a| a.0), Some(a));
        }

        assert_eq!(block.get(block.len()), None);
        rank += block.len();
        flat.extend(addresses);
    }

    assert_eq!(rank, expected.len());
    assert_eq!(flat, expected);
}

const STEPS: &[u64] = &[1, 4, 8];
const DENSITIES: &[f64] = &[0.5, 1.0, 5.0, 6.25, 10.0, 25.0, 50.0, 90.0, 100.0];

#[test]
fn oracle_steps_and_densities() {
    let mut rng = Rng::new(1);

    for &step in STEPS {
        for &percent in DENSITIES {
            for _ in 0..4 {
                let expected = oracle(&mut rng, step, percent);
                let set = build(step, &expected);
                check(&set, &expected, &mut rng);
            }
        }
    }
}

#[test]
fn dense_blocks_use_bitmaps() {
    // Every slot of one block filled.
    let expected: Vec<u64> = (0..16384).map(|i| 0x10000 + i * 4).collect();
    let set = build(4, &expected);
    let blocks: Vec<_> = set.blocks().collect();
    assert_eq!(blocks.len(), 1);
    assert_eq!(blocks[0].encoding(), Encoding::Bitmap);
    assert_eq!(set.bytes(), 16 + 2048);
    check(&set, &expected, &mut Rng::new(2));

    // Break-even at step 4 is 1024 hits: offsets at 1024, a bitmap above.
    let expected: Vec<u64> = (0..1024).map(|i| i * 4).collect();
    let set = build(4, &expected);
    assert_eq!(set.blocks().next().unwrap().encoding(), Encoding::Offsets);

    let expected: Vec<u64> = (0..1025).map(|i| i * 4).collect();
    let set = build(4, &expected);
    assert_eq!(set.blocks().next().unwrap().encoding(), Encoding::Bitmap);
}

#[test]
fn odd_and_large_steps() {
    let mut rng = Rng::new(3);

    // Not a power of two: slots are one byte wide.
    for &(step, percent) in &[(3, 30.0), (12, 60.0), (1 << 17, 100.0), (4096, 50.0)] {
        let expected = oracle(&mut rng, step, percent);
        let set = build(step, &expected);
        check(&set, &expected, &mut rng);
    }
}

#[test]
fn top_of_address_space() {
    let expected = vec![u64::MAX - 7, u64::MAX];
    let set = build(1, &expected);
    check(&set, &expected, &mut Rng::new(4));

    let expected: Vec<u64> = (0..20000).map(|i| (u64::MAX - 7) - i * 8).rev().collect();
    let set = build(8, &expected);
    check(&set, &expected, &mut Rng::new(5));
}

#[test]
fn empty() {
    let set = BlockAddresses::new(4).unwrap();
    check(&set, &[], &mut Rng::new(6));
    assert_eq!(set.bytes(), 0);
    assert_eq!(set.blocks().count(), 0);
}

#[test]
fn errors() {
    assert_eq!(BlockAddresses::new(0).unwrap_err(), BlockError::ZeroStep);

    let mut b = BlockAddressesBuilder::new(4).unwrap();
    b.push(Address(8)).unwrap();
    assert!(matches!(
        b.push(Address(8)),
        Err(BlockError::Unsorted { .. })
    ));
    assert!(matches!(
        b.push(Address(4)),
        Err(BlockError::Unsorted { .. })
    ));
    assert!(matches!(
        b.push(Address(10)),
        Err(BlockError::Misaligned { .. })
    ));

    let other = build(8, &[16]);
    assert_eq!(b.append(&other), Err(BlockError::StepMismatch(4, 8)));

    let other = build(4, &[8, 12]);
    assert!(matches!(b.append(&other), Err(BlockError::Unsorted { .. })));

    b.push(Address(12)).unwrap();
    assert_eq!(collect(&b.finish().unwrap()), vec![8, 12]);
}

/// Splitting a set at random points, building each part on its own and
/// appending them must give exactly the set built in one go, including parts
/// that meet inside of one block.
#[test]
fn append_merges_boundary_blocks() {
    let mut rng = Rng::new(7);

    for &step in STEPS {
        for &percent in DENSITIES {
            for _ in 0..4 {
                let expected = oracle(&mut rng, step, percent);
                let whole = build(step, &expected);

                let mut cuts: Vec<usize> = (0..rng.below(6))
                    .map(|_| rng.below(expected.len() as u64 + 1) as usize)
                    .collect();
                cuts.push(0);
                cuts.push(expected.len());
                cuts.sort_unstable();

                let mut builder = BlockAddressesBuilder::new(step).unwrap();

                for w in cuts.windows(2) {
                    let part = &expected[w[0]..w[1]];

                    // Sometimes push a few addresses directly before appending.
                    if rng.below(3) == 0 && !part.is_empty() {
                        let n = rng.below(part.len() as u64 + 1) as usize;

                        for &a in &part[..n] {
                            builder.push(Address(a)).unwrap();
                        }

                        builder.append(&build(step, &part[n..])).unwrap();
                    } else {
                        builder.append(&build(step, part)).unwrap();
                    }
                }

                assert_eq!(builder.len(), expected.len());
                let merged = builder.finish().unwrap();
                assert_eq!(merged, whole);
                check(&merged, &expected, &mut rng);
            }
        }
    }
}

#[test]
fn append_boundary_block_explicit() {
    // Two chunks meeting inside of block 1, the second spanning into block 2.
    let a = build(4, &[0x1_0000, 0x1_8000]);
    let b = build(4, &[0x1_8004, 0x1_fffc, 0x2_0000]);

    let mut builder = BlockAddressesBuilder::new(4).unwrap();
    builder.append(&a).unwrap();
    builder.append(&b).unwrap();
    let merged = builder.finish().unwrap();

    assert_eq!(merged.blocks().count(), 2);
    assert_eq!(
        merged,
        build(4, &[0x1_0000, 0x1_8000, 0x1_8004, 0x1_fffc, 0x2_0000])
    );
}

#[test]
fn retain_by_index_matches_oracle() {
    let mut rng = Rng::new(8);

    for &step in STEPS {
        for &percent in DENSITIES {
            for &keep_percent in &[0.0, 1.0, 10.0, 50.0, 99.0, 100.0] {
                let expected = oracle(&mut rng, step, percent);
                let mut set = build(step, &expected);

                let mask: Vec<bool> = expected.iter().map(|_| rng.chance(keep_percent)).collect();
                let kept: Vec<u64> = expected
                    .iter()
                    .zip(&mask)
                    .filter(|(_, &k)| k)
                    .map(|(&a, _)| a)
                    .collect();

                let mut seen = 0;
                set.retain_by_index(|i| {
                    assert_eq!(i, seen);
                    seen += 1;
                    mask[i]
                })
                .unwrap();
                assert_eq!(seen, expected.len());

                assert_eq!(set, build(step, &kept));
                check(&set, &kept, &mut rng);
            }
        }
    }
}

/// Encode, iterate and `get` on a 38 M set at step 4, like `== 0` u32 on
/// the 256 MiB bench child.
///
/// cargo test --release -p ptscan-core --lib block_addresses_bench -- --ignored --nocapture
#[test]
#[ignore]
fn block_addresses_bench() {
    const SCANNED: u64 = 256 << 20;
    const STEP: u64 = 4;
    const HITS: u64 = 38_000_000;

    let mut rng = Rng::new(9);
    let percent = HITS as f64 / (SCANNED / STEP) as f64 * 100.0;
    let base = 0x7f00_0000_0000u64;
    let input: Vec<u64> = (0..SCANNED / STEP)
        .filter(|_| rng.chance(percent))
        .map(|slot| base + slot * STEP)
        .collect();

    let start = Instant::now();
    let set = build(STEP, &input);
    let encode = start.elapsed();

    let start = Instant::now();
    let mut sum = 0u64;
    for a in set.iter() {
        sum = sum.wrapping_add(a.0);
    }
    let iterate = start.elapsed();

    let start = Instant::now();
    let mut block_sum = 0u64;
    for block in set.blocks() {
        for a in block.iter() {
            block_sum = block_sum.wrapping_add(a.0);
        }
    }
    let iterate_blocks = start.elapsed();
    assert_eq!(sum, block_sum);

    const GETS: usize = 100_000;
    let indexes: Vec<usize> = (0..GETS)
        .map(|_| rng.below(set.len() as u64) as usize)
        .collect();
    let start = Instant::now();
    for &i in &indexes {
        sum = sum.wrapping_add(set.get(i).unwrap().0);
    }
    let get = start.elapsed();

    const PAGES: usize = 10_000;
    let start = Instant::now();
    for &i in indexes.iter().take(PAGES) {
        for a in set.iter_from(i).take(1000) {
            sum = sum.wrapping_add(a.0);
        }
    }
    let page = start.elapsed();

    let mut rng2 = Rng::new(10);
    let mut retained = set.clone();
    let start = Instant::now();
    retained.retain_by_index(|_| rng2.chance(18.0)).unwrap();
    let retain = start.elapsed();

    println!(
        "hits: {} ({:.1} % of slots), {} blocks",
        set.len(),
        percent,
        set.blocks().len()
    );
    println!(
        "bytes: {:.2} MiB (Vec<u64> {:.0} MiB)",
        set.bytes() as f64 / (1 << 20) as f64,
        (input.len() * 8) as f64 / (1 << 20) as f64,
    );
    println!(
        "encode: {:?} ({:.2} ns/address)",
        encode,
        encode.as_nanos() as f64 / set.len() as f64
    );
    println!(
        "iter: {:?} ({:.2} ns/address)",
        iterate,
        iterate.as_nanos() as f64 / set.len() as f64
    );
    println!(
        "blocks: {:?} ({:.2} ns/address)",
        iterate_blocks,
        iterate_blocks.as_nanos() as f64 / set.len() as f64
    );
    println!("get: {:?} per call", get / GETS as u32);
    println!(
        "page (iter_from + take 1000): {:?} per page",
        page / PAGES as u32
    );
    println!(
        "retain_by_index (18 %): {:?}, {} kept, {:.2} MiB",
        retain,
        retained.len(),
        retained.bytes() as f64 / (1 << 20) as f64
    );
    println!("(checksum {})", sum);
}
