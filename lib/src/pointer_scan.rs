use crate::{
    Address, Location, Offset, Pointer, PointerBase, ProcessHandle, ScanResult, Size, Token, Type,
    Value,
};
use anyhow::anyhow;
use hashbrown::{hash_map, HashMap};
use std::collections::{BTreeMap, VecDeque};

type SVec = Vec<Offset>;

pub trait PointerScanInitialProgress {
    fn report(&mut self, queue: usize, results: usize) -> anyhow::Result<()>;
}

pub trait PointerScanBackreferenceProgress {
    /// Report progress during a backreference scan.
    fn report(&mut self, remaining: usize, results: usize) -> anyhow::Result<()>;
}

/// Helper for performing a pointer scan.
///
/// The configuration to use during a scan.
///
/// Defaults are
///
/// max_offset: 0x1000
/// max_depth: 7
pub struct PointerScan<'a> {
    #[allow(unused)]
    thread_pool: &'a rayon::ThreadPool,
    handle: &'a ProcessHandle,
    cancel: &'a Token,
    /// Forward references used.
    pub forward: HashMap<Address, Address>,
    /// Reverse references used.
    pub reverse: BTreeMap<Address, Address>,
    // contains both all visited links and alternative paths for each address.
    pub visited: HashMap<Address, Vec<SVec>>,
    pub visited_count: usize,
    pub max_offset: Size,
    pub max_depth: usize,
}

impl<'a> PointerScan<'a> {
    pub fn new(
        thread_pool: &'a rayon::ThreadPool,
        handle: &'a ProcessHandle,
        cancel: &'a Token,
    ) -> Self {
        Self {
            thread_pool,
            handle,
            cancel,
            forward: Default::default(),
            reverse: Default::default(),
            visited: HashMap::new(),
            visited_count: 0,
            max_offset: 0x1000u32.into(),
            max_depth: 7,
        }
    }

    /// Build forward and backward references.
    pub fn build_references<'it, I>(&mut self, results: I) -> anyhow::Result<()>
    where
        I: IntoIterator<Item = &'it Box<ScanResult>>,
    {
        for result in results {
            let to = match result.last().as_address() {
                Some(address) => address,
                None => continue,
            };

            let from = match result.pointer.follow_default(self.handle)? {
                Some(address) => address,
                None => continue,
            };

            self.forward.insert(from, to);
            self.reverse.insert(to, from);
        }

        Ok(())
    }

    /// Perform the initial backwards pointer scan.
    pub fn scan(
        &mut self,
        ty: Type,
        needle: Address,
        results: &mut Vec<Box<ScanResult>>,
        progress: &mut impl PointerScanInitialProgress,
    ) -> anyhow::Result<()> {
        let Self {
            ref handle,
            ref cancel,
            ref reverse,
            ref mut visited,
            ref mut visited_count,
            max_depth,
            max_offset,
            ..
        } = *self;

        let mut queue = VecDeque::new();
        queue.push_back((needle, 0usize, SVec::new()));

        while let Some((n, depth, path)) = queue.pop_front() {
            if cancel.test() {
                break;
            }

            let it = reverse.range(..=n).rev();

            for (from, hit) in it {
                let offset = n.offset_of(*from);

                if !offset.is_within(max_offset) {
                    break;
                }

                let mut path = path.clone();
                path.push(offset);

                match visited.entry(*hit) {
                    hash_map::Entry::Occupied(mut e) => {
                        e.get_mut().push(path);
                        *visited_count += 1;
                        continue;
                    }
                    hash_map::Entry::Vacant(e) => {
                        e.insert(Vec::new());
                    }
                }

                match handle.find_location(*hit) {
                    Location::Module(module) => {
                        let mut path = path.clone();
                        let offset = hit.offset_of(module.range.base);
                        path.reverse();

                        let pointer = Pointer::new(
                            PointerBase::Module {
                                name: module.lossy_name.to_string(),
                                offset,
                            },
                            path.clone(),
                            Some(needle),
                        );

                        results.push(Box::new(ScanResult::new(pointer, Value::None(ty))));
                    }
                    // ignore thread stacks
                    Location::Thread(..) => {
                        continue;
                    }
                    _ => (),
                }

                let depth = depth + 1;

                if depth < max_depth {
                    queue.push_back((*hit, depth, path));
                }
            }

            progress.report(queue.len(), results.len())?;
        }

        Ok(())
    }

    /// Perform a "backreference scan", where we add all additional pointer
    /// paths that were skipped during the first scan.
    ///
    /// We accomplish this by going through all found results so far and
    /// traversing theirs paths, looking for backreferences that were omitted
    /// in the first step.
    pub fn backreference_scan(
        &mut self,
        ty: Type,
        needle: Address,
        results: &mut Vec<Box<ScanResult>>,
        progress: &mut impl PointerScanBackreferenceProgress,
    ) -> anyhow::Result<()> {
        // add all tailing pointers.
        let mut additions = Vec::new();

        for (i, result) in results.iter().enumerate() {
            if self.cancel.test() {
                break;
            }

            let mut address = result
                .pointer
                .base()
                .eval(self.handle)?
                .ok_or_else(|| anyhow!("should resolve to address"))?;

            let mut path = SVec::new();
            let mut it = result.pointer.offsets().iter().copied();

            while !self.cancel.test() {
                if let Some(extra) = self.visited.get(&address) {
                    for p in extra {
                        let mut path = path.clone();
                        path.extend(p.iter().rev().cloned());

                        let pointer =
                            Pointer::new(result.pointer.base().clone(), path, Some(needle));

                        additions.push(Box::new(ScanResult::new(pointer, Value::None(ty))));
                    }
                }

                if let Some(a) = self.forward.get(&address).copied() {
                    if let Some(o) = it.next() {
                        address = a.saturating_offset(o);
                        path.push(o);
                        continue;
                    }
                }

                break;
            }

            progress.report(results.len() - (i + 1), results.len() + additions.len())?;
        }

        results.append(&mut additions);
        Ok(())
    }
}
