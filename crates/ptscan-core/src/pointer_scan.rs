use crate::{Address, Base, Location, Offset, Pointer, ProcessHandle, Size, Token, Type};
use anyhow::anyhow;
use hashbrown::{hash_map, HashMap};
use rayon::slice::ParallelSliceMut as _;
use std::{collections::VecDeque, convert::TryFrom as _};

type SVec = Vec<Offset>;

/// The parent of a node reached straight from the needle.
const ROOT: u32 = u32::MAX;

/// A pointer reached by [`PointerScan::scan`] for the first time.
///
/// Its path back to the needle is its `offset` followed by the path of its
/// `parent`, so every path is shared by the paths that extend it.
#[derive(Clone, Copy)]
struct Node {
    /// Index of the node this pointer was reached from, or [`ROOT`].
    parent: u32,
    /// Number of offsets in the path from this node to the needle.
    len: u32,
    /// Offset from what this pointer points to, to its parent.
    offset: Offset,
}

/// Another path to an already visited node, through `parent` (or [`ROOT`])
/// with `offset`.
#[derive(Clone, Copy)]
struct Alternative {
    node: u32,
    parent: u32,
    offset: Offset,
}

/// Push the offsets of the path from the node at `index` to the needle onto
/// `path`.
///
/// Parents are always pushed before their children, so this terminates.
fn extend_path(nodes: &[Node], mut index: u32, path: &mut SVec) {
    while let Some(node) = nodes.get(index as usize) {
        path.push(node.offset);
        index = node.parent;
    }
}

/// Number of offsets in the path from the node at `index` to the needle.
fn path_len(nodes: &[Node], index: u32) -> usize {
    nodes
        .get(index as usize)
        .map_or(0, |node| node.len as usize)
}

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
/// max_paths: unlimited
pub struct PointerScan<'a> {
    thread_pool: &'a rayon::ThreadPool,
    handle: &'a ProcessHandle,
    cancel: &'a Token,
    /// Every pointer as `(from, to)`, sorted with one entry per `from`.
    pub forward: Vec<(Address, Address)>,
    /// Every pointer as `(to, from)`, sorted, so that all pointers to a
    /// shared target are kept.
    pub reverse: Vec<(Address, Address)>,
    /// The node of every pointer visited, by address.
    visited: HashMap<Address, u32>,
    /// Every pointer visited, indexed by `visited`.
    nodes: Vec<Node>,
    /// Every path to a pointer after the first, in the order visited.
    alternatives: Vec<Alternative>,
    pub visited_count: usize,
    pub max_offset: Size,
    pub max_depth: usize,
    /// Most paths stored in the results passed to the scans.
    pub max_paths: usize,
    truncated: bool,
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
            nodes: Vec::new(),
            alternatives: Vec::new(),
            visited_count: 0,
            max_offset: 0x1000u32.into(),
            max_depth: 7,
            max_paths: usize::MAX,
            truncated: false,
        }
    }

    /// Build forward and backward references.
    pub fn build_references<'v, A, V>(&mut self, addresses: A, values: V) -> anyhow::Result<()>
    where
        A: IntoIterator<Item = Address>,
        V: IntoIterator<Item = dynamicvec::Accessor<'v, Type>>,
    {
        let mut forward = addresses
            .into_iter()
            .zip(values)
            .filter_map(|(from, value)| Some((from, value.read().as_address()?)))
            .collect::<Vec<_>>();

        self.thread_pool.install(|| {
            // Sorted initial scan output needs neither the sort nor the dedup.
            if !forward.is_sorted_by(|a, b| a.0 < b.0) {
                forward.par_sort_unstable();
                forward.dedup_by_key(|&mut (from, _)| from);
            }

            let mut reverse = forward
                .iter()
                .map(|&(from, to)| (to, from))
                .collect::<Vec<_>>();
            reverse.par_sort_unstable();

            self.forward = forward;
            self.reverse = reverse;
        });

        Ok(())
    }

    /// A path was dropped because the results held [`Self::max_paths`].
    pub fn truncated(&self) -> bool {
        self.truncated
    }

    fn follow(&self, from: Address) -> Option<Address> {
        let index = self.forward.partition_point(|&(f, _)| f < from);

        match self.forward.get(index) {
            Some(&(f, to)) if f == from => Some(to),
            _ => None,
        }
    }

    /// Perform the initial backwards pointer scan.
    pub fn scan(
        &mut self,
        needle: Address,
        pointers: &mut Vec<Pointer>,
        progress: &mut impl PointerScanInitialProgress,
    ) -> anyhow::Result<()> {
        let Self {
            handle,
            cancel,
            ref reverse,
            ref mut visited,
            ref mut nodes,
            ref mut alternatives,
            ref mut visited_count,
            max_depth,
            max_offset,
            max_paths,
            ref mut truncated,
            ..
        } = *self;

        let mut queue = VecDeque::new();
        queue.push_back((needle, 0usize, ROOT));

        while let Some((n, depth, parent)) = queue.pop_front() {
            if cancel.test() {
                break;
            }

            let end = reverse.partition_point(|&(to, _)| to <= n);
            let it = reverse.get(..end).unwrap_or_default().iter().rev();

            for &(to, hit) in it {
                let offset = n.offset_of(to);

                if !offset.is_within(max_offset) {
                    break;
                }

                let node = match visited.entry(hit) {
                    hash_map::Entry::Occupied(e) => {
                        alternatives.push(Alternative {
                            node: *e.get(),
                            parent,
                            offset,
                        });
                        *visited_count = visited_count.saturating_add(1);
                        continue;
                    }
                    hash_map::Entry::Vacant(e) => {
                        let node = u32::try_from(nodes.len())
                            .ok()
                            .filter(|&node| node != ROOT)
                            .ok_or_else(|| anyhow!("pointer scan visited too many pointers"))?;

                        nodes.push(Node {
                            parent,
                            len: u32::try_from(depth.saturating_add(1)).unwrap_or(u32::MAX),
                            offset,
                        });

                        *e.insert(node)
                    }
                };

                match handle.find_location(hit) {
                    Location::Module(module) => {
                        if pointers.len() >= max_paths {
                            *truncated = true;
                            return Ok(());
                        }

                        let mut path = SVec::with_capacity(depth.saturating_add(1));
                        extend_path(nodes, node, &mut path);

                        let pointer = Pointer::new(
                            Base::Module {
                                id: module.id,
                                offset: hit.offset_of(module.range.base),
                            },
                            path,
                        );

                        pointers.push(pointer);
                    }
                    // ignore thread stacks
                    Location::Thread(..) => {
                        continue;
                    }
                    _ => (),
                }

                let depth = depth.saturating_add(1);

                if depth < max_depth {
                    queue.push_back((hit, depth, node));
                }
            }

            progress.report(queue.len(), pointers.len())?;
        }

        Ok(())
    }

    /// Walk the path of `pointer` from its base, calling `f` at each pointer
    /// along it with the offsets taken so far and the alternative paths to
    /// that pointer.
    fn walk_alternatives(
        &self,
        pointer: &Pointer,
        mut f: impl FnMut(&[Offset], &[Alternative]),
    ) -> anyhow::Result<()> {
        let mut address = pointer
            .base
            .eval(self.handle)?
            .ok_or_else(|| anyhow!("could not resolve pointer base to address"))?;

        let mut path = SVec::new();
        let mut it = pointer.offsets.iter().copied();

        while !self.cancel.test() {
            if let Some(&node) = self.visited.get(&address) {
                let start = self.alternatives.partition_point(|a| a.node < node);
                let extra = self.alternatives.get(start..).unwrap_or_default();
                let end = extra.partition_point(|a| a.node == node);
                f(&path, extra.get(..end).unwrap_or_default());
            }

            if let Some(a) = self.follow(address) {
                if let Some(o) = it.next() {
                    address = a.saturating_offset(o);
                    path.push(o);
                    continue;
                }
            }

            break;
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
        pointers: &mut Vec<Pointer>,
        progress: &mut impl PointerScanBackreferenceProgress,
    ) -> anyhow::Result<()> {
        if self.truncated {
            return Ok(());
        }

        // Group the alternatives by node, keeping the order they were found
        // in, so that each node's are a range.
        let alternatives = &mut self.alternatives;
        self.thread_pool
            .install(|| alternatives.par_sort_by_key(|alternative| alternative.node));

        // Count the additions first so that they can be added in place, with
        // no room to spare.
        let mut count = 0usize;

        for pointer in pointers.iter() {
            if self.cancel.test() {
                break;
            }

            self.walk_alternatives(pointer, |_, extra| count += extra.len())?;
        }

        let len = pointers.len();
        pointers.reserve_exact(count.min(self.max_paths.saturating_sub(len)));
        let mut truncated = false;

        // add all tailing pointers.
        for i in 0..len {
            if self.cancel.test() {
                break;
            }

            let pointer = match pointers.get(i) {
                Some(pointer) => pointer.clone(),
                None => break,
            };

            self.walk_alternatives(&pointer, |path, extra| {
                for alternative in extra {
                    if truncated || pointers.len() >= self.max_paths {
                        truncated = true;
                        return;
                    }

                    let capacity = path
                        .len()
                        .saturating_add(1)
                        .saturating_add(path_len(&self.nodes, alternative.parent));
                    let mut offsets = SVec::with_capacity(capacity);
                    offsets.extend_from_slice(path);
                    offsets.push(alternative.offset);
                    extend_path(&self.nodes, alternative.parent, &mut offsets);
                    pointers.push(Pointer::new(pointer.base.clone(), offsets));
                }
            })?;

            if truncated {
                break;
            }

            progress.report(len.saturating_sub(i.saturating_add(1)), pointers.len())?;
        }

        self.truncated = truncated;
        Ok(())
    }
}
