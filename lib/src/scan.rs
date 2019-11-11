//! Predicates used for matching against memory.

use crate::{
    progress_reporter::ProgressReporter, Address, FilterExpr, Pointer, PointerBase, ProcessHandle,
    ProcessInfo as _, RawPointer, ScanResult, Size, Special, Test, Token, Type, Value, ValueExpr,
};
use anyhow::{anyhow, bail};
use crossbeam_queue::SegQueue;
use hashbrown::{HashMap, HashSet};
use serde::{Deserialize, Serialize};
use std::{
    cmp,
    convert::TryFrom as _,
    ops, slice,
    sync::mpsc,
    time::{Duration, Instant},
};

#[derive(Debug, Clone, Default)]
pub struct InitialScanConfig {
    pub modules_only: bool,
    pub tasks: Option<usize>,
    pub buffer_size: Option<usize>,
}

/// A trait to track the progress of processes.
pub trait ScanProgress {
    /// Report the total number of bytes to process.
    fn report_bytes(&mut self, bytes: Size) -> anyhow::Result<()>;

    /// Report that the process has progresses to the given percentage.
    fn report(&mut self, percentage: usize, results: u64) -> anyhow::Result<()>;
}

/// A scan responsible for finding results in memory.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Scan {
    /// Only scan for aligned values.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub aligned: Option<bool>,
    /// If this scan has a set of initial results.
    pub initial: bool,
    /// Scan results.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub results: Vec<Box<ScanResult>>,
    /// Value expression being used for resolving the current value.
    #[serde(default, skip_serializing_if = "ValueExpr::is_default")]
    pub value_expr: ValueExpr,
    /// Comments associated with each scan result.
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub comments: HashMap<RawPointer, String>,
}

impl Scan {
    /// Construct a new scan associated with a thread pool.
    pub fn new() -> Self {
        Self {
            aligned: None,
            initial: true,
            results: Vec::new(),
            value_expr: ValueExpr::Value,
            comments: HashMap::new(),
        }
    }

    /// Find a single result by address.
    pub fn find_result_by_address<'a>(&'a self, address: Address) -> Option<&'a Box<ScanResult>> {
        self.results.iter().find(|r| match r.pointer.address() {
            Some(a) if a == address => true,
            _ => false,
        })
    }

    /// Construct a scan from an already existing collection of results.
    pub fn from_results(results: Vec<Box<ScanResult>>) -> Self {
        Self {
            aligned: None,
            initial: false,
            results,
            value_expr: ValueExpr::Value,
            comments: HashMap::new(),
        }
    }

    /// Append from a different scan.
    pub fn append_scan(&mut self, mut other: Self) {
        self.results.append(&mut other.results);
        self.comments.extend(other.comments);
    }

    /// Load from a different scan.
    pub fn load_scan(&mut self, other: Self) {
        self.results = other.results;
        self.comments = other.comments;
        self.value_expr = other.value_expr;
        self.initial = other.initial;
        self.aligned = other.aligned;
    }

    /// Get a mutable reference to a single result.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut Box<ScanResult>> {
        self.results.get_mut(index)
    }

    /// Set the results associated with the scan.
    pub fn set(&mut self, results: Vec<Box<ScanResult>>) {
        self.results = results;
        self.comments.clear();
    }

    /// Append to results associated with the scan.
    pub fn append(&mut self, results: &mut Vec<Box<ScanResult>>) {
        self.results.append(results);
    }

    /// Push a scan result.
    pub fn push(&mut self, result: Box<ScanResult>) {
        self.results.push(result);
    }

    /// Clear the current scan.
    pub fn clear(&mut self) {
        self.results.clear();
        self.comments.clear();
        self.initial = true;
        self.value_expr = Default::default();
    }

    /// Get the length of the scan.
    pub fn len(&self) -> usize {
        self.results.len()
    }

    /// Check if the scan is empty.
    pub fn is_empty(&self) -> bool {
        self.results.is_empty()
    }

    /// Iterate over the scan.
    pub fn iter(&self) -> slice::Iter<'_, Box<ScanResult>> {
        self.results.iter()
    }

    /// Mutably iterate over the scan.
    pub fn iter_mut(&mut self) -> slice::IterMut<'_, Box<ScanResult>> {
        self.results.iter_mut()
    }

    /// Sort by the given predicate.
    pub fn sort_by(
        &mut self,
        compare: impl FnMut(&Box<ScanResult>, &Box<ScanResult>) -> cmp::Ordering,
    ) {
        self.results.sort_by(compare)
    }

    /// Only scan for values which are aligned.
    pub fn aligned(self) -> Self {
        Self {
            aligned: Some(true),
            ..self
        }
    }

    /// rescan a set of results.
    pub fn scan(
        &mut self,
        thread_pool: &rayon::ThreadPool,
        handle: &ProcessHandle,
        new_type: Option<Type>,
        cancel: Option<&Token>,
        progress: (impl ScanProgress + Send),
        filter: &FilterExpr,
    ) -> anyhow::Result<()> {
        handle.rescan_values(
            thread_pool,
            &mut self.results,
            new_type,
            cancel,
            progress,
            filter,
        )?;
        Ok(())
    }

    /// Scan for the given value in memory.
    ///
    /// TODO: handle unaligned regions.
    ///
    /// Errors raised in worker threads will be collected, and the last error propagated to the user.
    /// Any error causes the processing to cancel.
    ///
    /// # Errors raised in ScanProgress
    ///
    /// We can't just ignore errors which are raised by `ScanProgress`, since these might be important to the
    /// processing at hand.
    ///
    /// Therefore, any error raised by ScanProgress will be treated as any error raised from a worker thread. Only the last
    /// error will be propagated to the user.
    pub fn initial_scan(
        &mut self,
        thread_pool: &rayon::ThreadPool,
        handle: &ProcessHandle,
        filter: &FilterExpr,
        value_type: Type,
        cancel: Option<&Token>,
        progress: (impl ScanProgress + Send),
        config: InitialScanConfig,
    ) -> anyhow::Result<()> {
        const DEFAULT_BUFFER_SIZE: usize = 0x100_000;

        use crate::utils::IteratorExtension;

        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        let Scan {
            ref mut results,
            aligned,
            ..
        } = *self;

        results.clear();

        let mut last_error = None;

        let aligned = aligned.unwrap_or_else(|| value_type.is_default_aligned());
        let alignment = value_type
            .alignment(&handle.process)
            .ok_or_else(|| anyhow!("cannot scan for none"))?;
        let step_size = if aligned { alignment } else { 1 };
        let alignment = if aligned { Some(alignment) } else { None };

        let special = filter.special(&handle.process, value_type)?;
        let special = special.as_ref();

        let tasks = config
            .tasks
            .unwrap_or_else(|| thread_pool.current_num_threads());

        let buffer_size = config.buffer_size.unwrap_or(DEFAULT_BUFFER_SIZE);

        let mut bytes = Size::zero();

        let mut ranges = Vec::new();

        {
            let thread_stacks = handle
                .threads
                .iter()
                .map(|t| t.stack.clone())
                .collect::<HashSet<_>>();

            if config.modules_only {
                ranges.extend(handle.modules.iter().map(|m| m.range));
            } else {
                for m in handle.process.virtual_memory_regions().only_relevant() {
                    let m = m?;

                    if !thread_stacks.contains(&m.range) {
                        ranges.push(m.range);
                    }
                }
            }
        }

        let mut total = 0;

        let queue = SegQueue::new();

        for range in &ranges {
            if let Some(alignment) = alignment {
                if usize::try_from(range.base)? % alignment != 0 {
                    bail!(
                        "range {:?} is not supported with alignment `{}`",
                        range,
                        alignment
                    );
                }
            }

            bytes.add_assign(range.size)?;
            total += range.size.as_usize();

            let mut offset = 0usize;
            let range_size = range.size.as_usize();

            while offset < range_size && !cancel.test() {
                let len = usize::min(buffer_size, range_size - offset);
                let address = range.base.add(Size::try_from(offset)?)?;
                queue.push((address, len));
                offset += len;
            }
        }

        let queue = &queue;

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel::<Task>();

                for _ in 0..tasks {
                    let tx = tx.clone();

                    s.spawn(move |_| {
                        let start = Instant::now();
                        let result = work(Work {
                            handle,
                            tx: &tx,
                            queue,
                            filter,
                            value_type,
                            special,
                            alignment,
                            step_size,
                            buffer_size,
                            cancel,
                        });

                        let now = Instant::now();
                        let duration = now.duration_since(start);

                        tx.send(Task::Done(result, duration, now))
                            .expect("send done failed");
                    });
                }

                let mut reporter = ProgressReporter::new(progress, total, cancel, &mut last_error);

                reporter.report_bytes(bytes);

                let mut hits = 0u64;

                let mut task_count = tasks;

                while task_count > 0 {
                    match rx.recv().expect("channel closed") {
                        Task::Done(result, ..) => {
                            if let Some(mut r) = reporter.eval(result) {
                                results.append(&mut r);
                            }

                            task_count -= 1;

                            if task_count == 0 {
                                break;
                            }
                        }
                        Task::Tick(bytes, c, ..) => {
                            hits += c;
                            reporter.tick_n(bytes, hits);
                        }
                    }
                }

                Ok::<(), anyhow::Error>(())
            })
        })?;

        if let Some(e) = last_error {
            return Err(e);
        }

        return Ok(());

        enum Task {
            Done(anyhow::Result<Vec<Box<ScanResult>>>, Duration, Instant),
            Tick(usize, u64, Duration, Instant),
        }

        struct Work<'a> {
            handle: &'a ProcessHandle,
            tx: &'a mpsc::Sender<Task>,
            queue: &'a SegQueue<(Address, usize)>,
            filter: &'a FilterExpr,
            value_type: Type,
            special: Option<&'a Special>,
            alignment: Option<usize>,
            step_size: usize,
            buffer_size: usize,
            cancel: &'a Token,
        }

        #[inline(always)]
        fn work(work: Work<'_>) -> anyhow::Result<Vec<Box<ScanResult>>> {
            let Work {
                handle,
                tx,
                queue,
                filter,
                value_type,
                special,
                alignment,
                step_size,
                buffer_size,
                cancel,
            } = work;

            let mut results = Vec::new();

            let mut buffer = vec![0u8; buffer_size];
            let none = Value::default();
            let none = &none;

            while let Ok((address, len)) = queue.pop() {
                if cancel.test() {
                    return Ok(results);
                }

                let len = usize::min(buffer.len(), len);
                let data = &mut buffer[..len];

                let start = Instant::now();
                let mut hits = 0;

                let len = handle.process.read_process_memory(address, data)?;

                if len == 0 {
                    continue;
                }

                let data = &data[..len];

                process_one(ProcessOne {
                    handle,
                    filter,
                    value_type,
                    results: &mut results,
                    hits: &mut hits,
                    base: address,
                    data,
                    step_size,
                    none,
                    alignment,
                    special,
                    cancel,
                })?;

                let duration = Instant::now().duration_since(start);
                tx.send(Task::Tick(data.len(), hits, duration, Instant::now()))
                    .expect("send tick failed");
            }

            Ok(results)
        };

        struct ProcessOne<'a> {
            handle: &'a ProcessHandle,
            filter: &'a FilterExpr,
            value_type: Type,
            results: &'a mut Vec<Box<ScanResult>>,
            hits: &'a mut u64,
            base: Address,
            data: &'a [u8],
            step_size: usize,
            none: &'a Value,
            alignment: Option<usize>,
            special: Option<&'a Special>,
            cancel: &'a Token,
        }

        #[inline(always)]
        fn process_one(process_one: ProcessOne<'_>) -> anyhow::Result<()> {
            let ProcessOne {
                handle,
                filter,
                value_type,
                results,
                hits,
                base,
                data,
                step_size,
                none,
                alignment,
                special,
                cancel,
                ..
            } = process_one;

            let mut offset = match special {
                Some(special) => match special.test(data) {
                    Some(offset) => offset,
                    None => return Ok(()),
                },
                None => 0usize,
            };

            if let Some(size) = alignment {
                align(&mut offset, size);
            }

            let mut last_address = None;

            while offset < data.len() && !cancel.test() {
                let address = base.add(Size::try_from(offset)?)?;
                let mut pointer =
                    Pointer::new(PointerBase::Address { address }, vec![], Some(address));
                let mut proxy = handle.address_proxy(&pointer);

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

                if let Test::True = filter.test(none, none, value_type, &mut proxy)? {
                    *hits += 1;
                    let (value, advance) = proxy.eval(value_type)?;
                    *pointer.base_mut() = handle.address_to_pointer_base(address)?;
                    *pointer.last_address_mut() = Some(address);
                    results.push(Box::new(ScanResult::new(pointer, value)));

                    if let Some(advance) = advance {
                        if advance == 0 {
                            bail!("BUG: attempt to advance by 0 bytes");
                        }

                        offset += advance;
                    } else {
                        offset += step_size;
                    }
                } else {
                    offset += step_size;
                }

                if offset >= data.len() {
                    break;
                }

                if let Some(special) = special {
                    offset += match special.test(&data[offset..]) {
                        Some(o) => o,
                        None => return Ok(()),
                    };

                    if let Some(size) = alignment {
                        align(&mut offset, size);
                    }
                }
            }

            Ok(())
        }

        fn align(to_align: &mut usize, alignment: usize) {
            let rem = *to_align % alignment;

            if rem > 0 {
                *to_align += alignment - rem;
            }
        }
    }
}

impl<'a> IntoIterator for &'a Scan {
    type Item = &'a Box<ScanResult>;
    type IntoIter = slice::Iter<'a, Box<ScanResult>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut Scan {
    type Item = &'a mut Box<ScanResult>;
    type IntoIter = slice::IterMut<'a, Box<ScanResult>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<I: slice::SliceIndex<[Box<ScanResult>]>> ops::Index<I> for Scan {
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        ops::Index::index(&self.results, index)
    }
}

impl<I: slice::SliceIndex<[Box<ScanResult>]>> ops::IndexMut<I> for Scan {
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        ops::IndexMut::index_mut(&mut self.results, index)
    }
}

impl Default for Scan {
    fn default() -> Self {
        Self::new()
    }
}
