//! Predicates used for matching against memory.

use crate::{
    error::Error, filter, Address, AddressRange, Pointer, PointerBase, ProcessHandle, Size, Token,
    Type, Value,
};
use serde::{Deserialize, Serialize};
use std::{
    convert::TryFrom as _,
    ops, slice,
    sync::{mpsc, Arc},
    time::{Duration, Instant},
};

#[derive(Debug, Clone, Default)]
pub struct InitialScanConfig {
    pub modules_only: bool,
    pub tasks: Option<usize>,
}

/// A trait to track the progress of processes.
pub trait Progress {
    /// Report the total number of bytes to process.
    fn report_bytes(&mut self, bytes: Size) -> anyhow::Result<()>;

    /// Report that the process has progresses to the given percentage.
    fn report(&mut self, percentage: usize, results: u64) -> anyhow::Result<()>;
}

/// A scan responsible for finding results in memory.
pub struct Scan {
    /// Thread pool this scan uses.
    pub thread_pool: Arc<rayon::ThreadPool>,
    /// Only scan for aligned values.
    pub aligned: Option<bool>,
    /// If this scan has a set of initial results.
    pub initial: bool,
    /// Last type used during a scan.
    pub last_type: Option<Type>,
    /// Scan results.
    pub results: Vec<Box<ScanResult>>,
}

impl Scan {
    /// Construct a new scan associated with a thread pool.
    pub fn new(thread_pool: &Arc<rayon::ThreadPool>) -> Self {
        Self {
            aligned: None,
            initial: true,
            last_type: None,
            thread_pool: Arc::clone(thread_pool),
            results: Vec::new(),
        }
    }

    /// Construct a scan from an already existing collection of results.
    pub fn from_results(
        thread_pool: &Arc<rayon::ThreadPool>,
        last_type: Type,
        results: Vec<Box<ScanResult>>,
    ) -> Self {
        Self {
            aligned: None,
            initial: false,
            last_type: Some(last_type),
            thread_pool: Arc::clone(thread_pool),
            results,
        }
    }

    /// Remove the given entry by address.
    ///
    /// Returns `true` if the address was removed, `false` otherwise.
    pub fn remove_by_pointer(&mut self, pointer: &Pointer) -> bool {
        if let Some(index) = self.results.iter().position(|r| r.pointer == *pointer) {
            self.results.swap_remove(index);
            true
        } else {
            false
        }
    }

    /// Clear the scan.
    pub fn clear(&mut self) {
        self.results.clear();
    }

    /// Get the length of the current scan.
    pub fn len(&self) -> usize {
        self.results.len()
    }

    /// Push the given scan result.
    pub fn push(&mut self, result: Box<ScanResult>) {
        self.results.push(result);
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
        handle: &ProcessHandle,
        filter: &filter::Filter,
        cancel: Option<&Token>,
        progress: (impl Progress + Send),
    ) -> anyhow::Result<()> {
        let mut results = Vec::new();

        handle.rescan_values(
            &*self.thread_pool,
            &self.results,
            &mut results,
            filter,
            cancel,
            progress,
        )?;

        self.last_type = Some(filter.ty);
        self.results = results;
        Ok(())
    }

    /// Scan for the given value in memory.
    ///
    /// TODO: handle unaligned regions.
    ///
    /// Errors raised in worker threads will be collected, and the last error propagated to the user.
    /// Any error causes the processing to cancel.
    ///
    /// # Errors raised in Progress
    ///
    /// We can't just ignore errors which are raised by `scan::Progress`, since these might be important to the
    /// processing at hand.
    ///
    /// Therefore, any error raised by Progress will be treated as any error raised from a worker thread. Only the last
    /// error will be propagated to the user.
    pub fn initial_scan(
        &mut self,
        handle: &ProcessHandle,
        filter: &filter::Filter,
        cancel: Option<&Token>,
        progress: (impl Progress + Send),
        config: InitialScanConfig,
    ) -> anyhow::Result<()> {
        const BUFFER_SIZE: usize = 0x10_000_000;

        use crate::utils::IteratorExtension;

        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        self.last_type = Some(filter.ty);

        let Scan {
            ref mut results,
            aligned,
            ref thread_pool,
            ..
        } = *self;

        results.clear();

        let mut last_error = None;

        let type_size = filter.ty.size(&handle.process);
        let aligned = aligned.unwrap_or(filter.ty.is_default_aligned());

        let special = filter.special(&handle.process)?;
        let special = special.as_ref();

        let tasks = config
            .tasks
            .unwrap_or_else(|| thread_pool.current_num_threads());

        let mut bytes = Size::zero();

        let mut ranges = Vec::new();

        if !config.modules_only {
            ranges.extend(
                handle
                    .process
                    .virtual_memory_regions()
                    .only_relevant()
                    .map(|m| m.map(|m| m.range))
                    .collect::<Result<Vec<_>, Error>>()?,
            );
        }

        ranges.extend(handle.modules.iter().map(|m| m.range.clone()));

        // the size of a chunk to assign to each task.
        let chunk = (ranges.len() + (tasks - 1)) / tasks;

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::channel::<Task>();

                let mut total = 0;
                let mut task_count = 0;

                for ranges in ranges.chunks(chunk) {
                    task_count += 1;

                    for r in ranges {
                        bytes.add_assign(r.size)?;
                        total += r.size.as_usize();
                    }

                    let tx = tx.clone();

                    s.spawn(move |_| {
                        let start = Instant::now();
                        let result = work(
                            handle, &tx, ranges, filter, special, aligned, type_size, cancel,
                        );

                        let now = Instant::now();
                        let duration = now.duration_since(start);
                        tx.send(Task::Done(result, duration, now))
                            .expect("send done failed");
                    });
                }

                let mut reporter = Reporter::new(progress, total, cancel, &mut last_error);

                reporter.report_bytes(bytes);

                let mut hits = 0u64;

                while task_count > 0 && !cancel.test() {
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

        #[inline(always)]
        fn work(
            handle: &ProcessHandle,
            tx: &mpsc::Sender<Task>,
            ranges: &[AddressRange],
            filter: &filter::Filter,
            special: Option<&filter::Special>,
            aligned: bool,
            type_size: usize,
            cancel: &Token,
        ) -> anyhow::Result<Vec<Box<ScanResult>>> {
            let mut results = Vec::new();

            let mut data = vec![0u8; BUFFER_SIZE];
            let none = Value::default();

            for range in ranges {
                if cancel.test() {
                    return Ok(results);
                }

                let mut offset = 0usize;
                let range_size = range.size.as_usize();

                while offset < range_size {
                    let data = {
                        let len = usize::min(data.len(), range_size - offset);
                        &mut data[..len]
                    };

                    let base = range.base.add(Size::try_from(offset)?)?;

                    let start = Instant::now();
                    let mut hits = 0;

                    if handle.process.read_process_memory(base, data)? {
                        process_one(
                            handle,
                            filter,
                            &mut results,
                            &mut hits,
                            base,
                            &data,
                            type_size,
                            &none,
                            aligned,
                            special,
                        )?;
                    }

                    offset += data.len();

                    let duration = Instant::now().duration_since(start);
                    tx.send(Task::Tick(data.len(), hits, duration, Instant::now()))
                        .expect("send tick failed");
                }
            }

            return Ok(results);
        };

        #[inline(always)]
        fn process_one(
            handle: &ProcessHandle,
            filter: &filter::Filter,
            results: &mut Vec<Box<ScanResult>>,
            hits: &mut u64,
            base: Address,
            data: &[u8],
            type_size: usize,
            none: &Value,
            aligned: bool,
            special: Option<&filter::Special>,
        ) -> anyhow::Result<()> {
            let mut inner_offset = match special {
                Some(special) => match special.test(data) {
                    Some(inner_offset) => inner_offset,
                    None => return Ok(()),
                },
                None => 0usize,
            };

            if aligned {
                align(&mut inner_offset, type_size);
            }

            while inner_offset < data.len() {
                let address = base.add(Size::try_from(inner_offset)?)?;
                let mut pointer = Pointer::new(PointerBase::Address { address }, vec![]);
                let proxy = handle.address_proxy(&pointer);

                if filter.test(none, proxy)? {
                    *hits += 1;
                    let value = proxy.eval(filter.ty)?;
                    pointer.base = handle.address_to_pointer_base(address)?;
                    results.push(Box::new(ScanResult { pointer, value }));
                }

                if let Some(special) = special {
                    if aligned {
                        inner_offset += type_size;
                    } else {
                        inner_offset += 1;
                    }

                    inner_offset += match special.test(&data[inner_offset..]) {
                        Some(o) => o,
                        None => return Ok(()),
                    };

                    if aligned {
                        align(&mut inner_offset, type_size);
                    }
                } else {
                    if aligned {
                        inner_offset += type_size;
                    } else {
                        inner_offset += 1;
                    }
                };
            }

            Ok(())
        }

        fn align(to_align: &mut usize, alignment: usize) {
            let rem = *to_align % alignment;

            if rem > 0 {
                *to_align -= rem;
            }
        }
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

pub struct Reporter<'token, 'err, P> {
    progress: P,
    /// Current progress.
    current: usize,
    /// Last percentage encountered.
    last_percentage: usize,
    /// Total.
    total: usize,
    /// Whether to report progress or not.
    token: &'token Token,
    /// Last error captured from the progress.
    last_err: &'err mut Option<anyhow::Error>,
}

impl<'token, 'err, P> Reporter<'token, 'err, P> {
    /// Create a new reporter.
    pub fn new(
        progress: P,
        total: usize,
        token: &'token Token,
        last_err: &'err mut Option<anyhow::Error>,
    ) -> Reporter<'token, 'err, P> {
        Reporter {
            progress,
            current: 0,
            last_percentage: 0,
            total,
            token,
            last_err,
        }
    }

    /// Evaluate the given result.
    pub fn eval<T>(&mut self, result: anyhow::Result<T>) -> Option<T> {
        match result {
            Ok(value) => Some(value),
            Err(e) => {
                self.token.set();
                *self.last_err = Some(e);
                None
            }
        }
    }

    /// Report a number of bytes.
    pub fn report_bytes(&mut self, bytes: Size)
    where
        P: Progress,
    {
        if let Err(e) = self.progress.report_bytes(bytes) {
            *self.last_err = Some(e);
            self.token.set();
        }
    }

    /// Tick a single task.
    pub fn tick(&mut self, results: u64)
    where
        P: Progress,
    {
        self.tick_n(1, results);
    }

    /// Tick a single task.
    pub fn tick_n(&mut self, count: usize, results: u64)
    where
        P: Progress,
    {
        self.current += count;
        let p = (self.current * 100) / self.total;

        if p > self.last_percentage {
            if let Err(e) = self.progress.report(p, results) {
                *self.last_err = Some(e);
                self.token.set();
            }

            self.last_percentage = p;
        }
    }

    /// Are we done?
    pub fn is_done(&self) -> bool {
        self.current >= self.total
    }
}

/// A single scan result.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScanResult {
    /// Address where the scanned value lives.
    pub pointer: Pointer,
    /// Value from last scan.
    pub value: Value,
}
