//! Predicates used for matching against memory.

use crate::{
    error::Error, filter, Address, Pointer, PointerBase, ProcessHandle, Size, Test, Token, Type,
    Value, ValueExpr,
};
use crossbeam_queue::SegQueue;
use serde::{Deserialize, Serialize};
use std::{
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
}

impl Scan {
    /// Construct a new scan associated with a thread pool.
    pub fn new() -> Self {
        Self {
            aligned: None,
            initial: true,
            results: Vec::new(),
            value_expr: ValueExpr::Value,
        }
    }

    /// Construct a scan from an already existing collection of results.
    pub fn from_results(results: Vec<Box<ScanResult>>) -> Self {
        Self {
            aligned: None,
            initial: false,
            results,
            value_expr: ValueExpr::Value,
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
        thread_pool: &rayon::ThreadPool,
        handle: &ProcessHandle,
        new_type: Option<Type>,
        cancel: Option<&Token>,
        progress: (impl ScanProgress + Send),
        filter: &filter::Filter,
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
        filter: &filter::Filter,
        ty: Type,
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

        let type_size = ty.size(&handle.process);
        let aligned = aligned.unwrap_or(ty.is_default_aligned());

        let special = filter.special(&handle.process, ty)?;
        let special = special.as_ref();

        let tasks = config
            .tasks
            .unwrap_or_else(|| thread_pool.current_num_threads());

        let buffer_size = config.buffer_size.unwrap_or(DEFAULT_BUFFER_SIZE);

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

        let mut total = 0;

        let queue = SegQueue::new();

        for range in &ranges {
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
                        let result = work(
                            handle,
                            &tx,
                            queue,
                            filter,
                            ty,
                            special,
                            aligned,
                            type_size,
                            buffer_size,
                            cancel,
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

        #[inline(always)]
        fn work(
            handle: &ProcessHandle,
            tx: &mpsc::Sender<Task>,
            queue: &SegQueue<(Address, usize)>,
            filter: &filter::Filter,
            ty: Type,
            special: Option<&filter::Special>,
            aligned: bool,
            type_size: usize,
            buffer_size: usize,
            cancel: &Token,
        ) -> anyhow::Result<Vec<Box<ScanResult>>> {
            let mut results = Vec::new();

            let mut buffer = vec![0u8; buffer_size];
            let none = Value::default();

            while let Ok((address, len)) = queue.pop() {
                if cancel.test() {
                    return Ok(results);
                }

                let len = usize::min(buffer.len(), len);
                let data = &mut buffer[..len];

                let start = Instant::now();
                let mut hits = 0;

                if handle.process.read_process_memory(address, data)? {
                    process_one(
                        handle,
                        filter,
                        ty,
                        &mut results,
                        &mut hits,
                        address,
                        &data,
                        type_size,
                        &none,
                        aligned,
                        special,
                        cancel,
                    )?;
                }

                let duration = Instant::now().duration_since(start);
                tx.send(Task::Tick(data.len(), hits, duration, Instant::now()))
                    .expect("send tick failed");
            }

            return Ok(results);
        };

        #[inline(always)]
        fn process_one(
            handle: &ProcessHandle,
            filter: &filter::Filter,
            ty: Type,
            results: &mut Vec<Box<ScanResult>>,
            hits: &mut u64,
            base: Address,
            data: &[u8],
            type_size: usize,
            none: &Value,
            aligned: bool,
            special: Option<&filter::Special>,
            cancel: &Token,
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

            let mut last_address = None;

            while inner_offset < data.len() && !cancel.test() {
                let address = base.add(Size::try_from(inner_offset)?)?;
                let mut pointer =
                    Pointer::new(PointerBase::Address { address }, vec![], Some(address));
                let mut proxy = handle.address_proxy(&pointer);

                if let Some(last_address) = last_address {
                    assert!(address > last_address, "address must always increase");
                }

                last_address = Some(address);

                if let Test::True = filter.test(ty, none, none, &mut proxy)? {
                    *hits += 1;
                    let value = proxy.eval(ty)?;
                    pointer.base = handle.address_to_pointer_base(address)?;
                    pointer.last_address = Some(address);
                    results.push(Box::new(ScanResult::new(pointer, value)));
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

impl Default for Scan {
    fn default() -> Self {
        Self::new()
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
        P: ScanProgress,
    {
        if let Err(e) = self.progress.report_bytes(bytes) {
            *self.last_err = Some(e);
            self.token.set();
        }
    }

    /// Tick a single task.
    pub fn tick(&mut self, results: u64)
    where
        P: ScanProgress,
    {
        self.tick_n(1, results);
    }

    /// Tick a single task.
    pub fn tick_n(&mut self, count: usize, results: u64)
    where
        P: ScanProgress,
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
    /// The initial value in the scan. Is not rotated out.
    pub initial: Value,
    /// Subsequent values. Might be rotated out in length sequence.
    pub last: Option<Value>,
}

impl ScanResult {
    /// Construct a new scan result where the last value is specified.
    pub fn new(pointer: Pointer, value: Value) -> Self {
        Self {
            pointer,
            initial: value,
            last: None,
        }
    }

    /// Access the initial value of this result.
    pub fn initial(&self) -> &Value {
        &self.initial
    }

    /// Access the last value of the scan.
    pub fn last(&self) -> &Value {
        self.last.as_ref().unwrap_or(&self.initial)
    }
}
