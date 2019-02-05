//! Predicates used for matching against memory.

use crate::{
    address::{Address, Size},
    filter, pointer,
    process::{MemoryInformation, Process},
    special::Special,
    thread_buffers,
    values::Values,
    watch::Watch,
    Location, ProcessHandle, Token, Type, Value,
};
use std::{
    convert::TryFrom,
    io,
    sync::{mpsc, Arc},
};

/// A trait to track the progress of processes.
pub trait Progress {
    /// Report the total number of bytes to process.
    fn report_bytes(&mut self, bytes: usize) -> Result<(), failure::Error>;

    /// Report that the process has progresses to the given percentage.
    fn report(&mut self, percentage: usize, results: u64) -> Result<(), failure::Error>;
}

/// A scan responsible for finding results in memory.
pub struct Scan {
    /// Thread pool this scan uses.
    pub thread_pool: Arc<rayon::ThreadPool>,
    /// Only scan for aligned values.
    pub aligned: bool,
    /// If this scan has a set of initial results.
    pub initial: bool,
    /// Addresses in the scan.
    pub addresses: Vec<Address>,
    /// Values in the scan.
    pub values: Values,
}

impl Scan {
    /// Construct a new scan associated with a thread pool.
    pub fn new(thread_pool: &Arc<rayon::ThreadPool>) -> Self {
        Self {
            aligned: false,
            initial: false,
            thread_pool: Arc::clone(thread_pool),
            addresses: Vec::new(),
            values: Values::new(),
        }
    }

    /// Clear the scan.
    pub fn clear(&mut self) {
        self.addresses.clear();
        self.values.clear();
    }

    /// Get the length of the current scan.
    pub fn len(&self) -> usize {
        self.addresses.len()
    }

    /// Get the result at the given location.
    pub fn get(&self, index: usize) -> Option<ScanResult> {
        let address = self.addresses.get(index)?;
        let value = self.values.get(index)?;

        Some(ScanResult {
            address: *address,
            value,
        })
    }

    /// Push the given scan result.
    pub fn push(&mut self, result: ScanResult) {
        self.addresses.push(result.address);
        self.values.push(result.value);
    }

    /// Create an iterator over the scan.
    pub fn iter<'a>(&'a self) -> Iter<'a> {
        Iter { scan: self, pos: 0 }
    }

    /// Only scan for values which are aligned.
    pub fn aligned(self) -> Self {
        Self {
            aligned: true,
            ..self
        }
    }

    /// rescan a set of results.
    pub fn rescan(
        &mut self,
        process: &Process,
        filter: &filter::Filter,
        cancel: Option<&Token>,
        progress: (impl Progress + Send),
    ) -> Result<(), failure::Error> {
        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        if self.addresses.is_empty() {
            return Ok(());
        }

        let values = &mut self.values;
        let addresses = &self.addresses;

        let len = self.addresses.len();
        let buffers = thread_buffers::ThreadBuffers::new();

        let mut last_error = None;

        self.thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::sync_channel(1024);

                let mut reporter = Reporter::new(progress, len, cancel, &mut last_error);

                for (mut value, address) in values.iter_mut().zip(addresses.iter().cloned()) {
                    let tx = tx.clone();
                    let buffers = &buffers;

                    s.spawn(move |_| {
                        if cancel.test() {
                            tx.send(Ok(())).expect("closed channel");
                            return;
                        }

                        let mut work = || {
                            let ty = value.ty();
                            let mut buf = buffers.get_mut(ty.size())?;
                            let new_value = process.read_memory_of_type(address, ty, &mut *buf)?;

                            if filter.test(Some(value), &new_value) {
                                value.set(new_value);
                            } else {
                                value.clear();
                            }

                            Ok::<_, failure::Error>(())
                        };

                        tx.send(work()).expect("closed channel");
                    });
                }

                let mut results = 0u64;

                while !reporter.is_done() {
                    results += 1;
                    reporter.eval(rx.recv().expect("closed channel"));
                    reporter.tick(results);
                }
            });
        });

        let mut addresses = Vec::new();
        let mut values = Values::new();

        for (v, address) in self
            .values
            .iter()
            .into_iter()
            .zip(self.addresses.iter().cloned())
        {
            let v = match v {
                Value::None => continue,
                v => v,
            };

            addresses.push(address);
            values.push(v);
        }

        self.addresses = addresses;
        self.values = values;

        if let Some(e) = last_error {
            return Err(e);
        }

        if cancel.test() {
            return Err(failure::format_err!("scan cancelled"));
        }

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
        process: &Process,
        filter: &filter::Filter,
        cancel: Option<&Token>,
        progress: (impl Progress + Send),
    ) -> Result<(), failure::Error> {
        use crate::utils::IteratorExtension;

        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        let size = match filter.ty().size() {
            0 => {
                return Err(failure::format_err!(
                    "can't perform initial scan with unsized filter"
                ));
            }
            size => size,
        };

        let ty = match filter.ty() {
            Type::None => failure::bail!("no unique type in filter"),
            other => other,
        };

        let buffer_size = 0x1000;

        let buffers = thread_buffers::ThreadBuffers::new();

        let Scan {
            ref mut addresses,
            ref mut values,
            aligned,
            ref thread_pool,
            ..
        } = *self;

        // if true, threads should stop working
        let special = filter.special();
        let mut last_error = None;

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::sync_channel(1024);
                let mut total = 0;
                let mut bytes = 0usize;

                for region in process.virtual_memory_regions().only_relevant() {
                    let region = region?;
                    bytes += region.range.size.into_usize()?;

                    total += 1;

                    let mut task = Task {
                        buffers: &buffers,
                        process,
                        region,
                        buffer_size,
                        size,
                        aligned,
                        filter,
                        special: special.as_ref(),
                        ty,
                        cancel: &cancel,
                        tx: tx.clone(),
                    };

                    s.spawn(move |_| {
                        task.guarded_work();
                    });
                }

                let mut reporter = Reporter::new(progress, total, cancel, &mut last_error);

                reporter.report_bytes(bytes);

                let mut results = 0u64;

                while !reporter.is_done() {
                    let m = rx.recv().expect("closed channel");

                    match m {
                        // Scan result from a task.
                        TaskProgress::ScanResult(scan_result) => {
                            let ScanResult { address, value } = scan_result;

                            addresses.push(address);
                            values.push(value);
                        }
                        // Result from one task.
                        TaskProgress::Result(result) => {
                            if let Some(r) = reporter.eval(result) {
                                results += r;
                            }

                            reporter.tick(results);
                        }
                    }
                }

                Ok::<(), failure::Error>(())
            })
        })?;

        if let Some(e) = last_error {
            return Err(e);
        }

        if cancel.test() {
            return Err(failure::format_err!("scan cancelled"));
        }

        return Ok(());

        enum TaskProgress {
            Result(Result<u64, failure::Error>),
            ScanResult(ScanResult),
        }

        struct Task<'a> {
            buffers: &'a thread_buffers::ThreadBuffers,
            process: &'a Process,
            region: MemoryInformation,
            buffer_size: usize,
            size: usize,
            aligned: bool,
            filter: &'a filter::Filter,
            special: Option<&'a Special>,
            ty: Type,
            cancel: &'a Token,
            tx: mpsc::SyncSender<TaskProgress>,
        }

        impl<'a> Task<'a> {
            /// Emit a single result.
            fn emit_result(
                &mut self,
                loc: &Address,
                offset: usize,
                value: Value,
            ) -> Result<(), failure::Error> {
                let address = loc.add(Size::try_from(offset)?)?;
                let scan_result = ScanResult { address, value };
                self.tx.send(TaskProgress::ScanResult(scan_result))?;
                Ok(())
            }

            /// Perform the work in a guarded context.
            fn guarded_work(&mut self) {
                let r = TaskProgress::Result(self.work());
                self.tx.send(r).expect("closed channel");
            }

            /// Internal work implementation.
            fn work(&mut self) -> Result<u64, failure::Error> {
                let region_base = self.region.range.base;
                let region_size = self.region.range.size.into_usize()?;

                let mut start = 0;
                let mut count = 0u64;

                while start < region_size && !self.cancel.test() {
                    let len = usize::min(self.buffer_size, region_size - start);
                    let loc = region_base.add(Size::try_from(start)?)?;

                    // length of the buffer we need to read process memory.
                    let mut buf = self.buffers.get_mut(len)?;
                    let buf = &mut *buf;

                    // TODO: figure out why we are trying to read invalid memory region sometimes.
                    if let Err(_) = self.process.read_process_memory(loc, buf) {
                        continue;
                    }

                    let mut n = 0;
                    let end = buf.len() - self.size;

                    while n < end {
                        let w = &buf[n..(n + self.size)];

                        match self.special.and_then(|s| s.test(w)) {
                            // A special, more efficient kind of matching is available.
                            Some(true) => {
                                let value = self.ty.decode(w);
                                count += 1;
                                self.emit_result(&loc, n, value)?;
                            }
                            // special match is negative.
                            Some(false) => {}
                            None => {
                                let value = self.ty.decode(w);

                                if self.filter.test(None, &value) {
                                    count += 1;
                                    self.emit_result(&loc, n, value)?;
                                }
                            }
                        }

                        if self.aligned {
                            n += self.size;
                        } else {
                            n += 1;
                        }
                    }

                    start += self.buffer_size;
                }

                Ok(count)
            }
        }
    }
}

pub struct Reporter<'token, 'err, P> {
    progress: P,
    /// Current progress.
    current: usize,
    /// how many ticks constitute a percentage.
    percentage: usize,
    /// Total.
    total: usize,
    /// Whether to report progress or not.
    token: &'token Token,
    /// Last error captured from the progress.
    last_err: &'err mut Option<failure::Error>,
}

impl<'token, 'err, P> Reporter<'token, 'err, P> {
    /// Create a new reporter.
    pub fn new(
        progress: P,
        total: usize,
        token: &'token Token,
        last_err: &'err mut Option<failure::Error>,
    ) -> Reporter<'token, 'err, P> {
        let mut percentage = total / 100;

        if percentage <= 0 {
            percentage = 1;
        }

        Reporter {
            progress,
            current: 0,
            percentage,
            total,
            token,
            last_err,
        }
    }

    /// Evaluate the given result.
    pub fn eval<T>(&mut self, result: Result<T, failure::Error>) -> Option<T> {
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
    pub fn report_bytes(&mut self, bytes: usize)
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
        self.current += 1;

        if self.token.test() || self.current % self.percentage != 0 {
            return;
        }

        let p = (self.current * 100) / self.total;

        if let Err(e) = self.progress.report(p, results) {
            *self.last_err = Some(e);
            self.token.set();
        }
    }

    /// Are we done?
    pub fn is_done(&self) -> bool {
        self.current >= self.total
    }
}

/// A single scan result.
#[derive(Debug)]
pub struct ScanResult {
    /// Address where the scanned value lives.
    pub address: Address,
    /// Value from last scan.
    pub value: Value,
}

impl ScanResult {
    /// The size in bytes of the scan result.
    pub fn size(&self) -> usize {
        self.value.ty().size()
    }

    /// Buld a watch out of a scan result.
    pub fn as_watch(&self, handle: Option<&ProcessHandle>) -> Result<Watch, io::Error> {
        let base = match handle {
            Some(handle) => match handle.find_location(self.address)? {
                Location::Module(module) => {
                    let offset = self.address.offset_of(module.range.base)?;
                    pointer::Base::Module(module.name.to_string(), offset)
                }
                _ => pointer::Base::Fixed(self.address),
            },
            None => pointer::Base::Fixed(self.address),
        };

        Ok(Watch {
            pointer: pointer::Pointer::new(base),
            value: self.value.clone(),
            ty: self.value.ty(),
        })
    }
}

pub struct Iter<'a> {
    scan: &'a Scan,
    pos: usize,
}

impl<'a> Iterator for Iter<'a> {
    type Item = ScanResult;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.scan.get(self.pos)?;
        self.pos += 1;
        Some(result)
    }
}
