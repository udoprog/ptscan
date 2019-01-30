//! Predicates used for matching against memory.

use crate::{
    address::{Address, Sign, Size},
    filter, pointer,
    process::{MemoryInformation, Process},
    special::Special,
    thread_buffers,
    watch::Watch,
    Location, ProcessHandle, Token, Type, Value,
};
use std::{
    convert::TryFrom,
    fmt, io,
    sync::{mpsc, Arc},
};

/// A trait to track the progress of processes.
pub trait Progress {
    /// Report the total number of bytes to process.
    fn report_bytes(&mut self, bytes: usize) -> Result<(), failure::Error>;

    /// Report that the process has progresses to the given percentage.
    fn report(&mut self, percentage: usize) -> Result<(), failure::Error>;
}

/// A scan responsible for finding results in memory.
pub struct Scan {
    /// Thread pool this scan uses.
    thread_pool: Arc<rayon::ThreadPool>,
    /// Only scan for aligned values.
    aligned: bool,
    /// If this scan has a set of initial results.
    pub initial: bool,
    /// Current results in scan.
    pub results: Vec<ScanResult>,
}

impl Scan {
    /// Construct a new scan associated with a thread pool.
    pub fn new(thread_pool: &Arc<rayon::ThreadPool>) -> Self {
        Self {
            results: Vec::new(),
            aligned: false,
            initial: false,
            thread_pool: Arc::clone(thread_pool),
        }
    }

    /// Only scan for values which are aligned.
    pub fn aligned(self) -> Self {
        Self {
            aligned: true,
            ..self
        }
    }

    /// Refresh current value for scan results.
    pub fn refresh(
        &self,
        process: &Process,
        values: &mut Vec<Value>,
        cancel: Option<&Token>,
        progress: (impl Progress + Send),
    ) -> Result<(), failure::Error> {
        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        let len = usize::min(self.results.len(), values.len());

        if len == 0 {
            return Ok(());
        }

        let buffers = thread_buffers::ThreadBuffers::new();

        let mut last_error = None;

        self.thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::sync_channel(1024);

                let mut reporter = Reporter::new(progress, len, cancel);

                for (result, v) in self.results.iter().zip(values) {
                    let tx = tx.clone();
                    let buffers = &buffers;

                    s.spawn(move |_| {
                        if cancel.test() {
                            tx.send(Ok(())).expect("closed channel");
                            return;
                        }

                        let mut work = || {
                            let ty = result.value.ty();
                            let mut buf = buffers.get_mut(ty.size())?;
                            let value =
                                process.read_memory_of_type(result.address, ty, &mut *buf)?;
                            *v = value;
                            Ok::<_, failure::Error>(())
                        };

                        tx.send(work()).expect("closed channel");
                    });
                }

                while !reporter.is_done() {
                    let m = rx.recv().expect("closed channel");

                    if let Err(e) = reporter.tick() {
                        last_error = Some(e);
                        cancel.set();
                    }

                    if let Err(e) = m {
                        last_error = Some(e);
                        cancel.set();
                    }
                }
            });
        });

        if let Some(e) = last_error {
            return Err(e);
        }

        if cancel.test() {
            return Err(failure::format_err!("scan cancelled"));
        }

        Ok(())
    }

    /// rescan a set of results.
    pub fn rescan(
        &mut self,
        process: &Process,
        filter: &(dyn filter::Filter),
        cancel: Option<&Token>,
        progress: (impl Progress + Send),
    ) -> Result<(), failure::Error> {
        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        if self.results.is_empty() {
            return Ok(());
        }

        let buffers = thread_buffers::ThreadBuffers::new();
        let results = &mut self.results;

        let mut last_error = None;

        self.thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::sync_channel(1024);

                let mut reporter = Reporter::new(progress, results.len(), cancel);

                for result in results {
                    let tx = tx.clone();
                    let buffers = &buffers;

                    s.spawn(move |_| {
                        if cancel.test() {
                            tx.send(Ok(())).expect("closed channel");
                            return;
                        }

                        let mut work = || {
                            let ty = result.value.ty();
                            let mut buf = buffers.get_mut(ty.size())?;
                            let value =
                                process.read_memory_of_type(result.address, ty, &mut *buf)?;

                            if filter.test(Some(result), &value) {
                                result.value = value;
                            } else {
                                result.value = Value::None;
                            }

                            Ok::<_, failure::Error>(())
                        };

                        tx.send(work()).expect("closed channel");
                    });
                }

                while !reporter.is_done() {
                    let m = rx.recv().expect("closed channel");

                    if let Err(e) = reporter.tick() {
                        last_error = Some(e);
                        cancel.set();
                    }

                    if let Err(e) = m {
                        last_error = Some(e);
                        cancel.set();
                    }
                }
            });
        });

        self.results.retain(|v| v.value.is_some());

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
        filter: &(dyn filter::Filter),
        cancel: Option<&Token>,
        progress: (impl Progress + Send),
    ) -> Result<(), failure::Error> {
        use crate::utils::IteratorExtension;

        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        let size = match filter.size() {
            Some(size) => size,
            None => {
                return Err(failure::format_err!(
                    "can't perform initial scan with type-less filter"
                ));
            }
        };

        let scan_type = filter.ty()?;

        let buffer_size = 0x1000;

        let buffers = thread_buffers::ThreadBuffers::new();

        let Scan {
            ref mut results,
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
                    bytes += region
                        .as_ref()
                        .map_err(|_| ())
                        .and_then(|r| r.range.size.into_usize().map_err(|_| ()))
                        .unwrap_or(0);

                    total += 1;
                    let mut tx = tx.clone();

                    let task = Task {
                        buffers: &buffers,
                        process,
                        region,
                        buffer_size,
                        size,
                        aligned,
                        filter,
                        special: special.as_ref(),
                        scan_type,
                        cancel: &cancel,
                    };

                    s.spawn(move |_| {
                        let result = task.work(&mut tx);

                        tx.send(TaskProgress::Result(result))
                            .expect("channel send failed");
                    });
                }

                let mut reporter = Reporter::new(progress, total, cancel);

                if let Err(e) = reporter.report_bytes(bytes) {
                    last_error = Some(e);
                    cancel.set();
                }

                while !reporter.is_done() {
                    let m = rx.recv().expect("closed channel");

                    match m {
                        // Scan result from a task.
                        TaskProgress::ScanResult(scan_result) => {
                            results.push(scan_result);
                        }
                        // Result from one task.
                        TaskProgress::Result(result) => {
                            if let Err(e) = result {
                                last_error = Some(e);
                                cancel.set();
                            }

                            if let Err(e) = reporter.tick() {
                                last_error = Some(e);
                                cancel.set();
                            }
                        }
                    }
                }
            })
        });

        if let Some(e) = last_error {
            return Err(e);
        }

        if cancel.test() {
            return Err(failure::format_err!("scan cancelled"));
        }

        return Ok(());

        enum TaskProgress {
            Result(Result<(), failure::Error>),
            ScanResult(ScanResult),
        }

        struct Task<'a> {
            buffers: &'a thread_buffers::ThreadBuffers,
            process: &'a Process,
            region: Result<MemoryInformation, io::Error>,
            buffer_size: usize,
            size: usize,
            aligned: bool,
            filter: &'a (dyn filter::Filter),
            special: Option<&'a Special>,
            scan_type: Type,
            cancel: &'a Token,
        }

        impl<'a> Task<'a> {
            fn emit(
                tx: &mut mpsc::SyncSender<TaskProgress>,
                loc: &Address,
                offset: usize,
                value: Value,
            ) -> Result<(), failure::Error> {
                let address = loc.add(Size::try_from(offset)?)?;

                let scan_result = ScanResult { address, value };

                tx.send(TaskProgress::ScanResult(scan_result))?;
                Ok(())
            }

            fn work(self, tx: &mut mpsc::SyncSender<TaskProgress>) -> Result<(), failure::Error> {
                let Task {
                    buffers,
                    process,
                    region,
                    buffer_size,
                    size,
                    aligned,
                    filter,
                    special,
                    scan_type,
                    cancel,
                    ..
                } = self;

                let region = region?;
                let range = region.range;
                let len = range.size.into_usize()?;

                let mut start = 0;

                while start < len && !cancel.test() {
                    let len = usize::min(buffer_size, len - start);
                    let loc = range.base.add(Size::try_from(start)?)?;

                    // length of the buffer we need to read process memory.
                    let mut buf = buffers.get_mut(len)?;
                    let buf = &mut *buf;

                    // TODO: figure out why we are trying to read invalid memory region sometimes.
                    if let Err(_) = process.read_process_memory(loc, buf) {
                        continue;
                    }

                    let mut n = 0;
                    let end = buf.len() - size;

                    while n < end && !cancel.test() {
                        let w = &buf[n..(n + size)];

                        // A special, more efficient kind of matching is available.
                        if let Some(special) = special {
                            if let Some(result) = special.test(w) {
                                if result {
                                    let value = scan_type.decode(w);
                                    Self::emit(tx, &loc, n, value)?;
                                }

                                if aligned {
                                    n += size;
                                } else {
                                    n += 1;
                                }

                                continue;
                            }
                        }

                        let value = scan_type.decode(w);

                        if filter.test(None, &value) {
                            Self::emit(tx, &loc, n, value)?;
                        }

                        if aligned {
                            n += size;
                        } else {
                            n += 1;
                        }
                    }

                    start += buffer_size;
                }

                Ok(())
            }
        }
    }
}

pub struct Reporter<'token, P> {
    progress: P,
    /// Current progress.
    current: usize,
    /// how many ticks constitute a percentage.
    percentage: usize,
    /// Total.
    total: usize,
    /// Whether to report progress or not.
    token: &'token Token,
}

impl<'token, P> Reporter<'token, P> {
    pub fn new<'a>(progress: P, total: usize, token: &'a Token) -> Reporter<P> {
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
        }
    }

    // Report a number of bytes.
    pub fn report_bytes(&mut self, bytes: usize) -> Result<(), failure::Error>
    where
        P: Progress,
    {
        self.progress.report_bytes(bytes)
    }

    // Tick a single task.
    pub fn tick(&mut self) -> Result<(), failure::Error>
    where
        P: Progress,
    {
        self.current += 1;

        if self.token.test() {
            return Ok(());
        }

        if self.current % self.percentage == 0 {
            return self.progress.report((self.current * 100) / self.total);
        }

        Ok(())
    }

    // Are we done?
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

    /// An improved display implemented with a process handle.
    pub fn address_display<'a>(&'a self, handle: &'a ProcessHandle) -> AddressDisplay<'a> {
        AddressDisplay {
            result: self,
            handle,
        }
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

// A displayed scan result.
pub struct AddressDisplay<'a> {
    result: &'a ScanResult,
    handle: &'a ProcessHandle,
}

impl<'a> fmt::Display for AddressDisplay<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let AddressDisplay {
            ref handle,
            ref result,
        } = *self;

        let ScanResult { address, .. } = *result;

        let location = handle
            .find_location(*address)
            .ok()
            .unwrap_or(Location::None);

        let offset = match location {
            Location::Module(module) => match address.offset_of(module.range.base).ok() {
                Some(offset) => {
                    write!(fmt, "{}", module.name)?;
                    offset
                }
                // TODO: handle error differently?
                None => {
                    write!(fmt, "{}", address)?;
                    return Ok(());
                }
            },
            Location::Thread(thread) => {
                let base = thread
                    .stack_exit
                    .as_ref()
                    .cloned()
                    .unwrap_or(thread.stack.base);

                match address.offset_of(base).ok() {
                    Some(offset) => {
                        write!(fmt, "THREADSTACK{}", thread.id)?;
                        offset
                    }
                    // TODO: handle error differently?
                    None => {
                        write!(fmt, "{}", address)?;
                        return Ok(());
                    }
                }
            }
            Location::None => {
                write!(fmt, "{}", address)?;
                return Ok(());
            }
        };

        match offset.sign() {
            Sign::Pos => write!(fmt, " + {}", offset)?,
            Sign::Neg => write!(fmt, " - {}", offset.abs())?,
        }

        Ok(())
    }
}
