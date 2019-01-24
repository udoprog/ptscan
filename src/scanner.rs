use crate::{
    address::{Address, Size},
    process::{MemoryInformation, Process},
    scan, thread_buffers,
};
use std::{convert::TryFrom, sync::Arc};

/// A scanner responsible for finding results in memory.
pub struct Scanner {
    /// Thread pool this scanner uses.
    thread_pool: Arc<rayon::ThreadPool>,
    /// Only scan for aligned values.
    aligned: bool,
    /// If this scanner has a set of initial results.
    pub initial: bool,
    /// Current results in scanner.
    pub results: Vec<ScanResult>,
}

impl Scanner {
    /// Construct a new scanner associated with a thread pool.
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
    pub fn refresh(&mut self, process: &Process) -> Result<(), failure::Error> {
        use rayon::prelude::*;

        let buffers = thread_buffers::ThreadBuffers::new();
        let results = &mut self.results;

        self.thread_pool.install(|| {
            results
                .par_iter_mut()
                .filter(|r| !r.killed)
                .map(|result| {
                    let mut work = || {
                        let ty = result.value.ty();

                        let mut buf = buffers.get_mut(ty.size())?;
                        let buf = process.read_process_memory(result.address, &mut *buf)?;

                        if buf.len() != ty.size() {
                            failure::bail!("incomplete read");
                        }

                        result.current = Some(ty.decode(buf));
                        Ok(())
                    };

                    match work() {
                        Ok(()) => 1,
                        Err(_) => 0,
                    }
                })
                .sum::<u64>();

            results.retain(|v| !v.killed);
        });

        Ok(())
    }

    /// rescan a set of results.
    pub fn rescan(
        &mut self,
        process: &Process,
        predicate: &(dyn scan::Predicate),
    ) -> Result<(), failure::Error> {
        use rayon::prelude::*;

        let buffers = thread_buffers::ThreadBuffers::new();
        let results = &mut self.results;

        self.thread_pool.install(|| {
            results
                .par_iter_mut()
                .filter(|r| !r.killed)
                .map(|result| {
                    let mut work = || {
                        let ty = result.value.ty();
                        let mut buf = buffers.get_mut(ty.size())?;
                        let value = process.read_memory_of_type(result.address, ty, &mut *buf)?;

                        result.killed = !predicate.test(Some(result), &value);
                        result.current = None;
                        result.value = value;

                        Ok::<_, failure::Error>(())
                    };

                    match work() {
                        Ok(()) => 1,
                        Err(_) => 0,
                    }
                })
                .sum::<u64>();

            results.retain(|v| !v.killed);
        });

        Ok(())
    }

    /// Scan for the given value in memory.
    ///
    /// TODO: handle unaligned regions.
    ///
    /// Errors raised in worker threads will be collected, and the last error propagated to the user.
    /// Any error causes the processing to bail.
    ///
    /// # Errors raised in Progress
    ///
    /// We can't just ignore errors which are raised by `scanner::Progress`, since these might be important to the
    /// processing at hand.
    ///
    /// Therefore, any error raised by Progress will be treated as any error raised from a worker thread. Only the last
    /// error will be propagated to the user.
    pub fn initial_scan(
        &mut self,
        process: &Process,
        predicate: &(dyn scan::Predicate),
        mut progress: (impl Progress + Send),
    ) -> Result<(), failure::Error> {
        use crate::utils::IteratorExtension;
        use std::sync::{
            atomic::{AtomicBool, Ordering},
            mpsc,
        };

        let scan_type = match predicate.ty() {
            Some(scan_type) => scan_type,
            None => failure::bail!("can't perform initial scan with type-less predicate"),
        };

        let size: usize = scan_type.size();
        let buffer_size = 0x1000;

        let buffers = thread_buffers::ThreadBuffers::new();

        let Scanner {
            ref mut results,
            aligned,
            ..
        } = *self;

        // if true, threads should stop working
        let bail = AtomicBool::new(false);
        let special = predicate.special();
        let mut last_error = None;

        self.thread_pool.install(|| {
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
                        predicate,
                        special: special.as_ref(),
                        scan_type,
                        bail: &bail,
                    };

                    s.spawn(move |_| {
                        let result = task.work(&mut tx);

                        tx.send(TaskProgress::Result(result))
                            .expect("channel send failed");
                    });
                }

                if let Err(e) = progress.report_bytes(bytes) {
                    last_error = Some(e);
                    bail.store(true, Ordering::SeqCst);
                }

                let mut current = 0;
                let mut percentage = total / 100;

                if percentage <= 0 {
                    percentage = 1;
                }

                // collect scan results.
                for m in rx {
                    match m {
                        // Scan result from a task.
                        TaskProgress::ScanResult(scan_result) => {
                            results.push(scan_result);
                        }
                        // Result from one task.
                        TaskProgress::Result(result) => {
                            if let Err(e) = result {
                                last_error = Some(e);
                                bail.store(true, Ordering::SeqCst);
                            }

                            current += 1;

                            if current % percentage == 0 {
                                if let Err(e) = progress.report((current * 100) / total) {
                                    last_error = Some(e);
                                    bail.store(true, Ordering::SeqCst);
                                }
                            }

                            if current >= total {
                                break;
                            }
                        }
                    }
                }
            })
        });

        progress.done(bail.load(Ordering::SeqCst))?;

        if let Some(e) = last_error {
            return Err(e);
        }

        return Ok(());

        enum TaskProgress {
            Result(Result<(), failure::Error>),
            ScanResult(ScanResult),
        }

        struct Task<'a> {
            buffers: &'a thread_buffers::ThreadBuffers,
            process: &'a Process,
            region: Result<MemoryInformation, failure::Error>,
            buffer_size: usize,
            size: usize,
            aligned: bool,
            predicate: &'a (dyn scan::Predicate),
            special: Option<&'a scan::Special>,
            scan_type: scan::Type,
            bail: &'a AtomicBool,
        }

        impl<'a> Task<'a> {
            fn emit(
                tx: &mut mpsc::SyncSender<TaskProgress>,
                loc: &Address,
                offset: usize,
                value: scan::Value,
            ) -> Result<(), failure::Error> {
                let address = loc.add(Size::try_from(offset)?)?;

                let scan_result = ScanResult {
                    address,
                    value,
                    current: None,
                    killed: false,
                };

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
                    predicate,
                    special,
                    scan_type,
                    bail,
                    ..
                } = self;

                let region = region?;
                let range = region.range;
                let len = range.size.into_usize()?;

                let mut start = 0;

                while start < len && !bail.load(Ordering::SeqCst) {
                    let len = usize::min(buffer_size, len - start);
                    let loc = range.base.add(Size::try_from(start)?)?;

                    // length of the buffer we need to read process memory.
                    let mut buf = buffers.get_mut(len)?;

                    // TODO: figure out why we are trying to read invalid memory region sometimes.
                    let buf = match process.read_process_memory(loc, &mut *buf) {
                        Ok(buf) => buf,
                        Err(_) => continue,
                    };

                    let mut n = 0;
                    let end = buf.len() - size;

                    while n < end && !bail.load(Ordering::SeqCst) {
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

                        if predicate.test(None, &value) {
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

/// A single scan result.
#[derive(Debug)]
pub struct ScanResult {
    /// Address where the scanned value lives.
    pub address: Address,
    /// Value from last scan.
    pub value: scan::Value,
    /// Last seen value.
    pub current: Option<scan::Value>,
    /// If the result is valid, or if it should be pruned.
    pub killed: bool,
}

/// A trait to track the progress of processes.
pub trait Progress {
    /// Report the total number of bytes to process.
    fn report_bytes(&mut self, bytes: usize) -> Result<(), failure::Error>;

    /// Report that the process has progresses to the given percentage.
    fn report(&mut self, percentage: usize) -> Result<(), failure::Error>;

    /// Report that the progress is completed, and wheter it was interrupted or not.
    fn done(&mut self, interrupted: bool) -> Result<(), failure::Error>;
}
