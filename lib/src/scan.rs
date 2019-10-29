//! Predicates used for matching against memory.

use crate::{
    address::{Address, Size},
    error::Error,
    filter, pointer,
    process::Process,
    watch::Watch,
    Location, ProcessHandle, Token, Value,
};
use std::{
    convert::TryInto,
    sync::{mpsc, Arc},
};

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
    /// Addresses in the scan.
    pub addresses: Vec<Address>,
    /// Values in the scan.
    pub values: Vec<Value>,
}

impl Scan {
    /// Construct a new scan associated with a thread pool.
    pub fn new(thread_pool: &Arc<rayon::ThreadPool>) -> Self {
        Self {
            aligned: None,
            initial: false,
            thread_pool: Arc::clone(thread_pool),
            addresses: Vec::new(),
            values: Vec::new(),
        }
    }

    /// Remove the given entry by address.
    ///
    /// Returns `true` if the address was removed, `false` otherwise.
    pub fn remove_by_address(&mut self, address: Address) -> bool {
        if let Some(index) = self.addresses.iter().position(|a| *a == address) {
            self.addresses.swap_remove(index);
            self.values.swap_remove(index);
            true
        } else {
            false
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
    pub fn get<'a>(&'a self, index: usize) -> Option<ScanResult> {
        let address = self.addresses.get(index).cloned()?;
        let value = self.values.get(index).cloned()?;
        Some(ScanResult { address, value })
    }

    /// Push the given scan result.
    pub fn push(&mut self, result: ScanResult) {
        self.addresses.push(result.address);
        self.values.push(result.value.clone());
    }

    /// Create an iterator over the scan.
    pub fn iter<'a>(&'a self) -> Iter<'a> {
        Iter { scan: self, pos: 0 }
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
        &self,
        process: &Process,
        filter: &filter::Filter,
        cancel: Option<&Token>,
        progress: (impl Progress + Send),
    ) -> Result<Scan, anyhow::Error> {
        let mut addresses = Vec::new();
        let mut values = Vec::new();

        process.rescan_values(
            &*self.thread_pool,
            &self.values,
            &self.addresses,
            &mut values,
            &mut addresses,
            filter,
            cancel,
            progress,
        )?;

        Ok(Scan {
            thread_pool: Arc::clone(&self.thread_pool),
            aligned: self.aligned,
            initial: false,
            addresses,
            values,
        })
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
    ) -> anyhow::Result<()> {
        use crate::utils::IteratorExtension;

        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        let Scan {
            ref mut addresses,
            ref mut values,
            aligned,
            ref thread_pool,
            ..
        } = *self;

        addresses.clear();
        values.clear();

        let mut last_error = None;

        let size: Size = filter.ty.size().try_into()?;

        let aligned = aligned.unwrap_or(filter.ty.is_default_aligned());

        let session = process.session()?;
        let session = &session;

        let special = filter.special()?;
        let special = special.as_ref();

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::sync_channel(1024);
                let mut total = 0;
                let mut bytes = Size::zero();

                for region in process.virtual_memory_regions().only_relevant() {
                    let region = region?;
                    bytes.add_assign(region.range.size)?;
                    total += 1;

                    let tx = tx.clone();

                    s.spawn(move |_| {
                        let work = move || {
                            let mut addresses = Vec::new();
                            let mut values = Vec::new();

                            if cancel.test() {
                                return Ok((addresses, values));
                            }

                            let mut current = match special {
                                Some(special) => {
                                    match session.next_address(
                                        &region.range,
                                        region.range.base,
                                        special,
                                    )? {
                                        Some(address) => address,
                                        None => return Ok((addresses, values)),
                                    }
                                }
                                None => region.range.base,
                            };

                            let end = region.range.base.add(region.range.size)?;

                            while current < end {
                                let proxy = session.address_proxy(current);

                                if filter.test(&Value::None, proxy)? {
                                    addresses.push(current);
                                    values.push(proxy.eval(filter.ty)?);
                                }

                                if let Some(special) = special {
                                    current = match session.next_address(
                                        &region.range,
                                        current.saturating_add(Size::new(1)),
                                        special,
                                    )? {
                                        Some(address) => address,
                                        None => break,
                                    }
                                } else {
                                    if aligned {
                                        current.add_assign(size)?;
                                    } else {
                                        current.add_assign(1u32.into())?;
                                    }
                                }
                            }

                            Ok::<_, anyhow::Error>((addresses, values))
                        };

                        tx.send(work()).expect("channel closed");
                    });
                }

                let mut reporter = Reporter::new(progress, total, cancel, &mut last_error);

                reporter.report_bytes(bytes);

                let mut results = 0u64;

                while !reporter.is_done() {
                    let result = rx.recv().expect("channel closed");

                    if let Some((mut a, mut v)) = reporter.eval(result) {
                        results += v.len() as u64;
                        addresses.append(&mut a);
                        values.append(&mut v);
                    }

                    reporter.tick(results);
                }

                Ok::<(), anyhow::Error>(())
            })
        })?;

        if let Some(e) = last_error {
            return Err(e);
        }

        return Ok(());
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
        self.current += 1;

        if self.current % self.percentage == 0 {
            let p = (self.current * 100) / self.total;

            if let Err(e) = self.progress.report(p, results) {
                *self.last_err = Some(e);
                self.token.set();
            }
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
    /// Buld a watch out of a scan result.
    pub fn as_watch(&self, handle: Option<&ProcessHandle>) -> Result<Watch, Error> {
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
