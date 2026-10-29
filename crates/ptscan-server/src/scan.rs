//! Scan generations of the attached process, with undo and redo.

use std::sync::atomic::AtomicBool;
use std::sync::{Arc, RwLock};
use std::time::Instant;

use anyhow::{Context as _, Result, anyhow};
use ptscan::{
    Addresses, FilterExpr, InitialScanConfig, PortablePointer, Process, ProcessHandle, Size, Token,
    TypeHint, ValueExpr, Values,
};
use tokio::sync::watch;

use crate::{Shared, lock, read, write};

#[derive(Clone)]
struct Generation {
    addresses: Addresses,
    initial: Values,
    last: Values,
    value: ValueExpr,
}

impl Generation {
    fn bytes(&self) -> usize {
        self.addresses.bytes() + self.initial.bytes() + self.last.bytes()
    }
}

struct Plan {
    filter: FilterExpr,
    value: ValueExpr,
    config: InitialScanConfig,
    suspend: bool,
}

#[derive(Default)]
pub(crate) struct Session {
    scans: Vec<Arc<Generation>>,
    undone: Vec<Arc<Generation>>,
    running: Option<Arc<AtomicBool>>,
    last: Option<api::ScanOutcome>,
    /// Results of the current generation while it is taken out to be rescanned
    /// in place.
    in_place: Option<u64>,
    /// Bumped whenever the generations are discarded, so that a scan started
    /// before that does not add its result.
    epoch: u64,
}

impl Session {
    fn state(&self) -> api::ScanState {
        api::ScanState {
            generations: (self.scans.len() + usize::from(self.in_place.is_some())) as u32,
            undone: self.undone.len() as u32,
            results: self
                .in_place
                .or_else(|| self.scans.last().map(|g| g.addresses.len() as u64))
                .unwrap_or(0),
            running: self.running.is_some(),
            last: self.last.clone(),
        }
    }

    fn cancel(&self) {
        if let Some(token) = &self.running {
            Token::from_raw(token).set();
        }
    }

    pub(crate) fn reset(&mut self) {
        self.cancel();
        self.running = None;
        self.in_place = None;
        self.scans.clear();
        self.undone.clear();
        self.last = None;
        self.epoch += 1;
    }
}

struct Progress(watch::Sender<api::ScanProgress>);

impl ptscan::ScanProgress for Progress {
    fn report_bytes(&mut self, _: Size) -> Result<()> {
        Ok(())
    }

    fn report(&mut self, percentage: usize, results: u64) -> Result<()> {
        self.0.send_replace(api::ScanProgress {
            percentage: percentage as u32,
            results,
        });
        Ok(())
    }
}

struct NoProgress;

impl ptscan::ScanProgress for NoProgress {
    fn report_bytes(&mut self, _: Size) -> Result<()> {
        Ok(())
    }

    fn report(&mut self, _: usize, _: u64) -> Result<()> {
        Ok(())
    }
}

impl Shared {
    pub fn scan_state(&self) -> api::ScanState {
        lock(&self.session).state()
    }

    pub(crate) fn publish_scan_state(&self, session: &Session) -> api::ScanState {
        let state = session.state();
        self.scan_state.send_replace(state.clone());
        state
    }

    /// Parse the request and start a scan in the background: an initial scan
    /// without generations, otherwise a rescan of the current one. A current
    /// generation larger than the copy limit is rescanned in place.
    pub fn start_scan(self: &Arc<Self>, request: api::ScanRequest) -> api::ScanResponse {
        let mut response = api::ScanResponse::default();

        let Some(handle) = lock(&self.handle).clone() else {
            response.error = Some("not attached to a process".into());
            return response;
        };

        let filter = if request.filter.trim().is_empty() {
            response.filter_error = Some("filter expression is empty".into());
            None
        } else {
            match FilterExpr::parse(&request.filter, &*read(&handle)) {
                Ok(filter) => Some(filter),
                Err(e) => {
                    response.filter_error = Some(format!("{e:#}"));
                    None
                }
            }
        };

        let value = if request.value.trim().is_empty() {
            Some(ValueExpr::Value)
        } else {
            match ValueExpr::parse(&request.value) {
                Ok(value) => Some(value),
                Err(e) => {
                    response.value_error = Some(format!("{e:#}"));
                    None
                }
            }
        };

        let (Some(filter), Some(value)) = (filter, value) else {
            return response;
        };

        let plan = Plan {
            filter,
            value,
            config: InitialScanConfig {
                alignment: request.alignment.map(|a| a as usize),
                modules_only: request.modules_only,
                ..Default::default()
            },
            suspend: request.suspend,
        };

        let mut session = lock(&self.session);

        if session.running.is_some() {
            response.error = Some("a scan is already running".into());
            return response;
        }

        let token = Arc::new(AtomicBool::new(false));
        session.running = Some(token.clone());
        let in_place = session
            .scans
            .last()
            .is_some_and(|g| g.bytes() > self.rescan_copy_limit);

        let previous = if in_place {
            let previous = session.scans.pop();
            session.in_place = previous.as_ref().map(|g| g.addresses.len() as u64);
            previous
        } else {
            session.scans.last().cloned()
        };

        let epoch = session.epoch;
        self.progress.send_replace(api::ScanProgress::default());
        self.publish_scan_state(&session);
        drop(session);

        let shared = self.clone();

        std::thread::spawn(move || {
            let started = Instant::now();
            let token_ref = Token::from_raw(&token);

            // Without other references this takes the generation, which is
            // the case when it is rescanned in place.
            let mut generation = previous.map(Arc::unwrap_or_clone);
            let result = shared.run_scan(&handle, plan, &mut generation, token_ref);

            let cancelled = token_ref.test();
            let mut session = lock(&shared.session);

            if session.epoch != epoch {
                return;
            }

            session.running = None;
            session.in_place = None;

            let mut outcome = api::ScanOutcome {
                cancelled,
                in_place,
                duration_ms: started.elapsed().as_millis() as u64,
                ..Default::default()
            };

            if let Err(e) = result {
                outcome.error = Some(format!("{e:#}"));

                // A generation rescanned in place is the only copy left.
                if !in_place {
                    generation = None;
                }
            }

            if let Some(generation) = generation {
                outcome.results = generation.addresses.len() as u64;
                session.scans.push(Arc::new(generation));
                session.undone.clear();
            }

            session.last = Some(outcome);
            shared.publish_scan_state(&session);
        });

        response
    }

    /// Rescan `generation`, or store a new one from an initial scan in it. A
    /// cancelled scan leaves its partial results.
    fn run_scan(
        &self,
        handle: &RwLock<ProcessHandle>,
        plan: Plan,
        generation: &mut Option<Generation>,
        token: &Token,
    ) -> Result<()> {
        let Plan {
            filter,
            value,
            config,
            suspend,
        } = plan;

        let progress = Progress(self.progress.clone());

        if let Some(generation) = generation {
            generation.value = value;
            let handle = read(handle);

            return suspended(&handle.process, suspend, || {
                handle.rescan_values(
                    &self.pool,
                    &mut generation.addresses,
                    &mut generation.initial,
                    &mut generation.last,
                    Some(token),
                    progress,
                    &filter,
                )
            });
        }

        let value_type = {
            let handle = read(handle);

            value
                .value_type_of(&*handle, TypeHint::NoHint)?
                .unwrap_or(filter.value_type_of(&*handle, TypeHint::NoHint)?)
                .ok_or_else(|| anyhow!("cannot determine type of value"))?
        };

        refresh_modules(handle)?;
        let handle = read(handle);
        let mut addresses = Addresses::new(handle.process.pointer_width);
        let mut values = Values::new(value_type);

        let result = suspended(&handle.process, suspend, || {
            handle.initial_scan(
                &self.pool,
                &filter,
                &mut addresses,
                &mut values,
                Some(token),
                progress,
                config,
            )
        });

        *generation = Some(Generation {
            addresses,
            initial: values.clone(),
            last: values,
            value,
        });

        result
    }

    pub fn cancel_scan(&self) -> api::ScanState {
        let session = lock(&self.session);
        session.cancel();
        session.state()
    }

    pub fn reset_scan(&self) -> api::ScanState {
        let mut session = lock(&self.session);
        session.reset();
        self.publish_scan_state(&session)
    }

    pub fn undo_scan(&self) -> api::ScanState {
        let mut session = lock(&self.session);

        if session.running.is_none() && session.scans.len() > 1 {
            let generation = session.scans.pop().unwrap();
            session.undone.push(generation);
            return self.publish_scan_state(&session);
        }

        session.state()
    }

    pub fn redo_scan(&self) -> api::ScanState {
        let mut session = lock(&self.session);

        if session.running.is_none()
            && let Some(generation) = session.undone.pop()
        {
            session.scans.push(generation);
            return self.publish_scan_state(&session);
        }

        session.state()
    }

    /// Read a page of the current generation along with the values in memory
    /// now.
    pub fn scan_results(&self, offset: u64, limit: u32) -> Result<api::ScanResultsResponse> {
        let generation = {
            let session = lock(&self.session);

            if let Some(total) = session.in_place {
                return Ok(api::ScanResultsResponse {
                    total,
                    offset,
                    error: Some("the current generation is being rescanned".into()),
                    ..Default::default()
                });
            }

            session.scans.last().cloned()
        };
        let handle = lock(&self.handle).clone();

        let (Some(generation), Some(handle)) = (generation, handle) else {
            return Ok(api::ScanResultsResponse {
                offset,
                ..Default::default()
            });
        };

        let total = generation.addresses.len();
        let start = usize::try_from(offset).unwrap_or(usize::MAX).min(total);
        let end = start
            .saturating_add(limit.min(api::MAX_SCAN_PAGE) as usize)
            .min(total);

        let handle = read(&handle);
        let last_type = generation.last.ty();

        let mut addresses = Addresses::new(handle.process.pointer_width);
        let mut current = Values::new(last_type);

        for index in start..end {
            addresses.push(generation.addresses.get(index).unwrap());
            current.push(generation.last.get(index).unwrap());
        }

        let value_type = generation
            .value
            .value_type_of(&*handle, TypeHint::NoHint)?
            .option()
            .unwrap_or(last_type)
            .unsize(last_type);

        if value_type != current.ty() {
            current.convert_in_place(&*handle, value_type);
        }

        let error = handle
            .refresh_values(
                &self.pool,
                &addresses,
                &mut current,
                None,
                NoProgress,
                &generation.value,
            )
            .err()
            .map(|e| format!("{e:#}"));

        let results = (start..end)
            .zip(addresses.iter_copied())
            .map(|(index, address)| {
                let pointer = PortablePointer::from(handle.address_to_portable_base(address));

                api::ScanResult {
                    address: address.into(),
                    pointer: pointer.to_string(),
                    value_type: last_type.to_string(),
                    last: generation.last.get(index).unwrap().to_string(),
                    current: current.get(index - start).unwrap().to_string(),
                }
            })
            .collect();

        Ok(api::ScanResultsResponse {
            total: total as u64,
            offset: start as u64,
            results,
            error,
        })
    }
}

pub(crate) fn refresh_modules(handle: &RwLock<ProcessHandle>) -> Result<()> {
    let (modules, threads) = {
        let handle = read(handle);
        let modules = handle.get_modules()?;
        // Threads only name stack-relative results, so a platform without
        // thread listing scans without them.
        let threads = match handle.get_threads_with_modules(&modules) {
            Err(e) if matches!(e.downcast_ref(), Some(ptscan::Error::Unsupported)) => Vec::new(),
            result => result?,
        };
        (modules, threads)
    };

    let mut handle = write(handle);
    handle.update_modules(modules);
    handle.update_threads(threads);
    Ok(())
}

/// Run `f` with the process suspended if `suspend` is set.
fn suspended<T>(process: &Process, suspend: bool, f: impl FnOnce() -> Result<T>) -> Result<T> {
    struct Resume<'a>(&'a Process);

    impl Drop for Resume<'_> {
        fn drop(&mut self) {
            _ = self.0.resume();
        }
    }

    if !suspend {
        return f();
    }

    process.suspend().context("failed to suspend process")?;
    let guard = Resume(process);
    let result = f();
    std::mem::forget(guard);
    let resumed = process.resume().context("failed to resume process");
    let value = result?;
    resumed?;
    Ok(value)
}
