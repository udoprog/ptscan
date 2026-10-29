//! Pointer scans for the address of a scratch entry.

use std::sync::atomic::AtomicBool;
use std::sync::{Arc, RwLock};
use std::time::Instant;

use anyhow::{Result, anyhow};
use ptscan::{
    Addresses, FilterExpr, FollowablePointer as _, PointerScan, PortablePointer, ProcessHandle,
    Size, Token, Type, ValueExpr, Values,
};
use tokio::sync::watch;

use crate::{Shared, lock, read};

const MAX_DEPTH: usize = 7;
const MAX_OFFSET: u32 = 0x1000;

#[derive(Default)]
pub(crate) struct PointerSession {
    entry: Option<u64>,
    value_type: Option<Type>,
    results: Arc<Vec<PortablePointer>>,
    running: Option<Arc<AtomicBool>>,
    last: Option<api::PointerScanOutcome>,
    /// Bumped whenever the results are discarded, so that a scan started
    /// before that does not store its result.
    epoch: u64,
}

impl PointerSession {
    fn state(&self) -> api::PointerScanState {
        api::PointerScanState {
            entry: self.entry,
            value_type: self.value_type.map(|t| t.to_string()),
            results: self.results.len() as u64,
            running: self.running.is_some(),
            last: self.last.clone(),
        }
    }

    fn cancel(&self) {
        if let Some(token) = &self.running {
            Token::from_raw(token).set();
        }
    }
}

struct Progress {
    sender: watch::Sender<api::PointerScanProgress>,
    stage: api::PointerScanStage,
}

impl Progress {
    fn send(&self, percentage: u32, remaining: usize, results: u64) {
        self.sender.send_replace(api::PointerScanProgress {
            stage: self.stage,
            percentage,
            remaining: remaining as u64,
            results,
        });
    }
}

impl ptscan::ScanProgress for Progress {
    fn report_bytes(&mut self, _: Size) -> Result<()> {
        Ok(())
    }

    fn report(&mut self, percentage: usize, results: u64) -> Result<()> {
        self.send(percentage as u32, 0, results);
        Ok(())
    }
}

impl ptscan::PointerScanInitialProgress for Progress {
    fn report(&mut self, queue: usize, results: usize) -> Result<()> {
        self.send(0, queue, results as u64);
        Ok(())
    }
}

impl ptscan::PointerScanBackreferenceProgress for Progress {
    fn report(&mut self, remaining: usize, results: usize) -> Result<()> {
        self.send(0, remaining, results as u64);
        Ok(())
    }
}

impl Shared {
    pub fn pointer_scan_state(&self) -> api::PointerScanState {
        lock(&self.pointer_session).state()
    }

    fn publish_pointer_scan_state(&self, session: &PointerSession) -> api::PointerScanState {
        let state = session.state();
        self.pointer_scan_state.send_replace(state.clone());
        state
    }

    /// Start a pointer scan in the background for the address the scratch
    /// entry `id` currently follows to, replacing the paths of the last one.
    pub fn start_pointer_scan(self: &Arc<Self>, id: u64) -> api::PointerScanResponse {
        let error = |e: &str| api::PointerScanResponse {
            error: Some(e.into()),
        };

        let Some(handle) = lock(&self.handle).clone() else {
            return error("not attached to a process");
        };

        let Some((pointer, value_type)) = lock(&self.scratch).get(id) else {
            return error(&format!("no scratch entry {id}"));
        };

        let mut session = lock(&self.pointer_session);

        if session.running.is_some() {
            return error("a pointer scan is already running");
        }

        let token = Arc::new(AtomicBool::new(false));
        session.running = Some(token.clone());
        session.entry = Some(id);
        session.value_type = Some(value_type);
        session.results = Arc::default();
        session.last = None;
        let epoch = session.epoch;
        self.pointer_progress
            .send_replace(api::PointerScanProgress::default());
        self.publish_pointer_scan_state(&session);
        drop(session);

        let shared = self.clone();

        std::thread::spawn(move || {
            let started = Instant::now();
            let token_ref = Token::from_raw(&token);
            let mut results = Vec::new();
            let result = shared.run_pointer_scan(&handle, &pointer, &mut results, token_ref);

            let mut session = lock(&shared.pointer_session);

            if session.epoch != epoch {
                return;
            }

            session.running = None;
            session.results = Arc::new(results);
            session.last = Some(api::PointerScanOutcome {
                results: session.results.len() as u64,
                cancelled: token_ref.test(),
                error: result.err().map(|e| format!("{e:#}")),
                duration_ms: started.elapsed().as_millis() as u64,
            });
            shared.publish_pointer_scan_state(&session);
        });

        api::PointerScanResponse::default()
    }

    /// The pointer scan of the GTK GUI: every pointer in memory is collected,
    /// then followed back from the address to modules.
    fn run_pointer_scan(
        &self,
        handle: &RwLock<ProcessHandle>,
        pointer: &PortablePointer,
        results: &mut Vec<PortablePointer>,
        token: &Token,
    ) -> Result<()> {
        crate::scan::refresh_modules(handle)?;
        let handle = read(handle);

        let needle = pointer
            .follow_default(&handle)?
            .ok_or_else(|| anyhow!("failed to follow pointer `{pointer}` to an address"))?;

        let filter = FilterExpr::pointer(ValueExpr::Value, &handle.process)?;
        let width = handle.process.pointer_width;
        let mut addresses = Addresses::new(width);
        let mut values = Values::new(width.into_type());

        let progress = |stage| Progress {
            sender: self.pointer_progress.clone(),
            stage,
        };

        handle.initial_scan(
            &self.pool,
            &filter,
            &mut addresses,
            &mut values,
            Some(token),
            progress(api::PointerScanStage::Initial),
            Default::default(),
        )?;

        let mut scan = PointerScan::new(&self.pool, &handle, token);
        scan.max_depth = MAX_DEPTH;
        scan.max_offset = Size::new(MAX_OFFSET.into());
        scan.build_references(addresses.iter_copied(), values.iter())?;
        drop((addresses, values));

        let mut pointers = Vec::new();
        let result = scan.scan(
            needle,
            &mut pointers,
            &mut progress(api::PointerScanStage::Paths),
        );

        if result.is_ok() {
            scan.backreference_scan(
                &mut pointers,
                &mut progress(api::PointerScanStage::Backreferences),
            )?;
        }

        results.extend(pointers.into_iter().filter_map(|p| {
            Some(PortablePointer::new(
                p.base.as_portable(&handle)?,
                p.offsets,
            ))
        }));

        result
    }

    pub fn cancel_pointer_scan(&self) -> api::PointerScanState {
        let session = lock(&self.pointer_session);
        session.cancel();
        session.state()
    }

    pub(crate) fn reset_pointer_scan(&self) {
        let mut session = lock(&self.pointer_session);
        session.cancel();
        let epoch = session.epoch + 1;
        *session = PointerSession {
            epoch,
            ..PointerSession::default()
        };
        self.publish_pointer_scan_state(&session);
    }

    pub fn pointer_scan_results(&self, offset: u64, limit: u32) -> api::PointerScanResultsResponse {
        let (results, value_type) = {
            let session = lock(&self.pointer_session);
            (session.results.clone(), session.value_type)
        };

        let total = results.len();
        let start = usize::try_from(offset).unwrap_or(usize::MAX).min(total);
        let end = start
            .saturating_add(limit.min(api::MAX_SCAN_PAGE) as usize)
            .min(total);

        api::PointerScanResultsResponse {
            total: total as u64,
            offset: start as u64,
            results: results[start..end].iter().map(|p| p.to_string()).collect(),
            value_type: value_type.map(|t| t.to_string()),
        }
    }
}
