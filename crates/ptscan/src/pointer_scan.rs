//! Pointer scans for the address of a scratch entry.

mod file;

use std::sync::atomic::AtomicBool;
use std::sync::{Arc, RwLock};
use std::time::Instant;

use anyhow::{Result, anyhow};
use ptscan_core::{
    Addresses, Base, FilterExpr, FollowablePointer as _, Pointer, PointerScan, PortableBase,
    PortablePointer, ProcessHandle, ScanContext, Size, Token, Type, ValueExpr, Values,
};
use tokio::sync::watch;

use crate::export::{self, Refused};
use crate::{Shared, lock, read};

/// The paths of a pointer scan, as the scan produced them.
///
/// Modules are referred to by index and only named when a page is formatted,
/// so that no path holds a copy of a module name.
#[derive(Default)]
struct Paths {
    pointers: Vec<Pointer>,
    /// The names of the modules at the end of the scan, by module id.
    modules: Vec<String>,
    /// The scan stopped at the most paths it may store.
    truncated: bool,
}

impl Paths {
    /// Format the path as a portable pointer.
    fn display(&self, pointer: &Pointer) -> String {
        let base = match pointer.base {
            Base::Null => PortableBase::Null,
            Base::Module { id, offset } => match self.modules.get(usize::from(id)) {
                Some(name) => PortableBase::Module {
                    name: name.clone(),
                    offset,
                },
                None => PortableBase::Null,
            },
            Base::Address { address } => PortableBase::Address { address },
        };

        PortablePointer::new(base, pointer.offsets.iter().copied()).to_string()
    }
}

#[derive(Default)]
pub(crate) struct PointerResults {
    entry: Option<u64>,
    value_type: Option<Type>,
    results: Arc<Paths>,
    /// The scan, save or load running; one at a time, so that each cancels
    /// the same way.
    running: Option<Arc<AtomicBool>>,
    /// What runs, when it is a save or load rather than a scan.
    file: Option<api::PointerScanFileKind>,
    last: Option<api::PointerScanOutcome>,
    /// The settings and pointer of the scan, kept to be saved with the paths.
    settings: Option<api::PointerScanSettings>,
    target: Option<String>,
    /// The file the paths were loaded from.
    source: Option<String>,
    last_file: Option<api::PointerScanFileOutcome>,
    serial: u64,
    /// Bumped whenever the results are discarded, so that a scan started
    /// before that does not store its result.
    epoch: u64,
}

impl PointerResults {
    fn state(&self) -> api::PointerScanState {
        api::PointerScanState {
            entry: self.entry,
            value_type: self.value_type.map(|t| t.to_string()),
            results: self.results.pointers.len() as u64,
            running: self.running.is_some() && self.file.is_none(),
            last: self.last.clone(),
            source: self.source.clone(),
            file: self.file,
            last_file: self.last_file.clone(),
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

impl ptscan_core::ScanProgress for Progress {
    fn report_bytes(&mut self, _: Size) -> Result<()> {
        Ok(())
    }

    fn report(&mut self, percentage: usize, results: u64) -> Result<()> {
        self.send(percentage as u32, 0, results);
        Ok(())
    }
}

impl ptscan_core::PointerScanInitialProgress for Progress {
    fn report(&mut self, queue: usize, results: usize) -> Result<()> {
        self.send(0, queue, results as u64);
        Ok(())
    }
}

impl ptscan_core::PointerScanBackreferenceProgress for Progress {
    fn report(&mut self, remaining: usize, results: usize) -> Result<()> {
        self.send(0, remaining, results as u64);
        Ok(())
    }
}

impl Shared {
    pub fn pointer_scan_state(&self) -> api::PointerScanState {
        lock(&self.pointer_results).state()
    }

    fn publish_pointer_scan_state(&self, current: &PointerResults) -> api::PointerScanState {
        let state = current.state();
        self.pointer_scan_state.send_replace(state.clone());
        state
    }

    /// Start a pointer scan in the background for the address the scratch
    /// entry `id` currently follows to, replacing the paths of the last one.
    pub fn start_pointer_scan(
        self: &Arc<Self>,
        id: u64,
        settings: api::PointerScanSettings,
    ) -> api::PointerScanResponse {
        let error = |e: &str| api::PointerScanResponse {
            error: Some(e.into()),
        };

        let Some(handle) = lock(&self.handle).clone() else {
            return error("not attached to a process");
        };

        let (pointer, value_type) = match lock(&self.scratch).get(id) {
            Ok(entry) => entry,
            Err(e) => return error(&e),
        };

        let mut current = lock(&self.pointer_results);

        if current.running.is_some() {
            return error(match current.file {
                None => "a pointer scan is already running",
                Some(_) => "the pointer paths are being saved or loaded",
            });
        }

        let token = Arc::new(AtomicBool::new(false));
        current.running = Some(token.clone());
        current.entry = Some(id);
        current.value_type = Some(value_type);
        current.results = Arc::default();
        current.last = None;
        current.settings = Some(settings.clone());
        current.target = Some(pointer.to_string());
        current.source = None;
        let epoch = current.epoch;
        self.pointer_progress
            .send_replace(api::PointerScanProgress::default());
        self.publish_pointer_scan_state(&current);
        drop(current);

        let shared = self.clone();

        std::thread::spawn(move || {
            let started = Instant::now();
            let token_ref = Token::from_raw(&token);
            let mut results = Paths::default();
            let result =
                shared.run_pointer_scan(&handle, &pointer, &settings, &mut results, token_ref);

            let mut current = lock(&shared.pointer_results);

            if current.epoch != epoch {
                return;
            }

            current.running = None;
            current.results = Arc::new(results);
            current.last = Some(api::PointerScanOutcome {
                results: current.results.pointers.len() as u64,
                cancelled: token_ref.test(),
                truncated: current.results.truncated,
                error: result.err().map(|e| format!("{e:#}")),
                duration_ms: started.elapsed().as_millis() as u64,
            });
            shared.publish_pointer_scan_state(&current);
        });

        api::PointerScanResponse::default()
    }

    /// The pointer scan of the GTK GUI: every pointer in memory is collected,
    /// then followed back from the address to modules.
    fn run_pointer_scan(
        &self,
        handle: &RwLock<ProcessHandle>,
        pointer: &PortablePointer,
        settings: &api::PointerScanSettings,
        results: &mut Paths,
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
            ScanContext::new(
                &self.pool,
                Some(token),
                progress(api::PointerScanStage::Initial),
            ),
            &filter,
            &mut addresses,
            &mut values,
            Default::default(),
        )?;

        let mut scan = PointerScan::new(&self.pool, &handle, token);
        scan.max_depth = usize::try_from(settings.max_depth)?;
        scan.max_offset = Size::new(settings.max_offset.into());
        scan.max_paths = usize::try_from(settings.max_paths)?;
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

        results.truncated = scan.truncated();

        // The references and paths visited are no longer needed, and are
        // large next to the results being converted.
        drop(scan);

        // Paths are only named when a page of them is asked for. Those through
        // a module that is no longer known are dropped.
        results.modules = handle
            .modules
            .modules
            .iter()
            .map(|m| m.name.clone())
            .collect();
        pointers.retain(|p| match p.base {
            Base::Module { id, .. } => usize::from(id) < results.modules.len(),
            _ => true,
        });
        pointers.shrink_to_fit();
        results.pointers = pointers;

        result
    }

    pub fn cancel_pointer_scan(&self) -> api::PointerScanState {
        let current = lock(&self.pointer_results);
        current.cancel();
        current.state()
    }

    pub(crate) fn reset_pointer_scan(&self) {
        let mut current = lock(&self.pointer_results);
        current.cancel();
        let epoch = current.epoch + 1;
        *current = PointerResults {
            epoch,
            serial: current.serial,
            ..PointerResults::default()
        };
        self.publish_pointer_scan_state(&current);
    }

    pub fn pointer_scan_results(&self, offset: u64, limit: u32) -> api::PointerScanResultsResponse {
        let (results, value_type) = {
            let current = lock(&self.pointer_results);
            (current.results.clone(), current.value_type)
        };

        let total = results.pointers.len();
        let start = usize::try_from(offset).unwrap_or(usize::MAX).min(total);
        let end = start
            .saturating_add(limit.min(api::MAX_SCAN_PAGE) as usize)
            .min(total);

        api::PointerScanResultsResponse {
            total: total as u64,
            offset: start as u64,
            results: results
                .pointers
                .get(start..end)
                .unwrap_or_default()
                .iter()
                .map(|p| results.display(p))
                .collect(),
            value_type: value_type.map(|t| t.to_string()),
        }
    }

    /// Start writing the paths of the last pointer scan to `path` in the
    /// background.
    pub fn save_pointer_scan(self: &Arc<Self>, path: String) -> api::PointerScanFileResponse {
        let mut current = lock(&self.pointer_results);

        if let Some(response) = busy(&current) {
            return response;
        }

        if current.last.is_none() {
            return api::PointerScanFileResponse {
                refusal: None,
                error: Some("there are no pointer paths to save".into()),
            };
        }

        if let Err(refused) = export::writable(&path) {
            return refusal(refused);
        }

        let results = current.results.clone();
        let meta = file::Meta {
            value_type: current.value_type.map(|t| t.to_string()),
            target: current.target.clone(),
            settings: current.settings.clone(),
        };

        let total = results.pointers.len() as u64;

        self.start_file(
            &mut current,
            api::PointerScanFileKind::Save,
            path,
            move |shared, path, token| {
                let progress = |written: u64| {
                    shared
                        .pointer_progress
                        .send_replace(api::PointerScanProgress {
                            stage: api::PointerScanStage::Saving,
                            percentage: percentage(written, total),
                            remaining: total.saturating_sub(written),
                            results: written,
                        });
                };

                let done = export::write_streamed::<file::PointerFile, _>(path, |out| {
                    file::write_value(out, &results, &meta, token, &mut |written| {
                        progress(written)
                    })
                })?;

                Ok(done.then_some((total, None)))
            },
        )
    }

    /// Start reading paths from the file at `path` in the background, which
    /// replace those of the last pointer scan once read.
    pub fn load_pointer_scan(self: &Arc<Self>, path: String) -> api::PointerScanFileResponse {
        let mut current = lock(&self.pointer_results);

        if let Some(response) = busy(&current) {
            return response;
        }

        if let Err(refused) = export::readable(&path) {
            return refusal(refused);
        }

        self.start_file(
            &mut current,
            api::PointerScanFileKind::Load,
            path,
            |shared, path, token| {
                let progress = |percentage: u32, results: u64| {
                    shared
                        .pointer_progress
                        .send_replace(api::PointerScanProgress {
                            stage: api::PointerScanStage::Loading,
                            percentage,
                            remaining: 0,
                            results,
                        });
                };

                let bytes = export::read_bytes::<file::PointerFile>(path, |read, size| {
                    progress(percentage(read, size), 0);
                    !token.test()
                })?;

                let Some(bytes) = bytes else {
                    return Ok(None);
                };

                let text = String::from_utf8(bytes)
                    .map_err(|_| Refused::new(api::FileRefusal::Malformed, "not UTF-8 text"))?;
                let decoded = export::decode::<file::PointerFile>(&text)?;
                drop(text);

                let Some((paths, meta, value_type)) =
                    file::into_paths(decoded, token, &mut |done| progress(100, done))?
                else {
                    return Ok(None);
                };

                let total = paths.pointers.len() as u64;
                Ok(Some((total, Some((paths, meta, value_type)))))
            },
        )
    }

    /// Run a save or load in the background, as `run` does it: `None` when
    /// cancelled, else the number of paths and, for a load, what was read.
    fn start_file<F>(
        self: &Arc<Self>,
        current: &mut PointerResults,
        kind: api::PointerScanFileKind,
        path: String,
        run: F,
    ) -> api::PointerScanFileResponse
    where
        F: 'static
            + Send
            + FnOnce(&Shared, &str, &Token) -> Result<Option<(u64, Option<Loaded>)>, Refused>,
    {
        let token = Arc::new(AtomicBool::new(false));
        current.running = Some(token.clone());
        current.file = Some(kind);
        let epoch = current.epoch;
        self.pointer_progress
            .send_replace(api::PointerScanProgress {
                stage: match kind {
                    api::PointerScanFileKind::Save => api::PointerScanStage::Saving,
                    api::PointerScanFileKind::Load => api::PointerScanStage::Loading,
                },
                ..Default::default()
            });
        self.publish_pointer_scan_state(current);

        let shared = self.clone();

        std::thread::spawn(move || {
            let started = Instant::now();
            let token_ref = Token::from_raw(&token);
            let result = run(&shared, &path, token_ref);

            let mut current = lock(&shared.pointer_results);

            if current.epoch != epoch {
                return;
            }

            current.running = None;
            current.file = None;
            current.serial = current.serial.wrapping_add(1);

            let mut outcome = api::PointerScanFileOutcome {
                serial: current.serial,
                kind,
                path: path.clone(),
                paths: 0,
                cancelled: false,
                refusal: None,
                error: None,
                duration_ms: started.elapsed().as_millis() as u64,
            };

            match result {
                Ok(None) => outcome.cancelled = true,
                Ok(Some((paths, loaded))) => {
                    outcome.paths = paths;

                    if let Some((results, meta, value_type)) = loaded {
                        current.entry = None;
                        current.value_type = value_type;
                        current.settings = meta.settings;
                        current.target = meta.target;
                        current.source = Some(path);
                        current.last = Some(api::PointerScanOutcome {
                            results: paths,
                            cancelled: false,
                            truncated: results.truncated,
                            error: None,
                            duration_ms: outcome.duration_ms,
                        });
                        current.results = Arc::new(results);
                    }
                }
                Err(refused) => {
                    outcome.refusal = Some(refused.refusal);
                    outcome.error = Some(refused.error);
                }
            }

            current.last_file = Some(outcome);
            shared.publish_pointer_scan_state(&current);
        });

        api::PointerScanFileResponse::default()
    }
}

/// What a load read: the paths, what the file says of them and their type.
type Loaded = (Paths, file::Meta, Option<Type>);

fn busy(current: &PointerResults) -> Option<api::PointerScanFileResponse> {
    let error = match (&current.running, current.file) {
        (None, _) => return None,
        (Some(_), None) => "a pointer scan is running",
        (Some(_), Some(_)) => "the pointer paths are already being saved or loaded",
    };

    Some(api::PointerScanFileResponse {
        refusal: None,
        error: Some(error.into()),
    })
}

fn refusal(refused: Refused) -> api::PointerScanFileResponse {
    api::PointerScanFileResponse {
        refusal: Some(refused.refusal),
        error: Some(refused.error),
    }
}

fn percentage(done: u64, total: u64) -> u32 {
    if total == 0 {
        return 100;
    }

    u32::try_from(u128::from(done.min(total)) * 100 / u128::from(total)).unwrap_or(100)
}
