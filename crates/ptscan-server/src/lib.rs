//! Websocket server owning the scanner and the attached process.

mod origin;

pub use self::origin::DevOrigin;
mod pointer_scan;
mod process_information;
mod scan;
mod scratch;

use std::sync::{Arc, Mutex, MutexGuard, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};

use anyhow::{Result, anyhow};
use axum::Router;
use axum::extract::State;
use axum::extract::ws::{WebSocket, WebSocketUpgrade};
use axum::middleware;
use axum::response::Response;
use axum::routing::any;
use musli_web::{axum08, ws};
use ptscan::ProcessHandle;
use tokio::net::TcpListener;
use tokio::sync::watch;

/// State shared by every connection.
pub struct Shared {
    handle: Mutex<Option<Arc<RwLock<ProcessHandle>>>>,
    state: watch::Sender<api::AttachState>,
    session: Mutex<scan::Session>,
    scan_state: watch::Sender<api::ScanState>,
    progress: watch::Sender<api::ScanProgress>,
    scratch: Mutex<scratch::Scratch>,
    scratch_list: watch::Sender<api::ScratchList>,
    pointer_session: Mutex<pointer_scan::PointerSession>,
    pointer_scan_state: watch::Sender<api::PointerScanState>,
    pointer_progress: watch::Sender<api::PointerScanProgress>,
    pool: rayon::ThreadPool,
    rescan_copy_limit: usize,
    dev_origins: Vec<DevOrigin>,
}

impl Default for Shared {
    fn default() -> Self {
        Self {
            handle: Mutex::new(None),
            state: watch::Sender::new(api::AttachState::default()),
            session: Mutex::default(),
            scan_state: watch::Sender::new(api::ScanState::default()),
            progress: watch::Sender::new(api::ScanProgress::default()),
            scratch: Mutex::default(),
            scratch_list: watch::Sender::new(api::ScratchList::default()),
            pointer_session: Mutex::default(),
            pointer_scan_state: watch::Sender::new(api::PointerScanState::default()),
            pointer_progress: watch::Sender::new(api::PointerScanProgress::default()),
            pool: rayon::ThreadPoolBuilder::new()
                .build()
                .expect("building scan thread pool"),
            rescan_copy_limit: 100_000_000,
            dev_origins: Vec::new(),
        }
    }
}

fn lock<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex.lock().unwrap_or_else(PoisonError::into_inner)
}

fn read<T>(lock: &RwLock<T>) -> RwLockReadGuard<'_, T> {
    lock.read().unwrap_or_else(PoisonError::into_inner)
}

fn write<T>(lock: &RwLock<T>) -> RwLockWriteGuard<'_, T> {
    lock.write().unwrap_or_else(PoisonError::into_inner)
}

impl Shared {
    /// Generations of more bytes than this are rescanned in place rather than
    /// copied, which means the rescan cannot be undone.
    pub fn with_rescan_copy_limit(mut self, bytes: usize) -> Self {
        self.rescan_copy_limit = bytes;
        self
    }

    /// Also accept websocket connections from pages served at `origin`.
    pub fn with_dev_origin(mut self, origin: DevOrigin) -> Self {
        self.dev_origins.push(origin);
        self
    }

    pub fn attach_state(&self) -> api::AttachState {
        self.state.borrow().clone()
    }

    fn attach(&self, pid: u32) -> Result<api::AttachState> {
        let handle = ProcessHandle::open(pid)?
            .ok_or_else(|| anyhow!("process {pid} does not exist or cannot be opened"))?;

        let state = api::AttachState {
            process: Some(api::ProcessInfo {
                pid,
                name: Some(handle.name.to_string_lossy().into_owned()),
            }),
        };

        let mut current = lock(&self.handle);
        *current = Some(Arc::new(RwLock::new(handle)));
        self.reset_scan();
        self.reset_pointer_scan();
        self.state.send_replace(state.clone());
        Ok(state)
    }

    fn detach(&self) -> api::AttachState {
        let mut current = lock(&self.handle);

        if current.take().is_some() {
            self.reset_scan();
            self.reset_pointer_scan();
            self.state.send_replace(api::AttachState::default());
        }

        api::AttachState::default()
    }
}

pub fn list_processes() -> Result<api::ListProcessesResponse> {
    let mut response = api::ListProcessesResponse::default();

    for pid in ptscan::processes()? {
        let result = ptscan::Process::builder()
            .query_information()
            .vm_read()
            .build(pid)
            .and_then(|p| Ok(p.module_base_name()?));

        match result {
            Ok(name) => response.processes.push(api::ProcessInfo {
                pid,
                name: name.into_string().ok(),
            }),
            Err(e) if e.is_process_unavailable() => {}
            Err(e) => response.errors.push(format!("process {pid}: {e:#}")),
        }
    }

    Ok(response)
}

/// The router; it must be served with [`origin::Connection`] connect info,
/// as [`serve`] does.
fn router(shared: Arc<Shared>) -> Router {
    Router::new()
        .route("/ws", any(handler).with_state(shared.clone()))
        .layer(middleware::from_fn_with_state(shared, origin::check))
}

pub async fn serve(listener: TcpListener, shared: Arc<Shared>) -> Result<()> {
    let service = router(shared).into_make_service_with_connect_info::<origin::Connection>();
    axum::serve(listener, service).await?;
    Ok(())
}

async fn handler(ws: WebSocketUpgrade, State(shared): State<Arc<Shared>>) -> Response {
    ws.on_upgrade(move |socket| async move {
        if let Err(error) = run(socket, shared).await {
            tracing::error!("Websocket error: {error:#}");
        }
    })
}

async fn run(socket: WebSocket, shared: Arc<Shared>) -> Result<()> {
    let mut state = shared.state.subscribe();
    let mut scan_state = shared.scan_state.subscribe();
    let mut progress = shared.progress.subscribe();
    let mut scratch = shared.scratch_list.subscribe();
    let mut pointer_state = shared.pointer_scan_state.subscribe();
    let mut pointer_progress = shared.pointer_progress.subscribe();
    let handler = Handler {
        shared: shared.clone(),
    };

    let mut server = axum08::server(socket, handler).connect().await?;

    loop {
        tokio::select! {
            changed = state.changed() => {
                if changed.is_err() {
                    break;
                }

                let event = state.borrow_and_update().clone();
                server.broadcast(event)?;
            }
            changed = scan_state.changed() => {
                if changed.is_err() {
                    break;
                }

                let event = scan_state.borrow_and_update().clone();
                server.broadcast(event)?;
            }
            changed = progress.changed() => {
                if changed.is_err() {
                    break;
                }

                let event = progress.borrow_and_update().clone();
                server.broadcast(event)?;
            }
            changed = scratch.changed() => {
                if changed.is_err() {
                    break;
                }

                let event = scratch.borrow_and_update().clone();
                server.broadcast(event)?;
            }
            changed = pointer_state.changed() => {
                if changed.is_err() {
                    break;
                }

                let event = pointer_state.borrow_and_update().clone();
                server.broadcast(event)?;
            }
            changed = pointer_progress.changed() => {
                if changed.is_err() {
                    break;
                }

                let event = pointer_progress.borrow_and_update().clone();
                server.broadcast(event)?;
            }
            result = server.run() => {
                result?;
                break;
            }
        }
    }

    Ok(())
}

#[derive(Clone)]
struct Handler {
    shared: Arc<Shared>,
}

impl ws::Handler for Handler {
    type Id = api::Request;
    type Response = Result<bool>;

    async fn handle(
        &self,
        id: Self::Id,
        incoming: &mut ws::Incoming<'_>,
        outgoing: &mut ws::Outgoing<'_>,
    ) -> Self::Response {
        match id {
            api::Request::ListProcesses => {
                let response = tokio::task::spawn_blocking(list_processes).await??;
                outgoing.write(response);
            }
            api::Request::Attach => {
                let Some(request) = incoming.read::<api::AttachRequest>() else {
                    return Ok(false);
                };

                let shared = self.shared.clone();
                let result =
                    tokio::task::spawn_blocking(move || shared.attach(request.pid)).await?;

                let response = match result {
                    Ok(state) => api::AttachResponse { state, error: None },
                    Err(e) => api::AttachResponse {
                        state: self.shared.attach_state(),
                        error: Some(format!("{e:#}")),
                    },
                };

                outgoing.write(response);
            }
            api::Request::Detach => {
                outgoing.write(self.shared.detach());
            }
            api::Request::GetAttachState => {
                outgoing.write(self.shared.attach_state());
            }
            api::Request::Scan => {
                let Some(request) = incoming.read::<api::ScanRequest>() else {
                    return Ok(false);
                };

                let shared = self.shared.clone();
                let response =
                    tokio::task::spawn_blocking(move || shared.start_scan(request)).await?;
                outgoing.write(response);
            }
            api::Request::CancelScan => {
                outgoing.write(self.shared.cancel_scan());
            }
            api::Request::ResetScan => {
                outgoing.write(self.shared.reset_scan());
            }
            api::Request::UndoScan => {
                outgoing.write(self.shared.undo_scan());
            }
            api::Request::RedoScan => {
                outgoing.write(self.shared.redo_scan());
            }
            api::Request::GetScanState => {
                outgoing.write(self.shared.scan_state());
            }
            api::Request::ScanResults => {
                let Some(request) = incoming.read::<api::ScanResultsRequest>() else {
                    return Ok(false);
                };

                let shared = self.shared.clone();
                let result = tokio::task::spawn_blocking(move || {
                    shared.scan_results(request.offset, request.limit)
                })
                .await?;

                let response = result.unwrap_or_else(|e| api::ScanResultsResponse {
                    offset: request.offset,
                    error: Some(format!("{e:#}")),
                    ..Default::default()
                });

                outgoing.write(response);
            }
            api::Request::RemoveScanResults => {
                let Some(request) = incoming.read::<api::RemoveScanResultsRequest>() else {
                    return Ok(false);
                };

                let shared = self.shared.clone();
                let response = tokio::task::spawn_blocking(move || {
                    shared.remove_scan_results(&request.addresses)
                })
                .await?;
                outgoing.write(response);
            }
            api::Request::WriteScanValue => {
                let Some(request) = incoming.read::<api::WriteScanValueRequest>() else {
                    return Ok(false);
                };

                let shared = self.shared.clone();
                let response =
                    tokio::task::spawn_blocking(move || shared.write_scan_value(request)).await?;
                outgoing.write(response);
            }
            api::Request::GetScratch => {
                let shared = self.shared.clone();
                outgoing.write(tokio::task::spawn_blocking(move || shared.scratch()).await?);
            }
            api::Request::AddScratch => {
                let Some(request) = incoming.read::<api::AddScratchRequest>() else {
                    return Ok(false);
                };

                let shared = self.shared.clone();
                let response =
                    tokio::task::spawn_blocking(move || shared.add_scratch(request)).await?;
                outgoing.write(response);
            }
            api::Request::EditScratch => {
                let Some(request) = incoming.read::<api::EditScratchRequest>() else {
                    return Ok(false);
                };

                let shared = self.shared.clone();
                let response =
                    tokio::task::spawn_blocking(move || shared.edit_scratch(request)).await?;
                outgoing.write(response);
            }
            api::Request::WriteScratchValue => {
                let Some(request) = incoming.read::<api::WriteScratchValueRequest>() else {
                    return Ok(false);
                };

                let shared = self.shared.clone();
                let response =
                    tokio::task::spawn_blocking(move || shared.write_scratch_value(request))
                        .await?;
                outgoing.write(response);
            }
            api::Request::RemoveScratch => {
                let Some(request) = incoming.read::<api::RemoveScratchRequest>() else {
                    return Ok(false);
                };

                let shared = self.shared.clone();
                let response =
                    tokio::task::spawn_blocking(move || shared.remove_scratch(&request.ids))
                        .await?;
                outgoing.write(response);
            }
            api::Request::PointerScan => {
                let Some(request) = incoming.read::<api::PointerScanRequest>() else {
                    return Ok(false);
                };

                let shared = self.shared.clone();
                let response =
                    tokio::task::spawn_blocking(move || shared.start_pointer_scan(request.id))
                        .await?;
                outgoing.write(response);
            }
            api::Request::CancelPointerScan => {
                outgoing.write(self.shared.cancel_pointer_scan());
            }
            api::Request::GetPointerScanState => {
                outgoing.write(self.shared.pointer_scan_state());
            }
            api::Request::PointerScanResults => {
                let Some(request) = incoming.read::<api::PointerScanResultsRequest>() else {
                    return Ok(false);
                };

                let shared = self.shared.clone();
                let response = tokio::task::spawn_blocking(move || {
                    shared.pointer_scan_results(request.offset, request.limit)
                })
                .await?;
                outgoing.write(response);
            }
            api::Request::GetProcessInformation => {
                let Some(request) = incoming.read::<api::ProcessInformationRequest>() else {
                    return Ok(false);
                };

                let shared = self.shared.clone();
                let response = tokio::task::spawn_blocking(move || {
                    shared.process_information(&request.filter)
                })
                .await?;
                outgoing.write(response);
            }
            _ => return Ok(false),
        }

        Ok(true)
    }
}
