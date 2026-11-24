//! ptscan finds the memory addresses behind the numbers in a running program, the
//! way Cheat Engine does, from a web UI in your browser on Windows and Linux. Attach
//! to a process, scan for a value, change it in the program, narrow the results
//! until one address is left, then watch it or write to it. When the program moves
//! it around between runs, a pointer scan finds a stable path back to it.
//!
//! * **Typed scans.** Integers, floats, pointers, strings in any encoding and
//!   byte arrays, and the expression language below to narrow them with
//!   comparisons, arithmetic, dereferences and regular expressions.
//! * **Pointer scans.** Resolve addresses that move into module-relative pointer
//!   paths which survive restarts.
//! * **Fast.** Scans run in parallel over the whole address space, with the exact
//!   value patterns of initial scans searched in batches.
//! * **A browser UI.** `ptscan` serves a local web UI, so there is nothing
//!   to attach a debugger-style window to, and no GUI toolkit to install.
//! * **Written in Rust.** One native binary per platform, with a library you can
//!   embed in your own tools.
//!
//! <br>
//!
//! ## Install
//!
//! On Linux (`x86_64`), install the latest release into `~/.local/bin` (set
//! `PTSCAN_INSTALL_DIR` to change it):
//!
//! ```sh
//! curl -fsSL https://udoprog.github.io/ptscan/install.sh | sh
//! ```
//!
//! On Windows, download the zip from the
//! [GitHub releases](https://github.com/udoprog/ptscan/releases).
//!
//! To update an existing installation to the latest release:
//!
//! ```sh
//! ptscan update
//! ```
//!
//! <br>
//!
//! ## Usage
//!
//! `ptscan` serves the web UI and its websocket API (at `/ws`) on
//! `127.0.0.1:7431`, and opens the UI in the default browser on launch:
//!
//! ```sh
//! ptscan [--bind ADDR] [--no-browser] [--no-tray]
//! ```
//!
//! Run `ptscan update` to replace the binary with the latest GitHub release.
//!
//! On Windows it also shows a notification area icon whose menu opens the UI,
//! shows the attached process and quits the server; `--no-tray` leaves it out.
//!
//! Scans and watches select values using _filters_, the expression language
//! described below.
//!
//! On Linux, reading another process's memory needs ptrace access to it, such
//! as `kernel.yama.ptrace_scope = 0` or `CAP_SYS_PTRACE`. See
//! [Linux](https://github.com/udoprog/ptscan#linux) for details.
//!
//! <br>
//!
//! ## Building
//!
//! The server embeds the web UI built by [trug](https://github.com/udoprog/trug), so build
//! the UI first (it needs the `wasm32-unknown-unknown` target):
//!
//! ```sh
//! trug build --release
//! cargo build --release -p ptscan
//! ```
//!
//! Without the first step the server still builds, but serves a page saying the
//! UI is missing. Releases are packaged for Linux (`.tar.gz`) and Windows
//! (`.zip`) by [kick](https://github.com/udoprog/kick).
//!
//! For development, run the server and serve the UI with `trug serve`, which
//! proxies `/ws` to the server:
//!
//! ```sh
//! cargo run -p ptscan -- --no-browser --dev-origin http://127.0.0.1:8080
//! trug serve
//! ```
//!
//! <br>
//!
//! ## Types
//!
//! All expressions support the following types:
//!
//! * `none` - The special `none` type. Any value of this type is _undefined_. See the section below for more details.
//! * `pointer` -  A pointer value, who's size depends on the process being attached to.
//! * `u8` - An unsigned 8-bit number.
//! * `i8` - A signed 8-bit number.
//! * `u16` - An unsigned 16-bit number.
//! * `i16` - A signed 16-bit number.
//! * `u32` - An unsigned 32-bit number.
//! * `i32` - A signed 32-bit number.
//! * `u64` - An unsigned 64-bit number.
//! * `i64` - A signed 64-bit number.
//! * `u128` - An unsigned 128-bit number.
//! * `i128` - A signed 128-bit number.
//! * `f32` - A 32-bit floating point number.
//! * `f64` - A 64-bit floating point number.
//! * `string` - A null-terminated string with the default encoding (`utf-8`).
//! * `string/<encoding>` - A null-terminated string with the specified `<encoding>`, as per [the whatwg Encoding standard](https://encoding.spec.whatwg.org/#names-and-labels).
//! * `bytes` - An unsized byte array.
//! * `bytes/<len>` - A byte array of length `<len>`.
//!
//! <br>
//!
//! #### The `none` type
//!
//! The `none` type is a special type which any value can assume.
//!
//! Any comparison (`==`, `!=`, `<`, `>`, ..) _except_ `value is none` or `value is not none` is `false`.
//!
//! Any expression (`+`, `-`, `*`, `/`) involving a value of the `none` type results in another `none` type.
//! For example, `*value + 1` would be `none` if `*value` is not a valid pointer.
//!
//! A value of type `none` still retains the old type information to make sure it can be successfully refresh if needed.
//! Therefore you'll see things like:
//!
//! ```text
//! none(u128)
//! ```
//!
//! Which means that an expression which was expected to evaluate to a `u128` value, evaluated to `none`.
//!
//! <br>
//!
//! ## Value Expressions
//!
//! Value expressions resolve to a specific _value_ in memory.
//! The take the following forms (the earlier it is listed, the higher its precedence):
//!
//! * `value` - The current as-we-are-scanning value of the memory location.
//! * `initial` - The initial value of the memory location, from the initial scan.
//! * `last` - The last value of the memory location, from the previous scan.
//! * `<number>` - A whole number literal. Default type is `u32`.
//!   * Example: `42`
//! * `<decimal>` - A decimal number literal. Default type is `f32`.
//!   * Example: `42.42`
//! * `<string>` - A string literal. Default type is `string/utf-8`.
//!   * Example: `"ui_boot"`
//! * `(<value>)` - Override default precedence.
//! * `*<value>` - Dereference the given value. This treats it as an address and follows the pointer.
//!   * Example: `*value`
//! * `&<value>` - Take the address of the given value. _not every value has an address_, like `&(value + 42)` is not valid.
//!   * Example: `&value`
//!   * Example: `*(&value + 0x40)`
//! * `<value> as <ty>` - Explicitly treat the value of `<value>` as the type `<ty>`.
//!   * Example: `value as u64`
//!   * Example: `value as u128 == 1`
//! * `<value> * <value>` - Multiply two values from each other.
//!   * Example: `value * 42`
//! * `<value> / <value>` - Divide two values.
//!   * Example: `value / 42`
//! * `<value> + <value>` - Add two values together.
//!   * Example: `value + 42`
//! * `<value> - <value>` - Subtract two values from each other.
//!   * Example: `value - 42`
//!
//! <br>
//!
//! ## Filters
//!
//! <br>
//!
//! #### Regular expressions `<a> ~ <b>`
//!
//! Test if `<a>` matches the regular expression specified in `<b>`.
//!
//! <br>
//!
//! #### Equality `<a> == <b>`
//!
//! Checks that `<a>` is equal to value `<b>`.
//!
//! For the initial scan, this allows for the following optimization:
//! * `value == 42` - scan in batches for the exact memory pattern of `42`
//!
//! <br>
//!
//! #### Non-equality `<a> != <b>`
//!
//! Checks that `<a>` is _not_ equal to value `<b>`.
//!
//! For the initial scan, this allows for the following optimization:
//! * `value != 0` - scans for non-zero memory addresses.
//!
//! <br>
//!
//! #### Less than `<a> < <b>`
//!
//! Checks that `<a>` is less than value `<b>`.
//!
//! For the initial scan, this allows for the following optimization:
//! * `value < 0` - scans for non-zero memory addresses.
//!
//! <br>
//!
//! #### Less than or equal `<a> <= <b>`
//!
//! Checks that `<a>` is less or equal to the value `<b>`.
//!
//! For the initial scan, this allows for the following optimization:
//! * `value <= 1` - scans for non-zero memory addresses.
//!
//! <br>
//!
//! #### Greater than `<a> > <b>`
//!
//! Checks that `<a>` is greater than the value `<b>`.
//!
//! For the initial scan, this allows for the following optimization:
//! * `value > 0` - scans for non-zero memory addresses.
//!
//! <br>
//!
//! #### Greater than or equal `<a> >= <b>`
//!
//! Checks that `<a>` is greater than or equal to the value `<b>`.
//!
//! For the initial scan, this allows for the following optimization:
//! * `value >= 1` - scans for non-zero memory addresses.
//!
//! <br>
//!
//! ## Linux
//!
//! On Linux, ptscan reads process memory with `process_vm_readv`, falling back to
//! `/proc/<pid>/mem`. Both need ptrace access to the target process:
//!
//! * With `kernel.yama.ptrace_scope = 0`, any process owned by the same user can be
//!   read. Set it with `sudo sysctl kernel.yama.ptrace_scope=0`, and put
//!   `kernel.yama.ptrace_scope = 0` in a file under `/etc/sysctl.d/` to keep it
//!   across reboots.
//! * With the common default of `1`, only processes started by ptscan can be read.
//! * Processes owned by other users, or any process under `ptrace_scope = 2`, need
//!   `CAP_SYS_PTRACE`: run ptscan as root or grant it with
//!   `sudo setcap cap_sys_ptrace=ep /path/to/ptscan`.
//! * `ptrace_scope = 3` forbids reading other processes entirely.
//!
//! <br>
//!
//! ## License
//!
//! This project bundles Adwaita icons parts of the GNOME Project under the Creative
//! Commons Attribution-Share Alike 3.0 license (see [licenses](https://github.com/udoprog/ptscan/tree/main/licenses)).
//!
//! You can find them at <https://www.gnome.org>.

mod origin;

pub use self::origin::DevOrigin;
mod pointer_scan;
mod process_information;
mod scan;
mod scratch;
pub mod web;

use std::sync::{Arc, Mutex, MutexGuard, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};

use anyhow::{Result, anyhow};
use axum::Router;
use axum::extract::State;
use axum::extract::ws::{WebSocket, WebSocketUpgrade};
use axum::middleware;
use axum::response::Response;
use axum::routing::any;
use musli_web::{axum08, ws};
use ptscan_core::ProcessHandle;
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

    pub fn subscribe_attach_state(&self) -> watch::Receiver<api::AttachState> {
        self.state.subscribe()
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

    for pid in ptscan_core::processes()? {
        let result = ptscan_core::Process::builder()
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
        .fallback(axum::routing::get(web::handler))
        .layer(middleware::from_fn_with_state(shared, origin::check))
}

pub async fn serve(listener: TcpListener, shared: Arc<Shared>) -> Result<()> {
    serve_until(listener, shared, std::future::pending()).await
}

/// Serve until `shutdown` completes, then stop accepting connections and wait
/// for open requests to finish.
pub async fn serve_until(
    listener: TcpListener,
    shared: Arc<Shared>,
    shutdown: impl Future<Output = ()> + Send + 'static,
) -> Result<()> {
    let service = router(shared).into_make_service_with_connect_info::<origin::Connection>();
    axum::serve(listener, service)
        .with_graceful_shutdown(shutdown)
        .await?;
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
