//! Websocket server owning the scanner and the attached process.

use std::sync::{Arc, Mutex};

use anyhow::{Result, anyhow};
use axum::Router;
use axum::extract::State;
use axum::extract::ws::{WebSocket, WebSocketUpgrade};
use axum::response::Response;
use axum::routing::any;
use musli_web::{axum08, ws};
use ptscan::ProcessHandle;
use tokio::net::TcpListener;
use tokio::sync::watch;

/// State shared by every connection.
pub struct Shared {
    handle: Mutex<Option<ProcessHandle>>,
    state: watch::Sender<api::AttachState>,
}

impl Default for Shared {
    fn default() -> Self {
        Self {
            handle: Mutex::new(None),
            state: watch::Sender::new(api::AttachState::default()),
        }
    }
}

impl Shared {
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

        let mut current = self.handle.lock().unwrap_or_else(|e| e.into_inner());
        *current = Some(handle);
        self.state.send_replace(state.clone());
        Ok(state)
    }

    fn detach(&self) -> api::AttachState {
        let mut current = self.handle.lock().unwrap_or_else(|e| e.into_inner());

        if current.take().is_some() {
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
            .map_err(anyhow::Error::from)
            .and_then(|p| Ok(p.module_base_name()?));

        match result {
            Ok(name) => response.processes.push(api::ProcessInfo {
                pid,
                name: name.into_string().ok(),
            }),
            Err(e) => response.errors.push(format!("process {pid}: {e:#}")),
        }
    }

    Ok(response)
}

pub fn router(shared: Arc<Shared>) -> Router {
    Router::new().route("/ws", any(handler).with_state(shared))
}

pub async fn serve(listener: TcpListener, shared: Arc<Shared>) -> Result<()> {
    axum::serve(listener, router(shared)).await?;
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
            _ => return Ok(false),
        }

        Ok(true)
    }
}
