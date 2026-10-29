#[cfg(unix)]
use std::process::{Child, Command};
use std::sync::Arc;
use std::time::Duration;

use musli_web::client::Handle;
use musli_web::tungstenite029;
use tokio::net::TcpListener;

const TIMEOUT: Duration = Duration::from_secs(20);

macro_rules! timeout {
    ($expr:expr) => {
        tokio::time::timeout(TIMEOUT, $expr)
            .await
            .unwrap_or_else(|_| panic!("timed out waiting for `{}`", stringify!($expr)))
    };
}

#[cfg(unix)]
struct KillOnDrop(Child);

#[cfg(unix)]
impl Drop for KillOnDrop {
    fn drop(&mut self) {
        _ = self.0.kill();
        _ = self.0.wait();
    }
}

async fn connect() -> Handle {
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    let shared = Arc::new(ptscan_server::Shared::default());
    tokio::spawn(ptscan_server::serve(listener, shared));

    let mut service = tungstenite029::connect(format!("ws://{addr}/ws")).build();
    let handle = service.handle().clone();
    tokio::spawn(async move { service.run().await });
    timeout!(handle.wait_until_open()).expect("open connection");
    handle
}

#[cfg(unix)]
#[tokio::test]
async fn list_attach_detach() {
    let child = KillOnDrop(Command::new("sleep").arg("60").spawn().unwrap());
    let pid = child.0.id();

    let handle = connect().await;
    let mut changes = handle.on_broadcast::<api::AttachStateChanged>();

    let packet = timeout!(handle.request().body(api::ListProcessesRequest).send()).unwrap();
    let list = packet.decode().unwrap();
    let info = list
        .processes
        .iter()
        .find(|p| p.pid == pid)
        .expect("child in process list");
    assert_eq!(info.name.as_deref(), Some("sleep"));
    // Processes of other users and kernel threads are unavailable, not errors.
    assert!(list.errors.is_empty(), "{:?}", list.errors);

    let packet = timeout!(handle.request().body(api::GetAttachStateRequest).send()).unwrap();
    assert_eq!(packet.decode().unwrap(), api::AttachState::default());

    let packet = timeout!(handle.request().body(api::AttachRequest { pid }).send()).unwrap();
    let response = packet.decode().unwrap();
    assert_eq!(response.error, None);
    let attached = response.state.process.clone().expect("attached");
    assert_eq!(attached.pid, pid);

    let event = timeout!(changes.recv()).unwrap().unwrap();
    assert_eq!(event.decode_event().unwrap(), response.state);

    let packet = timeout!(handle.request().body(api::DetachRequest).send()).unwrap();
    assert_eq!(packet.decode().unwrap(), api::AttachState::default());

    let event = timeout!(changes.recv()).unwrap().unwrap();
    assert_eq!(event.decode_event().unwrap(), api::AttachState::default());
}

#[tokio::test]
async fn attach_missing_process() {
    let handle = connect().await;

    let packet = timeout!(
        handle
            .request()
            .body(api::AttachRequest { pid: u32::MAX })
            .send()
    )
    .unwrap();

    let response = packet.decode().unwrap();
    assert!(response.error.is_some());
    assert_eq!(response.state, api::AttachState::default());
}
