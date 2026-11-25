use std::net::SocketAddr;
use std::path::Path;
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

async fn server(shared: ptscan::Shared) -> SocketAddr {
    let shared = Arc::new(shared);
    shared.open_last_session().await.unwrap();
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(ptscan::serve(listener, shared));
    addr
}

/// A server keeping its settings in the database at `path`, as a fresh start
/// of `ptscan --db <path>` would.
async fn start(path: &Path) -> Handle {
    let db = ptscan::Db::open(path).unwrap();
    connect(server(ptscan::Shared::default().with_db(db)).await).await
}

async fn connect(addr: SocketAddr) -> Handle {
    let mut service = tungstenite029::connect(format!("ws://{addr}/ws")).build();
    let handle = service.handle().clone();
    tokio::spawn(async move { service.run().await });
    timeout!(handle.wait_until_open()).expect("open connection");
    handle
}

async fn send<T>(handle: &Handle, body: T) -> api::SessionResponse
where
    T: musli_web::api::Request,
{
    timeout!(handle.request().body(body).send())
        .unwrap()
        .decode_any()
        .unwrap()
}

async fn session_state(handle: &Handle) -> api::SessionState {
    timeout!(handle.request().body(api::GetSessionStateRequest).send())
        .unwrap()
        .decode()
        .unwrap()
}

async fn create(handle: &Handle, name: &str) -> u64 {
    let response = send(handle, api::CreateSessionRequest { name: name.into() }).await;
    assert_eq!(response.error, None);

    response
        .state
        .sessions
        .iter()
        .find(|s| s.name == name.trim())
        .expect("created session")
        .id
}

fn form(filter: &str) -> api::ScanForm {
    api::ScanForm {
        filter: filter.into(),
        value: "value + 1".into(),
        alignment: Some(4),
        suspend: true,
        modules_only: false,
    }
}

fn count(path: &Path, sql: &str, id: u64) -> i64 {
    let c = sqll::OpenOptions::new().read_write().open(path).unwrap();
    let mut stmt = c.prepare(sql).unwrap();
    stmt.bind((i64::try_from(id).unwrap(),)).unwrap();
    stmt.next::<i64>().unwrap().unwrap()
}

#[tokio::test]
async fn starts_with_a_default_session() {
    let dir = tempfile::tempdir().unwrap();
    let handle = start(&dir.path().join("ptscan.db")).await;

    let state = session_state(&handle).await;
    assert_eq!(state.sessions.len(), 1);
    let current = state.current().expect("open session");
    assert_eq!(current.name, "Default");
    assert_eq!(current.target, None);
    assert_eq!(state.scan, api::ScanForm::default());
    assert_eq!(state.pointer_scan, api::PointerScanSettings::default());
}

#[tokio::test]
async fn create_rename_open_delete() {
    let dir = tempfile::tempdir().unwrap();
    let handle = start(&dir.path().join("ptscan.db")).await;
    let mut changes = handle.on_broadcast::<api::SessionStateChanged>();
    let default = session_state(&handle).await.current.unwrap();

    let response = send(&handle, api::CreateSessionRequest { name: "  ".into() }).await;
    assert!(response.error.is_some());

    let second = create(&handle, " Second ").await;
    assert_ne!(second, default);

    let event = timeout!(changes.recv()).unwrap().unwrap();
    let event = event.decode_event().unwrap();
    assert_eq!(event.sessions.len(), 2);
    assert_eq!(event.current, Some(default), "creating does not open");

    let response = send(
        &handle,
        api::RenameSessionRequest {
            id: second,
            name: "Renamed".into(),
        },
    )
    .await;
    assert_eq!(response.error, None);
    let names: Vec<_> = response.state.sessions.iter().map(|s| &s.name).collect();
    assert!(names.contains(&&"Renamed".to_owned()), "{names:?}");

    let response = send(&handle, api::OpenSessionRequest { id: second }).await;
    assert_eq!(response.error, None);
    assert_eq!(response.state.current, Some(second));
    assert_eq!(response.state.sessions.first().map(|s| s.id), Some(second));

    let response = send(&handle, api::DeleteSessionRequest { id: second }).await;
    assert!(response.error.is_some(), "the open session is kept");

    let response = send(&handle, api::OpenSessionRequest { id: 9999 }).await;
    assert!(response.error.is_some());
    assert_eq!(response.state.current, Some(second));

    let response = send(&handle, api::DeleteSessionRequest { id: default }).await;
    assert_eq!(response.error, None);
    let ids: Vec<_> = response.state.sessions.iter().map(|s| s.id).collect();
    assert_eq!(ids, [second]);
}

#[tokio::test]
async fn settings_survive_a_restart() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("ptscan.db");

    let handle = start(&path).await;
    let first = session_state(&handle).await.current.unwrap();

    let response = send(
        &handle,
        api::SetScanFormRequest {
            form: form("value == 1"),
        },
    )
    .await;
    assert_eq!(response.error, None);

    let settings = api::PointerScanSettings {
        max_depth: 3,
        max_offset: 0x200,
    };
    let response = send(
        &handle,
        api::SetPointerScanSettingsRequest {
            settings: settings.clone(),
        },
    )
    .await;
    assert_eq!(response.error, None);

    let response = send(
        &handle,
        api::SetPointerScanSettingsRequest {
            settings: api::PointerScanSettings {
                max_depth: 0,
                max_offset: 0x200,
            },
        },
    )
    .await;
    assert!(response.error.is_some());
    assert_eq!(response.state.pointer_scan, settings);

    let second = create(&handle, "Second").await;
    let response = send(&handle, api::OpenSessionRequest { id: second }).await;
    assert_eq!(response.state.scan, api::ScanForm::default());
    assert_eq!(
        response.state.pointer_scan,
        api::PointerScanSettings::default()
    );

    let mut second_form = form("value != 0");
    second_form.alignment = None;
    send(
        &handle,
        api::SetScanFormRequest {
            form: second_form.clone(),
        },
    )
    .await;
    drop(handle);

    let handle = start(&path).await;
    let state = session_state(&handle).await;
    assert_eq!(state.current, Some(second), "the open session is reopened");
    assert_eq!(state.sessions.len(), 2);
    assert_eq!(state.scan, second_form);

    let response = send(&handle, api::OpenSessionRequest { id: first }).await;
    assert_eq!(response.error, None);
    assert_eq!(response.state.scan, form("value == 1"));
    assert_eq!(response.state.pointer_scan, settings);
}

#[tokio::test]
async fn deleting_a_session_removes_its_rows() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("ptscan.db");
    let handle = start(&path).await;
    let first = session_state(&handle).await.current.unwrap();

    let second = create(&handle, "Second").await;
    send(&handle, api::OpenSessionRequest { id: second }).await;
    send(
        &handle,
        api::SetScanFormRequest {
            form: form("value == 2"),
        },
    )
    .await;
    send(&handle, api::OpenSessionRequest { id: first }).await;

    let rows = "SELECT count(*) FROM session_config WHERE session_id = ?";
    assert!(count(&path, rows, second) > 0);

    let response = send(&handle, api::DeleteSessionRequest { id: second }).await;
    assert_eq!(response.error, None);
    assert_eq!(count(&path, rows, second), 0);
    assert_eq!(
        count(&path, "SELECT count(*) FROM sessions WHERE id = ?", second),
        0
    );
    assert!(count(&path, rows, first) > 0);
}

#[tokio::test]
async fn without_a_database() {
    let handle = connect(server(ptscan::Shared::default()).await).await;

    let state = session_state(&handle).await;
    assert_eq!(state.current, None);
    assert!(state.sessions.is_empty());

    let response = send(&handle, api::CreateSessionRequest { name: "A".into() }).await;
    assert!(response.error.is_some());

    let response = send(
        &handle,
        api::SetScanFormRequest {
            form: form("value == 3"),
        },
    )
    .await;
    assert_eq!(response.error, None);
    assert_eq!(response.state.scan, form("value == 3"));
}

#[cfg(unix)]
#[tokio::test]
async fn attaching_records_the_target() {
    use std::process::Command;

    struct KillOnDrop(std::process::Child);

    impl Drop for KillOnDrop {
        fn drop(&mut self) {
            _ = self.0.kill();
            _ = self.0.wait();
        }
    }

    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("ptscan.db");
    let child = KillOnDrop(Command::new("sleep").arg("60").spawn().unwrap());
    let pid = child.0.id();

    let handle = start(&path).await;
    let response: api::AttachResponse =
        timeout!(handle.request().body(api::AttachRequest { pid }).send())
            .unwrap()
            .decode()
            .unwrap();
    assert_eq!(response.error, None);

    let state = session_state(&handle).await;
    let target = state.current().unwrap().target.clone().expect("target");
    assert_eq!(target.name, "sleep");
    drop(handle);

    let handle = start(&path).await;
    let attached: api::AttachState =
        timeout!(handle.request().body(api::GetAttachStateRequest).send())
            .unwrap()
            .decode()
            .unwrap();
    assert_eq!(attached.process, None, "opening a session does not attach");

    let state = session_state(&handle).await;
    assert_eq!(state.current().unwrap().target, Some(target));
}
