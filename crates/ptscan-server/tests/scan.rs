use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

use musli_web::client::Handle;
use musli_web::tungstenite029;
use tokio::io::{AsyncReadExt as _, AsyncWriteExt as _};
use tokio::net::{TcpListener, TcpStream};

const TIMEOUT: Duration = Duration::from_secs(60);

macro_rules! timeout {
    ($expr:expr) => {
        tokio::time::timeout(TIMEOUT, $expr)
            .await
            .unwrap_or_else(|_| panic!("timed out waiting for `{}`", stringify!($expr)))
    };
}

async fn server() -> SocketAddr {
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    let shared = Arc::new(ptscan_server::Shared::default());
    tokio::spawn(ptscan_server::serve(listener, shared));
    addr
}

async fn connect(addr: SocketAddr) -> Handle {
    let mut service = tungstenite029::connect(format!("ws://{addr}/ws")).build();
    let handle = service.handle().clone();
    tokio::spawn(async move { service.run().await });
    timeout!(handle.wait_until_open()).expect("open connection");
    handle
}

async fn scan(handle: &Handle, filter: &str) -> api::ScanResponse {
    let request = api::ScanRequest {
        filter: filter.into(),
        value: String::new(),
        alignment: Some(8),
    };

    timeout!(handle.request().body(request).send())
        .unwrap()
        .decode()
        .unwrap()
}

async fn results(handle: &Handle) -> api::ScanResultsResponse {
    let request = api::ScanResultsRequest {
        offset: 0,
        limit: api::MAX_SCAN_PAGE,
    };

    timeout!(handle.request().body(request).send())
        .unwrap()
        .decode()
        .unwrap()
}

#[tokio::test]
async fn scan_needs_attached_process() {
    let handle = connect(server().await).await;
    let response = scan(&handle, "value == 1u32").await;
    assert!(response.error.is_some());

    let page = results(&handle).await;
    assert_eq!(page.total, 0);
    assert!(page.results.is_empty());
}

async fn upgrade(addr: SocketAddr, host: &str, origin: Option<&str>) -> String {
    let mut socket = TcpStream::connect(addr).await.unwrap();

    let mut request = format!(
        "GET /ws HTTP/1.1\r\nHost: {host}\r\nConnection: Upgrade\r\nUpgrade: websocket\r\n\
         Sec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
    );

    if let Some(origin) = origin {
        request.push_str(&format!("Origin: {origin}\r\n"));
    }

    request.push_str("\r\n");
    socket.write_all(request.as_bytes()).await.unwrap();

    timeout!(async {
        let mut response = Vec::new();

        while !response.ends_with(b"\r\n\r\n") {
            response.push(socket.read_u8().await.unwrap());
        }

        String::from_utf8(response).unwrap()
    })
}

#[tokio::test]
async fn upgrades_from_other_origins_are_refused() {
    let addr = server().await;
    let host = addr.to_string();
    let own = format!("http://{host}");

    let response = upgrade(addr, &host, Some("http://evil.example")).await;
    assert!(response.starts_with("HTTP/1.1 403"), "{response}");

    let response = upgrade(addr, "evil.example", Some("http://evil.example")).await;
    assert!(response.starts_with("HTTP/1.1 403"), "{response}");

    let response = upgrade(addr, &host, Some(&own)).await;
    assert!(response.starts_with("HTTP/1.1 101"), "{response}");

    let response = upgrade(addr, &host, None).await;
    assert!(response.starts_with("HTTP/1.1 101"), "{response}");
}

#[cfg(target_os = "linux")]
mod linux {
    use std::io::{BufRead as _, BufReader, Write as _};
    use std::process::{Child, ChildStdin, Command, Stdio};

    use super::*;

    /// Wait for a finished scan which leaves `generations` generations.
    async fn finished(
        changes: &mut musli_web::client::Listener<api::ScanStateChanged>,
        generations: u32,
    ) -> api::ScanState {
        loop {
            let event = timeout!(changes.recv()).unwrap().unwrap();
            let state = event.decode_event().unwrap();

            if !state.running && state.last.is_some() && state.generations == generations {
                return state;
            }
        }
    }

    const HOLD_ENV: &str = "PTSCAN_SERVER_TEST_HOLD_VALUE";
    const HELD: u64 = 0x5eed_c0de_f00d_7a11;
    const CHANGED: u64 = 0x7a11_f00d_c0de_5eed;

    /// Run in a re-executed test binary by [`Holder::spawn`]: holds `HELD` on the
    /// heap, prints its address, and stores `CHANGED` in it on a line of stdin.
    #[test]
    fn hold_value_child() {
        if std::env::var_os(HOLD_ENV).is_none() {
            return;
        }

        let mut value = Box::new(std::hint::black_box(HELD));
        let address = &*value as *const u64 as usize;
        println!("held {address:x}");
        std::io::stdout().flush().unwrap();

        let mut line = String::new();
        std::io::stdin().read_line(&mut line).unwrap();
        // SAFETY: the box is live and the write must not be optimized away.
        unsafe { std::ptr::write_volatile(&mut *value, CHANGED) };
        println!("changed");
        std::io::stdout().flush().unwrap();

        std::thread::sleep(Duration::from_secs(60));
        std::hint::black_box(value);
    }

    struct Holder {
        child: Child,
        stdin: ChildStdin,
        stdout: BufReader<std::process::ChildStdout>,
        address: u64,
    }

    impl Holder {
        fn spawn() -> Self {
            let mut child = Command::new(std::env::current_exe().unwrap())
                .args([
                    "--exact",
                    "linux::hold_value_child",
                    "--nocapture",
                    "--test-threads=1",
                ])
                .env(HOLD_ENV, "1")
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
                .expect("spawn holder");

            let stdin = child.stdin.take().unwrap();
            let mut stdout = BufReader::new(child.stdout.take().unwrap());
            let address = read_until(&mut stdout, "held ");
            let address = u64::from_str_radix(&address, 16).unwrap();

            Self {
                child,
                stdin,
                stdout,
                address,
            }
        }

        fn change(&mut self) {
            writeln!(self.stdin).unwrap();
            read_until(&mut self.stdout, "changed");
        }
    }

    impl Drop for Holder {
        fn drop(&mut self) {
            _ = self.child.kill();
            _ = self.child.wait();
        }
    }

    fn read_until(stdout: &mut impl std::io::BufRead, marker: &str) -> String {
        for line in stdout.lines() {
            let line = line.unwrap();

            if let Some((_, rest)) = line.split_once(marker) {
                return rest.to_owned();
            }
        }

        panic!("holder exited before printing `{marker}`");
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn scan_rescan_undo_redo() {
        let mut holder = Holder::spawn();
        let pid = holder.child.id();

        let handle = connect(server().await).await;
        let mut changes = handle.on_broadcast::<api::ScanStateChanged>();

        let response: api::AttachResponse =
            timeout!(handle.request().body(api::AttachRequest { pid }).send())
                .unwrap()
                .decode()
                .unwrap();
        assert_eq!(response.error, None);

        let response = scan(&handle, "value ==").await;
        assert!(response.filter_error.is_some(), "{response:?}");

        let response = scan(&handle, &format!("value == {HELD}u64")).await;
        assert_eq!(response, api::ScanResponse::default());

        let state = finished(&mut changes, 1).await;
        let outcome = state.last.clone().unwrap();
        assert_eq!(outcome.error, None);
        assert!(!outcome.cancelled);
        assert_eq!(state.generations, 1);
        assert!(state.results > 0);
        assert_eq!(outcome.results, state.results);

        let page = results(&handle).await;
        assert_eq!(page.error, None);
        let held = page
            .results
            .iter()
            .find(|r| r.address == holder.address)
            .expect("held value in results");
        assert_eq!(held.value_type, "u64");
        assert_eq!(held.last, HELD.to_string());
        assert_eq!(held.current, HELD.to_string());

        holder.change();

        let page = results(&handle).await;
        let held = page
            .results
            .iter()
            .find(|r| r.address == holder.address)
            .unwrap();
        assert_eq!(held.last, HELD.to_string());
        assert_eq!(held.current, CHANGED.to_string());

        let response = scan(&handle, &format!("value == {CHANGED}u64")).await;
        assert_eq!(response, api::ScanResponse::default());

        let state = finished(&mut changes, 2).await;
        assert_eq!(state.last.as_ref().unwrap().error, None);
        assert_eq!(state.generations, 2);
        assert!(state.results >= 1 && state.results < 1000);

        let page = results(&handle).await;
        let held = page
            .results
            .iter()
            .find(|r| r.address == holder.address)
            .expect("changed value survives rescan");
        assert_eq!(held.last, CHANGED.to_string());

        let undone: api::ScanState = timeout!(handle.request().body(api::UndoScanRequest).send())
            .unwrap()
            .decode()
            .unwrap();
        assert_eq!((undone.generations, undone.undone), (1, 1));
        assert_eq!(undone.results, outcome.results);

        let redone: api::ScanState = timeout!(handle.request().body(api::RedoScanRequest).send())
            .unwrap()
            .decode()
            .unwrap();
        assert_eq!((redone.generations, redone.undone), (2, 0));

        let reset: api::ScanState = timeout!(handle.request().body(api::ResetScanRequest).send())
            .unwrap()
            .decode()
            .unwrap();
        assert_eq!(reset, api::ScanState::default());
    }
}
