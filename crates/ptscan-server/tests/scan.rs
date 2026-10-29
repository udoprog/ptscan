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
    server_with(ptscan_server::Shared::default()).await
}

async fn server_with(shared: ptscan_server::Shared) -> SocketAddr {
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(ptscan_server::serve(listener, Arc::new(shared)));
    addr
}

async fn connect(addr: SocketAddr) -> Handle {
    let mut service = tungstenite029::connect(format!("ws://{addr}/ws")).build();
    let handle = service.handle().clone();
    tokio::spawn(async move { service.run().await });
    timeout!(handle.wait_until_open()).expect("open connection");
    handle
}

fn request(filter: &str) -> api::ScanRequest {
    api::ScanRequest {
        filter: filter.into(),
        value: String::new(),
        alignment: Some(8),
        suspend: false,
        modules_only: false,
    }
}

async fn scan(handle: &Handle, filter: &str) -> api::ScanResponse {
    scan_with(handle, request(filter)).await
}

async fn scan_with(handle: &Handle, request: api::ScanRequest) -> api::ScanResponse {
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

    let request = api::WriteScanValueRequest {
        address: 0x1000,
        value_type: "u32".into(),
        value: "1".into(),
    };
    let response: api::WriteScanValueResponse = timeout!(handle.request().body(request).send())
        .unwrap()
        .decode()
        .unwrap();
    assert!(response.error.is_some(), "{response:?}");

    let request = api::RemoveScanResultsRequest {
        addresses: vec![0x1000],
    };
    let response: api::RemoveScanResultsResponse = timeout!(handle.request().body(request).send())
        .unwrap()
        .decode()
        .unwrap();
    assert_eq!(response, api::RemoveScanResultsResponse::default());
}

async fn process_information(
    handle: &Handle,
    filter: api::MemoryFilter,
) -> api::ProcessInformation {
    timeout!(
        handle
            .request()
            .body(api::ProcessInformationRequest { filter })
            .send()
    )
    .unwrap()
    .decode()
    .unwrap()
}

#[tokio::test]
async fn process_information_needs_attached_process() {
    let handle = connect(server().await).await;
    let info = process_information(&handle, api::MemoryFilter::default()).await;
    assert!(info.error.is_some());
    assert!(info.regions.is_empty());
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
    use std::sync::atomic::{AtomicUsize, Ordering};

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
    /// Points at the held value from the image's writable data, for pointer
    /// scans to find; nonzero so that it is not in `.bss`.
    static HELD_POINTER: AtomicUsize = AtomicUsize::new(1);

    /// Run in a re-executed test binary by [`Holder::spawn`]: holds `HELD` on the
    /// heap, points `HELD_POINTER` at it and prints its address, then on each line of stdin either stores
    /// `CHANGED` in it (`change`) or prints the value it now holds (`read`).
    #[test]
    fn hold_value_child() {
        if std::env::var_os(HOLD_ENV).is_none() {
            return;
        }

        let mut value = Box::new(std::hint::black_box(HELD));
        let address = &*value as *const u64 as usize;
        HELD_POINTER.store(address, Ordering::SeqCst);
        println!("held {address:x}");
        std::io::stdout().flush().unwrap();

        for line in std::io::stdin().lines() {
            if line.unwrap() == "read" {
                // SAFETY: the box is live; the parent writes it behind our back.
                let now = unsafe { std::ptr::read_volatile(&*value) };
                println!("now {now:x}");
            } else {
                // SAFETY: the box is live and the write must not be optimized away.
                unsafe { std::ptr::write_volatile(&mut *value, CHANGED) };
                println!("changed");
            }

            std::io::stdout().flush().unwrap();
        }

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

        fn state(&self) -> char {
            let stat = std::fs::read_to_string(format!("/proc/{}/stat", self.child.id())).unwrap();
            let (_, rest) = stat.rsplit_once(')').unwrap();
            rest.trim_start().chars().next().unwrap()
        }

        fn change(&mut self) {
            writeln!(self.stdin, "change").unwrap();
            read_until(&mut self.stdout, "changed");
        }

        fn now(&mut self) -> u64 {
            writeln!(self.stdin, "read").unwrap();
            let now = read_until(&mut self.stdout, "now ");
            u64::from_str_radix(&now, 16).unwrap()
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

    async fn attach(handle: &Handle, pid: u32) {
        let response: api::AttachResponse =
            timeout!(handle.request().body(api::AttachRequest { pid }).send())
                .unwrap()
                .decode()
                .unwrap();
        assert_eq!(response.error, None);
    }

    async fn find_held(handle: &Handle, holder: &Holder) -> Option<api::ScanResult> {
        let page = results(handle).await;
        assert_eq!(page.error, None);
        page.results
            .into_iter()
            .find(|r| r.address == holder.address)
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn suspend_and_modules_only() {
        let mut holder = Holder::spawn();
        let handle = connect(server().await).await;
        let mut changes = handle.on_broadcast::<api::ScanStateChanged>();
        attach(&handle, holder.child.id()).await;

        let response = scan_with(
            &handle,
            api::ScanRequest {
                modules_only: true,
                ..request(&format!("value == {HELD}u64"))
            },
        )
        .await;
        assert_eq!(response, api::ScanResponse::default());
        let state = finished(&mut changes, 1).await;
        assert_eq!(state.last.unwrap().error, None);
        assert!(
            find_held(&handle, &holder).await.is_none(),
            "heap value is outside modules"
        );

        let reset: api::ScanState = timeout!(handle.request().body(api::ResetScanRequest).send())
            .unwrap()
            .decode()
            .unwrap();
        assert_eq!(reset, api::ScanState::default());

        let response = scan_with(
            &handle,
            api::ScanRequest {
                suspend: true,
                ..request(&format!("value == {HELD}u64"))
            },
        )
        .await;
        assert_eq!(response, api::ScanResponse::default());
        let state = finished(&mut changes, 1).await;
        assert_eq!(state.last.unwrap().error, None);
        assert!(find_held(&handle, &holder).await.is_some());

        // The process is resumed after the scan.
        assert_ne!(holder.state(), 'T');
        holder.change();
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn large_generation_is_rescanned_in_place() {
        let mut holder = Holder::spawn();
        let shared = ptscan_server::Shared::default().with_rescan_copy_limit(0);
        let handle = connect(server_with(shared).await).await;
        let mut changes = handle.on_broadcast::<api::ScanStateChanged>();
        attach(&handle, holder.child.id()).await;

        scan(&handle, &format!("value == {HELD}u64")).await;
        let state = finished(&mut changes, 1).await;
        assert!(!state.last.unwrap().in_place);

        holder.change();
        scan(&handle, &format!("value == {CHANGED}u64")).await;
        let state = finished(&mut changes, 1).await;
        let outcome = state.last.clone().unwrap();
        assert_eq!(outcome.error, None);
        assert!(outcome.in_place);
        assert_eq!((state.generations, state.undone), (1, 0));
        assert_eq!(outcome.results, state.results);

        let held = find_held(&handle, &holder)
            .await
            .expect("changed value survives rescan");
        assert_eq!(held.last, CHANGED.to_string());
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn cancelled_scan_keeps_partial_results() {
        let holder = Holder::spawn();
        let handle = connect(server().await).await;
        let mut changes = handle.on_broadcast::<api::ScanStateChanged>();
        attach(&handle, holder.child.id()).await;

        // Matches nearly everything, so the scan is still running when the
        // cancellation arrives.
        let response = scan(&handle, "value != 1u8").await;
        assert_eq!(response, api::ScanResponse::default());
        let _: api::ScanState = timeout!(handle.request().body(api::CancelScanRequest).send())
            .unwrap()
            .decode()
            .unwrap();

        let state = finished(&mut changes, 1).await;
        let outcome = state.last.clone().unwrap();
        assert_eq!(outcome.error, None);
        assert!(outcome.cancelled, "{outcome:?}");
        assert_eq!(outcome.results, state.results);
        assert_eq!(results(&handle).await.total, state.results);
    }

    async fn edit(handle: &Handle, request: api::EditScratchRequest) -> api::ScratchResponse {
        timeout!(handle.request().body(request).send())
            .unwrap()
            .decode()
            .unwrap()
    }

    async fn write_value(handle: &Handle, id: u64, value: &str) -> api::ScratchResponse {
        let request = api::WriteScratchValueRequest {
            id,
            value: value.into(),
        };

        timeout!(handle.request().body(request).send())
            .unwrap()
            .decode()
            .unwrap()
    }

    async fn scratch(handle: &Handle) -> api::ScratchList {
        timeout!(handle.request().body(api::GetScratchRequest).send())
            .unwrap()
            .decode()
            .unwrap()
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn scratch_list() {
        let mut holder = Holder::spawn();
        let handle = connect(server().await).await;
        let mut changes = handle.on_broadcast::<api::ScanStateChanged>();
        let mut scratch_changes = handle.on_broadcast::<api::ScratchChanged>();
        attach(&handle, holder.child.id()).await;

        scan(&handle, &format!("value == {HELD}u64")).await;
        finished(&mut changes, 1).await;
        let held = find_held(&handle, &holder).await.unwrap();

        let response: api::ScratchResponse = timeout!(
            handle
                .request()
                .body(api::AddScratchRequest {
                    pointer: "0x10 +".into(),
                    value_type: "nope".into(),
                })
                .send()
        )
        .unwrap()
        .decode()
        .unwrap();
        assert!(response.pointer_error.is_some(), "{response:?}");
        assert!(response.value_type_error.is_some(), "{response:?}");
        assert_eq!(response.entry, None);

        let response: api::ScratchResponse = timeout!(
            handle
                .request()
                .body(api::AddScratchRequest {
                    pointer: held.pointer.clone(),
                    value_type: held.value_type.clone(),
                })
                .send()
        )
        .unwrap()
        .decode()
        .unwrap();
        let entry = response.entry.clone().expect("added entry");
        assert_eq!(response.error, None);
        assert_eq!(entry.address, Some(holder.address));
        assert_eq!(entry.value_type, "u64");
        assert_eq!(entry.value, Some(HELD.to_string()));

        let event = timeout!(scratch_changes.recv()).unwrap().unwrap();
        let list = event.decode_event().unwrap();
        assert_eq!(list.entries, std::slice::from_ref(&entry));

        holder.change();
        let list = scratch(&handle).await;
        assert_eq!(list.entries[0].value, Some(CHANGED.to_string()));

        let response = write_value(&handle, entry.id, "nope").await;
        assert!(response.value_error.is_some(), "{response:?}");
        assert_eq!(holder.now(), CHANGED);

        let response = write_value(&handle, entry.id, "42").await;
        assert_eq!(response.error, None);
        assert_eq!(response.entry.unwrap().value, Some("42".into()));
        assert_eq!(holder.now(), 42);

        let response = edit(
            &handle,
            api::EditScratchRequest {
                id: entry.id,
                pointer: Some("0x10 +".into()),
                value_type: None,
            },
        )
        .await;
        assert!(response.pointer_error.is_some(), "{response:?}");

        write_value(&handle, entry.id, &CHANGED.to_string()).await;
        let response = edit(
            &handle,
            api::EditScratchRequest {
                id: entry.id,
                pointer: None,
                value_type: Some("i32".into()),
            },
        )
        .await;
        let edited = response.entry.unwrap();
        assert_eq!(edited.value_type, "i32");
        assert_eq!(edited.value, Some((CHANGED as u32 as i32).to_string()));

        // Only the four bytes of the i32 are written.
        let response = write_value(&handle, entry.id, "-1").await;
        assert_eq!(response.error, None);
        assert_eq!(holder.now(), CHANGED | 0xffff_ffff);

        let response = write_value(&handle, entry.id + 1, "1").await;
        assert!(response.error.is_some());

        let list: api::ScratchList = timeout!(
            handle
                .request()
                .body(api::RemoveScratchRequest {
                    ids: vec![entry.id],
                })
                .send()
        )
        .unwrap()
        .decode()
        .unwrap();
        assert!(list.entries.is_empty());
        assert!(scratch(&handle).await.entries.is_empty());
    }

    async fn write_scan_value(
        handle: &Handle,
        address: u64,
        value_type: &str,
        value: &str,
    ) -> api::WriteScanValueResponse {
        let request = api::WriteScanValueRequest {
            address,
            value_type: value_type.into(),
            value: value.into(),
        };

        timeout!(handle.request().body(request).send())
            .unwrap()
            .decode()
            .unwrap()
    }

    async fn remove_results(
        handle: &Handle,
        addresses: Vec<u64>,
    ) -> api::RemoveScanResultsResponse {
        timeout!(
            handle
                .request()
                .body(api::RemoveScanResultsRequest { addresses })
                .send()
        )
        .unwrap()
        .decode()
        .unwrap()
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn write_and_remove_results() {
        let mut holder = Holder::spawn();
        let handle = connect(server().await).await;
        let mut changes = handle.on_broadcast::<api::ScanStateChanged>();
        attach(&handle, holder.child.id()).await;

        scan(&handle, &format!("value == {HELD}u64")).await;
        let first = finished(&mut changes, 1).await;
        let held = find_held(&handle, &holder).await.unwrap();

        let response = write_scan_value(&handle, held.address, "nope", "1").await;
        assert!(response.value_type_error.is_some(), "{response:?}");
        let response = write_scan_value(&handle, held.address, "u64", "nope").await;
        assert!(response.value_error.is_some(), "{response:?}");
        assert_eq!(holder.now(), HELD);

        let response = write_scan_value(&handle, held.address, "u64", &CHANGED.to_string()).await;
        assert_eq!(response.error, None);
        assert_eq!(response.current, Some(CHANGED.to_string()));
        assert_eq!(holder.now(), CHANGED);
        let held = find_held(&handle, &holder).await.unwrap();
        assert_eq!(held.current, CHANGED.to_string());

        scan(&handle, &format!("value == {CHANGED}u64")).await;
        finished(&mut changes, 2).await;
        let undone: api::ScanState = timeout!(handle.request().body(api::UndoScanRequest).send())
            .unwrap()
            .decode()
            .unwrap();
        assert_eq!((undone.generations, undone.undone), (1, 1));

        let response = remove_results(&handle, vec![held.address, 0]).await;
        assert_eq!(response.error, None);
        assert_eq!(response.removed, 1);
        assert_eq!(response.state.generations, 1);
        assert_eq!(response.state.undone, 0, "removal discards the redo stack");
        assert_eq!(response.state.results, first.results - 1);
        assert!(find_held(&handle, &holder).await.is_none());

        // The undo's broadcast may still be queued before the removal's.
        loop {
            let event = timeout!(changes.recv()).unwrap().unwrap();

            if event.decode_event().unwrap() == response.state {
                break;
            }
        }

        let response = remove_results(&handle, vec![held.address]).await;
        assert_eq!(response.removed, 0);
        assert_eq!(response.state.results, first.results - 1);
    }

    async fn add_scratch(handle: &Handle, pointer: &str) -> api::ScratchEntry {
        let request = api::AddScratchRequest {
            pointer: pointer.into(),
            value_type: "u64".into(),
        };

        let response: api::ScratchResponse = timeout!(handle.request().body(request).send())
            .unwrap()
            .decode()
            .unwrap();
        response.entry.expect("added entry")
    }

    async fn pointer_scan(handle: &Handle, id: u64) -> api::PointerScanResponse {
        timeout!(handle.request().body(api::PointerScanRequest { id }).send())
            .unwrap()
            .decode()
            .unwrap()
    }

    async fn pointer_finished(
        changes: &mut musli_web::client::Listener<api::PointerScanStateChanged>,
    ) -> api::PointerScanState {
        loop {
            let event = timeout!(changes.recv()).unwrap().unwrap();
            let state = event.decode_event().unwrap();

            if !state.running && state.last.is_some() {
                return state;
            }
        }
    }

    async fn pointer_results(
        handle: &Handle,
        offset: u64,
        limit: u32,
    ) -> api::PointerScanResultsResponse {
        let request = api::PointerScanResultsRequest { offset, limit };

        timeout!(handle.request().body(request).send())
            .unwrap()
            .decode()
            .unwrap()
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn pointer_scan_finds_static_path() {
        let holder = Holder::spawn();
        let handle = connect(server().await).await;
        let mut changes = handle.on_broadcast::<api::PointerScanStateChanged>();
        let mut progress = handle.on_broadcast::<api::PointerScanProgressed>();

        let response = pointer_scan(&handle, 0).await;
        assert!(response.error.is_some(), "{response:?}");

        attach(&handle, holder.child.id()).await;
        let response = pointer_scan(&handle, 0).await;
        assert!(response.error.is_some(), "no such entry: {response:?}");

        let entry = add_scratch(&handle, &format!("{:#x}", holder.address)).await;
        assert_eq!(entry.value, Some(HELD.to_string()));

        let response = pointer_scan(&handle, entry.id).await;
        assert_eq!(response, api::PointerScanResponse::default());

        let state = pointer_finished(&mut changes).await;
        let outcome = state.last.clone().unwrap();
        assert_eq!(outcome.error, None);
        assert!(!outcome.cancelled);
        assert_eq!(state.entry, Some(entry.id));
        assert_eq!(state.value_type.as_deref(), Some("u64"));
        assert!(state.results > 0);
        assert_eq!(outcome.results, state.results);
        timeout!(progress.recv()).unwrap().unwrap();

        let mut found = Vec::new();
        let mut offset = 0;

        while offset < state.results {
            let page = pointer_results(&handle, offset, api::MAX_SCAN_PAGE).await;
            assert_eq!(page.total, state.results);
            assert_eq!(page.offset, offset);
            assert_eq!(page.value_type.as_deref(), Some("u64"));
            offset += page.results.len() as u64;
            found.extend(page.results);
        }

        let page = pointer_results(&handle, state.results + 5, 10).await;
        assert!(page.results.is_empty());

        // The path through `HELD_POINTER` is the only one with a single
        // dereference and no offset.
        let path = found
            .iter()
            .find(|p| p.matches(" -> ").count() == 1 && p.ends_with(" -> 0x0"))
            .unwrap_or_else(|| panic!("no path through the static in {} paths", found.len()));
        let added = add_scratch(&handle, path).await;
        assert_eq!(added.address, Some(holder.address), "{path}");
        assert_eq!(added.value, Some(HELD.to_string()));
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn pointer_scan_cancel_and_detach() {
        let holder = Holder::spawn();
        let handle = connect(server().await).await;
        let mut changes = handle.on_broadcast::<api::PointerScanStateChanged>();
        attach(&handle, holder.child.id()).await;
        let entry = add_scratch(&handle, &format!("{:#x}", holder.address)).await;

        assert_eq!(
            pointer_scan(&handle, entry.id).await,
            api::PointerScanResponse::default()
        );
        let cancelled: api::PointerScanState =
            timeout!(handle.request().body(api::CancelPointerScanRequest).send())
                .unwrap()
                .decode()
                .unwrap();
        assert_eq!(cancelled.entry, Some(entry.id));

        let state = pointer_finished(&mut changes).await;
        let outcome = state.last.unwrap();
        assert_eq!(outcome.error, None);
        assert_eq!(outcome.results, state.results);

        timeout!(handle.request().body(api::DetachRequest).send()).unwrap();
        let state: api::PointerScanState = timeout!(
            handle
                .request()
                .body(api::GetPointerScanStateRequest)
                .send()
        )
        .unwrap()
        .decode()
        .unwrap();
        assert_eq!(state, api::PointerScanState::default());
        assert_eq!(pointer_results(&handle, 0, 10).await.total, 0);
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn process_information_of_child() {
        let holder = Holder::spawn();
        let pid = holder.child.id();
        let handle = connect(server().await).await;
        attach(&handle, pid).await;

        let all = process_information(&handle, api::MemoryFilter::default()).await;
        assert_eq!(all.error, None);

        let exe = std::env::current_exe().unwrap();
        let exe = exe.file_name().unwrap().to_str().unwrap();
        assert_eq!(all.name.as_deref(), Some(exe));
        let module = all
            .modules
            .iter()
            .find(|m| m.name == exe)
            .expect("executable module");
        assert!(module.size > 0);

        let main = all
            .threads
            .iter()
            .find(|t| t.id == pid)
            .expect("main thread");
        assert!(main.stack_size > 0);

        let contains =
            |r: &api::MemoryRegion, address: u64| r.base <= address && address < r.base + r.size;

        let held = all
            .regions
            .iter()
            .find(|r| contains(r, holder.address))
            .expect("region holding the value");
        assert!(held.readable && held.writable);
        assert_eq!(held.module, None);
        assert!(
            all.regions
                .iter()
                .any(|r| r.module.as_deref() == Some(exe) && contains(r, module.base))
        );
        assert!(
            all.regions
                .iter()
                .any(|r| r.thread_stack == Some(pid) && contains(r, main.stack_base))
        );

        let filter = api::MemoryFilter {
            readable: true,
            writable: true,
            ..Default::default()
        };
        let writable = process_information(&handle, filter).await;
        assert_eq!(writable.error, None);
        assert!(writable.regions.iter().all(|r| r.readable && r.writable));
        assert!(writable.regions.contains(held));
        assert!(writable.regions.len() < all.regions.len());

        let filter = api::MemoryFilter {
            modules: true,
            executable: true,
            ..Default::default()
        };
        let code = process_information(&handle, filter).await;
        assert!(!code.regions.is_empty());
        assert!(
            code.regions
                .iter()
                .all(|r| r.module.is_some() && r.executable)
        );

        let filter = api::MemoryFilter {
            thread_stacks: true,
            ..Default::default()
        };
        let stacks = process_information(&handle, filter).await;
        assert!(stacks.regions.iter().any(|r| r.thread_stack == Some(pid)));
        assert!(stacks.regions.iter().all(|r| r.thread_stack.is_some()));

        let filter = api::MemoryFilter {
            other: true,
            readable: true,
            ..Default::default()
        };
        let other = process_information(&handle, filter).await;
        assert!(
            other
                .regions
                .iter()
                .all(|r| { !r.readable && !r.writable && !r.executable && r.module.is_none() })
        );
    }
}
