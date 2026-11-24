use std::sync::Arc;

use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::{TcpListener, TcpStream};

async fn get(addr: std::net::SocketAddr, path: &str) -> (String, String) {
    let mut stream = TcpStream::connect(addr).await.unwrap();
    let request = format!("GET {path} HTTP/1.1\r\nHost: {addr}\r\nConnection: close\r\n\r\n");
    stream.write_all(request.as_bytes()).await.unwrap();

    let mut response = Vec::new();
    stream.read_to_end(&mut response).await.unwrap();
    let response = String::from_utf8_lossy(&response);
    let (head, body) = response.split_once("\r\n\r\n").unwrap();
    (head.to_ascii_lowercase(), body.to_owned())
}

#[tokio::test]
async fn serves_web_ui() {
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    let shared = Arc::new(ptscan::Shared::default());
    tokio::spawn(ptscan::serve(listener, shared));

    let (head, body) = get(addr, "/").await;
    assert!(head.contains("content-type: text/html"), "{head}");

    if !ptscan::web::is_embedded() {
        assert!(head.starts_with("http/1.1 404"), "{head}");
        assert!(body.contains("trunk build --release"), "{body}");
        return;
    }

    assert!(head.starts_with("http/1.1 200"), "{head}");
    assert!(body.contains("<title>ptscan</title>"), "{body}");

    let js = ptscan::web::files()
        .find(|name| name.ends_with(".js"))
        .expect("embedded script");
    let (head, body) = get(addr, &format!("/{js}")).await;
    assert!(head.starts_with("http/1.1 200"), "{head}");
    assert!(head.contains("content-type: text/javascript"), "{head}");
    assert!(!body.is_empty());

    let (head, _) = get(addr, "/client/route").await;
    assert!(head.starts_with("http/1.1 200"), "{head}");
}
