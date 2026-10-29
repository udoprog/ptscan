//! Refuses requests a browser makes on behalf of another site.
//!
//! A browser always sends `Origin` on websocket upgrades, so a page on another
//! site (or one using DNS rebinding to reach this port) is refused, while
//! native clients which send no `Origin` are allowed.

use std::net::{IpAddr, SocketAddr};

use axum::extract::connect_info::Connected;
use axum::extract::{ConnectInfo, Request};
use axum::http::{HeaderMap, StatusCode, Uri, header};
use axum::middleware::Next;
use axum::response::Response;
use axum::serve::IncomingStream;
use tokio::net::TcpListener;

#[derive(Clone)]
pub struct Connection {
    local: Option<SocketAddr>,
}

impl Connected<IncomingStream<'_, TcpListener>> for Connection {
    fn connect_info(stream: IncomingStream<'_, TcpListener>) -> Self {
        Self {
            local: stream.io().local_addr().ok(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Authority {
    host: String,
    port: u16,
}

impl Authority {
    fn parse(value: &str) -> Option<Self> {
        let authority = value.parse::<axum::http::uri::Authority>().ok()?;

        if authority.as_str().contains('@') {
            return None;
        }

        let host = authority.host();
        let bare = host.trim_start_matches('[').trim_end_matches(']');

        let bare = match bare.parse::<IpAddr>() {
            Ok(ip) => ip.to_canonical().to_string(),
            Err(_) => bare.to_ascii_lowercase(),
        };

        let port = match authority.as_str().strip_prefix(host)? {
            "" => 80,
            suffix => suffix.strip_prefix(':')?.parse().ok()?,
        };

        Some(Self { host: bare, port })
    }

    /// Parse a serialized `http` origin, which has no path or trailing slash.
    fn origin(value: &str) -> Option<Self> {
        let uri = value.parse::<Uri>().ok()?;

        if uri.scheme_str() != Some("http")
            || uri.path_and_query().is_some_and(|p| p.as_str() != "/")
            || value.ends_with('/')
        {
            return None;
        }

        Self::parse(uri.authority()?.as_str())
    }

    fn serves(&self, local: SocketAddr) -> bool {
        let ip = local.ip().to_canonical();
        self.port == local.port()
            && (self.host == ip.to_string() || (self.host == "localhost" && ip.is_loopback()))
    }
}

/// Allow a request whose `Host` names the socket it arrived on, and whose
/// `Origin`, when present, is that same host.
fn allows(headers: &HeaderMap, local: Option<SocketAddr>) -> bool {
    let Some(local) = local else {
        return false;
    };

    let Some(host) = single(headers, header::HOST).and_then(Authority::parse) else {
        return false;
    };

    if !host.serves(local) {
        return false;
    }

    if !headers.contains_key(header::ORIGIN) {
        return true;
    }

    single(headers, header::ORIGIN).and_then(Authority::origin) == Some(host)
}

fn single(headers: &HeaderMap, name: header::HeaderName) -> Option<&str> {
    let mut values = headers.get_all(name).iter();
    let value = values.next()?.to_str().ok()?;
    values.next().is_none().then_some(value)
}

pub async fn check(
    ConnectInfo(connection): ConnectInfo<Connection>,
    request: Request,
    next: Next,
) -> Result<Response, StatusCode> {
    if !allows(request.headers(), connection.local) {
        return Err(StatusCode::FORBIDDEN);
    }

    Ok(next.run(request).await)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn headers(host: &str, origin: Option<&str>) -> HeaderMap {
        let mut headers = HeaderMap::new();
        headers.insert(header::HOST, host.parse().unwrap());

        if let Some(origin) = origin {
            headers.insert(header::ORIGIN, origin.parse().unwrap());
        }

        headers
    }

    fn local(addr: &str) -> Option<SocketAddr> {
        Some(addr.parse().unwrap())
    }

    #[test]
    fn origin_and_host_must_match_the_socket() {
        let local = local("127.0.0.1:7431");

        assert!(allows(
            &headers("localhost:7431", Some("http://localhost:7431")),
            local
        ));
        assert!(allows(
            &headers("127.0.0.1:7431", Some("http://127.0.0.1:7431")),
            local
        ));
        assert!(allows(&headers("127.0.0.1:7431", None), local));

        for origin in [
            "http://evil.example",
            "null",
            "https://localhost:7431",
            "http://localhost:7432",
            "http://127.0.0.1:7431/path",
            "http://localhost:7431/",
            "http://localhost:7431?x",
            "http://user@localhost:7431",
            "http://127.0.0.1:7431",
        ] {
            assert!(
                !allows(&headers("localhost:7431", Some(origin)), local),
                "{origin}"
            );
        }

        for host in [
            "evil.example:7431",
            "localhost:7432",
            "localhost",
            "user@localhost:7431",
            "localhost:99999",
        ] {
            assert!(!allows(&headers(host, None), local), "{host}");
        }

        let rebound = headers("evil.example:7431", Some("http://evil.example:7431"));
        assert!(!allows(&rebound, local));

        let mut duplicate = headers("localhost:7431", Some("http://localhost:7431"));
        duplicate.append(header::ORIGIN, "http://evil.example".parse().unwrap());
        assert!(!allows(&duplicate, local));

        assert!(!allows(&HeaderMap::new(), local));
        assert!(!allows(&headers("localhost:7431", None), None));
    }

    #[test]
    fn other_interfaces_use_the_accepted_socket() {
        for (local_addr, host, origin) in [
            (
                "192.168.1.20:7431",
                "192.168.1.20:7431",
                "http://192.168.1.20:7431",
            ),
            ("[::1]:7431", "[::1]:7431", "http://[::1]:7431"),
            (
                "[::ffff:127.0.0.1]:7431",
                "127.0.0.1:7431",
                "http://127.0.0.1:7431",
            ),
            ("127.0.0.1:80", "localhost", "http://localhost"),
        ] {
            assert!(allows(&headers(host, Some(origin)), local(local_addr)));
        }

        assert!(!allows(
            &headers("localhost:7431", None),
            local("192.168.1.20:7431")
        ));
    }
}
