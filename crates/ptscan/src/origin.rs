//! Refuses requests a browser makes on behalf of another site.
//!
//! A browser always sends `Origin` on websocket upgrades, so a page on another
//! site (or one using DNS rebinding to reach this port) is refused, while
//! native clients which send no `Origin` are allowed. Origins added with
//! [`crate::Shared::with_dev_origin`], such as a `trug serve` page proxying
//! `/ws`, are also allowed.

use std::fmt;
use std::net::{IpAddr, SocketAddr};
use std::str::FromStr;
use std::sync::Arc;

use axum::extract::connect_info::Connected;
use axum::extract::{ConnectInfo, Request, State};
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

/// An additional `http` origin allowed to connect.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DevOrigin(Authority);

impl FromStr for DevOrigin {
    type Err = InvalidOrigin;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Authority::origin(value).map(Self).ok_or(InvalidOrigin)
    }
}

#[derive(Debug)]
pub struct InvalidOrigin;

impl fmt::Display for InvalidOrigin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("expected an origin such as http://127.0.0.1:8080")
    }
}

impl std::error::Error for InvalidOrigin {}

/// Allow a request whose `Host` names the socket it arrived on, and whose
/// `Origin`, when present, is that same host or one of `dev`.
fn allows(headers: &HeaderMap, local: Option<SocketAddr>, dev: &[DevOrigin]) -> bool {
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

    let Some(origin) = single(headers, header::ORIGIN).and_then(Authority::origin) else {
        return false;
    };

    origin == host || dev.iter().any(|dev| dev.0 == origin)
}

fn single(headers: &HeaderMap, name: header::HeaderName) -> Option<&str> {
    let mut values = headers.get_all(name).iter();
    let value = values.next()?.to_str().ok()?;
    values.next().is_none().then_some(value)
}

pub async fn check(
    State(shared): State<Arc<crate::Shared>>,
    ConnectInfo(connection): ConnectInfo<Connection>,
    request: Request,
    next: Next,
) -> Result<Response, StatusCode> {
    if !allows(request.headers(), connection.local, &shared.dev_origins) {
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
            local,
            &[]
        ));
        assert!(allows(
            &headers("127.0.0.1:7431", Some("http://127.0.0.1:7431")),
            local,
            &[]
        ));
        assert!(allows(&headers("127.0.0.1:7431", None), local, &[]));

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
                !allows(&headers("localhost:7431", Some(origin)), local, &[]),
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
            assert!(!allows(&headers(host, None), local, &[]), "{host}");
        }

        let rebound = headers("evil.example:7431", Some("http://evil.example:7431"));
        assert!(!allows(&rebound, local, &[]));

        let mut duplicate = headers("localhost:7431", Some("http://localhost:7431"));
        duplicate.append(header::ORIGIN, "http://evil.example".parse().unwrap());
        assert!(!allows(&duplicate, local, &[]));

        assert!(!allows(&HeaderMap::new(), local, &[]));
        assert!(!allows(&headers("localhost:7431", None), None, &[]));
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
            assert!(allows(&headers(host, Some(origin)), local(local_addr), &[]));
        }

        assert!(!allows(
            &headers("localhost:7431", None),
            local("192.168.1.20:7431"),
            &[]
        ));
    }

    #[test]
    fn dev_origins_are_allowed_explicitly() {
        let local = local("127.0.0.1:7431");
        let dev = ["http://127.0.0.1:8080".parse::<DevOrigin>().unwrap()];
        let proxied = headers("127.0.0.1:7431", Some("http://127.0.0.1:8080"));

        assert!(!allows(&proxied, local, &[]));
        assert!(allows(&proxied, local, &dev));
        assert!(allows(
            &headers("127.0.0.1:7431", Some("http://127.0.0.1:7431")),
            local,
            &dev
        ));

        for origin in ["http://127.0.0.1:8081", "http://localhost:8080"] {
            assert!(
                !allows(&headers("127.0.0.1:7431", Some(origin)), local, &dev),
                "{origin}"
            );
        }

        let rebound = headers("evil.example:7431", Some("http://127.0.0.1:8080"));
        assert!(!allows(&rebound, local, &dev));

        for invalid in ["127.0.0.1:8080", "https://127.0.0.1:8080", "http://x:1/"] {
            assert!(invalid.parse::<DevOrigin>().is_err(), "{invalid}");
        }
    }
}
