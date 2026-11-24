//! Serves the trunk-built web UI embedded into the binary.
//!
//! The UI is built into `target/dist` with `trunk build --release` before the
//! server is built. Without it the server still builds, but serves a page
//! saying the UI is missing and warns on startup.

use axum::http::{StatusCode, Uri, header};
use axum::response::{Html, IntoResponse, Response};
use rust_embed::RustEmbed;

#[derive(RustEmbed)]
#[folder = "../../target/dist"]
#[allow_missing = true]
struct Asset;

const MISSING: &str = "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>ptscan</title></head>\
<body><p>The ptscan web UI was not built into this server. Run <code>trunk build --release</code> \
in the repository root, then rebuild ptscan (<code>cargo build -p ptscan-server</code>).</p></body></html>";

/// Test if the web UI is embedded.
pub fn is_embedded() -> bool {
    Asset::get("index.html").is_some()
}

/// Names of the embedded files.
pub fn files() -> impl Iterator<Item = String> {
    Asset::iter().map(|name| name.into_owned())
}

/// Serve the asset at `uri`, or `index.html` for paths without a file.
pub(crate) async fn handler(uri: Uri) -> Response {
    let path = uri.path().trim_start_matches('/');

    if !path.is_empty()
        && let Some(content) = Asset::get(path)
    {
        let mime = mime_guess::from_path(path).first_or_octet_stream();
        return ([(header::CONTENT_TYPE, mime.as_ref())], content.data).into_response();
    }

    match Asset::get("index.html") {
        Some(content) => Html(content.data).into_response(),
        None => (StatusCode::NOT_FOUND, Html(MISSING)).into_response(),
    }
}
