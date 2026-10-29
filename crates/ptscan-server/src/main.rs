use std::net::SocketAddr;
use std::sync::Arc;

use anyhow::{Context as _, Result};
use clap::Parser;
use tokio::net::TcpListener;
use tracing_subscriber::EnvFilter;

#[derive(Parser)]
#[command(about = "Serve the ptscan websocket API")]
struct Args {
    /// Address to listen on.
    #[arg(long, default_value = api::DEFAULT_BIND)]
    bind: SocketAddr,
    /// Also accept connections from pages served at this origin, such as
    /// `http://127.0.0.1:8080` for `trunk serve`. Can be repeated.
    #[arg(long, value_name = "ORIGIN")]
    dev_origin: Vec<ptscan_server::DevOrigin>,
}

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .init();

    let args = Args::parse();

    let listener = TcpListener::bind(args.bind)
        .await
        .with_context(|| format!("binding {}", args.bind))?;

    tracing::info!("Listening on ws://{}/ws", listener.local_addr()?);
    let shared = args
        .dev_origin
        .into_iter()
        .fold(ptscan_server::Shared::default(), |shared, origin| {
            shared.with_dev_origin(origin)
        });

    ptscan_server::serve(listener, Arc::new(shared)).await
}
