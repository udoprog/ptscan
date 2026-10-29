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
    ptscan_server::serve(listener, Arc::new(ptscan_server::Shared::default())).await
}
