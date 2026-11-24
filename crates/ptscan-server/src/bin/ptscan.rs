use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr};
use std::process;
use std::sync::Arc;
use std::time::Duration;

use anyhow::{Context as _, Result};
use clap::{Parser, Subcommand};
use tokio::net::TcpListener;
use tokio::sync::oneshot;
use tracing_subscriber::EnvFilter;

#[cfg(windows)]
#[path = "../tray.rs"]
mod tray;
#[path = "../update.rs"]
mod update;

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
    /// Do not open the web UI in the default browser on launch.
    #[arg(long)]
    no_browser: bool,
    /// Do not show an icon in the Windows notification area.
    #[arg(long)]
    no_tray: bool,
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Update ptscan to the latest GitHub release.
    Update,
}

fn main() -> Result<()> {
    let args = Args::parse();

    if let Some(Command::Update) = args.command {
        return update::update();
    }

    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .init();

    let runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("building the runtime")?;

    let result = runtime.block_on(run(args));
    // A scan blocking a worker thread would otherwise keep the process alive.
    runtime.shutdown_timeout(Duration::from_secs(5));
    result
}

async fn run(args: Args) -> Result<()> {
    let listener = TcpListener::bind(args.bind)
        .await
        .with_context(|| format!("binding {}", args.bind))?;

    let url = url(listener.local_addr()?);
    tracing::info!("Serving the web UI on {url}");

    if !ptscan_server::web::is_embedded() {
        tracing::warn!(
            "The web UI is not embedded; run `trunk build --release` and rebuild ptscan (`cargo build -p ptscan-server`)"
        );
    }

    if !args.no_browser
        && let Err(error) = open_browser(&url)
    {
        tracing::warn!("Could not open a browser at {url}: {error:#}");
    }

    let shared = Arc::new(
        args.dev_origin
            .into_iter()
            .fold(ptscan_server::Shared::default(), |shared, origin| {
                shared.with_dev_origin(origin)
            }),
    );

    let (quit, quitting) = oneshot::channel::<()>();

    #[cfg(windows)]
    let tray = if args.no_tray {
        drop(quit);
        None
    } else {
        let url = url.clone();
        let open = move || {
            if let Err(error) = open_browser(&url) {
                tracing::warn!("Could not open a browser at {url}: {error:#}");
            }
        };

        match tray::Tray::spawn(shared.subscribe_attach_state(), open, quit) {
            Ok(tray) => Some(tray),
            Err(error) => {
                tracing::warn!("Could not show the tray icon: {error:#}");
                None
            }
        }
    };

    #[cfg(not(windows))]
    {
        _ = args.no_tray;
        drop(quit);
    }

    let shutdown = async move {
        if quitting.await.is_ok() {
            tracing::info!("Quitting");
        } else {
            std::future::pending::<()>().await;
        }
    };

    let result = ptscan_server::serve_until(listener, shared, shutdown).await;

    #[cfg(windows)]
    if let Some(tray) = tray {
        tray.stop();
    }

    result
}

/// The URL a local browser reaches the server at, using loopback for an
/// unspecified bind address.
fn url(mut addr: SocketAddr) -> String {
    match addr.ip() {
        IpAddr::V4(ip) if ip.is_unspecified() => addr.set_ip(Ipv4Addr::LOCALHOST.into()),
        IpAddr::V6(ip) if ip.is_unspecified() => addr.set_ip(Ipv6Addr::LOCALHOST.into()),
        _ => {}
    }

    format!("http://{addr}/")
}

fn open_browser(url: &str) -> Result<()> {
    let mut command = if cfg!(windows) {
        let mut command = process::Command::new("rundll32");
        command.args(["url.dll,FileProtocolHandler", url]);
        command
    } else if cfg!(target_os = "macos") {
        let mut command = process::Command::new("open");
        command.arg(url);
        command
    } else {
        let mut command = process::Command::new("xdg-open");
        command.arg(url);
        command
    };

    let mut child = command.spawn().context("spawning browser")?;
    std::thread::spawn(move || child.wait());
    Ok(())
}
