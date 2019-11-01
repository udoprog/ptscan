use std::path::PathBuf;

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Default)]
pub struct Opts {
    pub process: Option<String>,
    pub suspend: bool,
    pub load_file: Option<PathBuf>,
}

/// Parse commandline options.
pub fn opts() -> Opts {
    let mut opts = Opts::default();

    let m = app().get_matches();

    opts.process = m.value_of("process").map(String::from);
    opts.suspend = m.is_present("suspend");
    opts.load_file = m.value_of("load").map(PathBuf::from);

    opts
}

fn app() -> clap::App<'static, 'static> {
    use clap::{App, Arg};

    App::new("ptscan")
        .version(VERSION)
        .author("John-John Tedro <udoprog@tedro.se>")
        .about("Scans memory of processes")
        .arg(
            Arg::with_name("process")
                .help("Only analyze processes matching the given name.")
                .takes_value(true)
                .long("process")
                .short("p"),
        )
        .arg(
            Arg::with_name("load")
                .help("Load state from the specified file.")
                .takes_value(true)
                .long("load")
                .short("l"),
        )
        .arg(
            Arg::with_name("suspend")
                .help("Suspend the process while scanning it.")
                .long("suspend"),
        )
}
