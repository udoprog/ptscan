const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug, Default)]
pub struct Opts {
    pub process: Option<String>,
    pub suspend: bool,
}

/// Parse commandline options.
pub fn opts() -> Opts {
    let mut opts = Opts::default();

    let m = app().get_matches();

    opts.process = m.value_of("process").map(String::from);
    opts.suspend = m.is_present("suspend");

    opts
}

fn app() -> clap::App<'static, 'static> {
    clap::App::new("ptscan")
        .version(VERSION)
        .author("John-John Tedro <udoprog@tedro.se>")
        .about("Scans memory of processes")
        .arg(
            clap::Arg::with_name("process")
                .help("Only analyze processes matching the given name.")
                .takes_value(true)
                .long("process"),
        )
        .arg(
            clap::Arg::with_name("suspend")
                .help("Suspend the process while scanning it.")
                .long("suspend"),
        )
}
