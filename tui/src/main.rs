#![feature(backtrace)]

use anyhow::{anyhow, bail, Context as _};
use hashbrown::{hash_map, HashMap};
use ptscan::{
    Address, FilterExpr, InitialScanConfig, Location, MemoryInformation, Offset, Pointer,
    PointerBase, ProcessHandle, ProcessId, Scan, ScanProgress, ScanResult, Size, Test, Token, Type,
    Value, ValueExpr,
};
use std::{
    collections::{BTreeMap, VecDeque},
    fs::File,
    io::{self, Write as _},
    path::PathBuf,
    sync::Arc,
    time::Instant,
};

use crossterm::{
    cursor,
    input::{input, InputEvent, KeyEvent, TerminalInput},
    screen::RawScreen,
    style, terminal,
    terminal::ClearType,
    QueueableCommand,
};

static DEFAULT_LIMIT: usize = 10;

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
        .version(ptscan::VERSION)
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

fn try_main() -> anyhow::Result<()> {
    let opts = opts();
    let thread_pool = Arc::new(
        rayon::ThreadPoolBuilder::new()
            .num_threads(num_cpus::get())
            .build()?,
    );

    let output = io::stdout();

    let term = Term {
        input: input(),
        output,
    };

    let mut app = Application::new(term, thread_pool, opts.suspend);
    app.process_name = opts.process.clone();

    if let Some(file) = opts.load_file {
        app.current_file = file;
        app.load(None, false)?;
    }

    if app.process_name.is_some() {
        app.attach()?;
    }

    app.run()?;
    Ok(())
}

fn main() {
    if let Err(e) = try_main() {
        eprintln!("{}", e);
        eprintln!("{}", e.backtrace());

        for c in e.chain().skip(1) {
            eprintln!("Caused by: {}", c);

            if let Some(bt) = c.backtrace() {
                eprintln!("Backtrace: {}", bt);
            }
        }
    }
}

struct NoopProgress;

impl ScanProgress for NoopProgress {
    fn report_bytes(&mut self, _: Size) -> anyhow::Result<()> {
        Ok(())
    }

    fn report(&mut self, _: usize, _: u64) -> anyhow::Result<()> {
        Ok(())
    }
}

struct SimpleProgress<'a> {
    width: usize,
    term: &'a mut Term,
    what: &'static str,
    token: Option<&'a Token>,
    colors: &'a BTreeMap<usize, style::Color>,
}

impl<'a> SimpleProgress<'a> {
    fn new(
        term: &'a mut Term,
        what: &'static str,
        token: Option<&'a Token>,
        colors: &'a BTreeMap<usize, style::Color>,
    ) -> SimpleProgress<'a> {
        Self {
            width: 20,
            term,
            what,
            token,
            colors,
        }
    }
}

impl<'a> ScanProgress for SimpleProgress<'a> {
    fn report_bytes(&mut self, _: Size) -> anyhow::Result<()> {
        // NB: do nothing
        Ok(())
    }

    fn report(&mut self, percentage: usize, count: u64) -> anyhow::Result<()> {
        use std::fmt::Write as _;
        use std::iter;

        self.term.save_position()?;
        self.term.clear(ClearType::CurrentLine)?;

        let amount = (percentage * self.width) / 100;

        let start = iter::repeat('■')
            .enumerate()
            .map(|(i, c)| {
                let color = self
                    .colors
                    .range(i..)
                    .next()
                    .map(|e| *e.1)
                    .unwrap_or(style::Color::White);
                style::style(c).with(color).to_string()
            })
            .take(amount)
            .collect::<String>();

        let padding = iter::repeat(' ')
            .take(self.width - amount)
            .collect::<String>();

        let mut buf = format!(
            "[{}{}] {:>3}% ({} results): {}",
            start, padding, percentage, count, self.what
        );

        if let Some(token) = self.token {
            if token.test() {
                write!(
                    buf,
                    "{}",
                    style::style(" (cancelled)").with(style::Color::DarkRed)
                )?;
            }
        }

        write!(self.term, "{}", buf)?;
        self.term.restore_position()?;
        self.term.flush()?;
        Ok(())
    }
}

/// Very simple terminal wrapper around crossterm.
struct Term {
    input: TerminalInput,
    output: io::Stdout,
}

impl Term {
    /// Perform an panicky write to the specified line, indicating that an error happened.
    fn move_to_line(&mut self, row: u16) -> anyhow::Result<()> {
        self.output.queue(cursor::MoveTo(0, row))?;
        Ok(())
    }

    /// Hide the terminal cursor.
    fn hide(&mut self) -> anyhow::Result<()> {
        self.output.queue(cursor::Hide)?;
        Ok(())
    }

    /// Show the terminal cursor.
    fn show(&mut self) -> anyhow::Result<()> {
        self.output.queue(cursor::Show)?;
        Ok(())
    }

    /// Queue up command to go to the given position in terminal.
    fn goto(&mut self, col: u16, row: u16) -> anyhow::Result<()> {
        self.output.queue(cursor::MoveTo(col, row))?;
        Ok(())
    }

    /// Queue up command to go to the given position in terminal.
    fn move_up(&mut self, rows: u16) -> anyhow::Result<()> {
        self.output.queue(cursor::MoveUp(rows))?;
        Ok(())
    }

    /// Queue up a clear command.
    fn clear(&mut self, ty: ClearType) -> anyhow::Result<()> {
        self.output.queue(terminal::Clear(ty))?;
        Ok(())
    }

    fn save_position(&mut self) -> anyhow::Result<()> {
        self.output.queue(cursor::SavePosition)?;
        Ok(())
    }

    fn restore_position(&mut self) -> anyhow::Result<()> {
        self.output.queue(cursor::RestorePosition)?;
        Ok(())
    }

    /// Perform some work that can be interrupted by pressing `q` or CTRL+C.
    fn work<W, R>(&mut self, work: W) -> anyhow::Result<R>
    where
        W: Send + FnOnce(&mut Self, &Token) -> anyhow::Result<R>,
        R: Send,
    {
        use std::sync::atomic::{AtomicBool, Ordering};

        let token = Token::new();
        let token = &token;

        self.hide()?;
        self.flush()?;

        let mut stdin = self.input.read_async();
        let raw = RawScreen::into_raw_mode()?;

        let term = &mut *self;
        let mut out = None;

        let exited = AtomicBool::new(false);

        rayon::scope(|s| {
            s.spawn(|_| {
                out = Some(work(term, token));
                exited.store(true, Ordering::Release);
            });

            while !exited.load(Ordering::Acquire) {
                if let Some(event) = stdin.next() {
                    match event {
                        InputEvent::Keyboard(KeyEvent::Char('q')) => break,
                        InputEvent::Keyboard(KeyEvent::Ctrl('c')) => break,
                        _ => (),
                    }
                }

                std::thread::sleep(std::time::Duration::from_millis(50));
            }

            token.set();
            Ok::<_, anyhow::Error>(())
        })?;

        drop(raw);
        self.clear(ClearType::CurrentLine)?;
        self.show()?;
        self.flush()?;
        out.ok_or_else(|| anyhow!("no output from work"))?
    }
}

impl io::Write for Term {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.output.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.output.flush()
    }
}

struct Application {
    term: Term,
    thread_pool: Arc<rayon::ThreadPool>,
    suspend: bool,
    /// Process name that we are attached to.
    process_name: Option<String>,
    /// Process that we are currently attached to.
    handle: Option<ProcessHandle>,
    scans: HashMap<String, Scan>,
    current_scan: String,
    /// Current file being saved/loaded.
    current_file: PathBuf,
    current_format: FileFormat,
    /// If the save file should be compressed or not.
    current_compress: bool,
    /// The current active limit.
    limit: usize,
    /// Get the color mapped to the given index.
    colors: BTreeMap<usize, style::Color>,
}

impl Application {
    /// Create a new application with the given options.
    fn new(term: Term, thread_pool: Arc<rayon::ThreadPool>, suspend: bool) -> Application {
        Application {
            term,
            thread_pool,
            suspend,
            process_name: None,
            handle: None,
            scans: HashMap::new(),
            current_scan: String::from("default"),
            current_file: PathBuf::from("state"),
            current_format: FileFormat::Cbor,
            current_compress: true,
            limit: DEFAULT_LIMIT,
            colors: Self::colors(),
        }
    }

    fn colors() -> BTreeMap<usize, style::Color> {
        let mut m = BTreeMap::new();

        for i in 0..20usize {
            let current = (i * 0x80) / 20;

            m.insert(
                i,
                style::Color::Rgb {
                    r: 0x40,
                    g: 0x40,
                    b: 0x80 + current as u8,
                },
            );
        }

        m
    }

    /// Run the application in a loop.
    pub fn run(&mut self) -> anyhow::Result<()> {
        use ptscan::utils::Words;
        let mut app = Self::app();

        loop {
            write!(
                self.term,
                "{}",
                style::style("ptscan> ")
                    .with(style::Color::Green)
                    .attribute(style::Attribute::Bold),
            )?;
            self.term.flush()?;
            let line = self.term.input.read_line()?;

            let it = std::iter::once(String::from("ptscan")).chain(Words::new(&line));

            let m = match app.get_matches_from_safe_borrow(it) {
                Ok(m) => m,
                Err(e) => {
                    writeln!(self.term, "{}", e.message)?;
                    continue;
                }
            };

            let action = match self.line_into_action(&m) {
                Ok(action) => action,
                Err(e) => {
                    writeln!(self.term, "{}", e)?;
                    continue;
                }
            };

            match self.apply_action(&mut app, action) {
                Ok(true) => return Ok(()),
                Ok(false) => {}
                Err(e) => {
                    writeln!(self.term, "{}", e)?;

                    for cause in e.chain().skip(1) {
                        writeln!(self.term, "caused by: {}", cause)?;

                        if let Some(bt) = cause.backtrace() {
                            eprintln!("backtrace: {}", bt);
                        }
                    }

                    continue;
                }
            }
        }
    }

    /// Returns a boolean indicating if the process should exit.
    fn apply_action(
        &mut self,
        app: &mut clap::App<'_, '_>,
        action: Action,
    ) -> anyhow::Result<bool> {
        match action {
            Action::List => {
                if self.scans.is_empty() {
                    writeln!(self.term, "No scans")?;
                    return Ok(false);
                }

                writeln!(self.term, "Scans:")?;

                for (key, s) in &self.scans {
                    writeln!(self.term, " - `{}` with {} result(s)", key, s.results.len())?;
                }
            }
            Action::Switch { name } => {
                writeln!(self.term, "Switch to scan `{}`", name)?;
                self.current_scan = name;
            }
            Action::Exit => {
                return Ok(true);
            }
            Action::Help => {
                let mut out = Vec::new();
                app.write_long_help(&mut out)?;
                writeln!(self.term, "{}", String::from_utf8(out)?)?;
            }
            Action::Clear => {
                self.term.clear(ClearType::All)?;
                self.term.goto(0, 0)?;
                self.term.flush()?;
            }
            Action::Process {
                include_modules,
                include_threads,
                include_memory,
            } => {
                self.process(include_modules, include_threads, include_memory)?;
            }
            Action::Attach(name) => {
                if name.is_some() {
                    self.process_name = name;
                }

                self.attach()?;
            }
            Action::Suspend => {
                let handle = match self.handle.as_ref() {
                    Some(handle) => handle,
                    None => bail!("not attached to a process"),
                };

                handle.process.suspend()?;
            }
            Action::Resume => {
                let handle = match self.handle.as_ref() {
                    Some(handle) => handle,
                    None => bail!("not attached to a process"),
                };

                handle.process.resume()?;
            }
            Action::Print {
                limit,
                expr,
                ty,
                verbose,
            } => {
                if let Some(limit) = limit {
                    self.limit = limit;
                }

                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => bail!("No active scan"),
                };

                let value_expr = if let (Some(expr), Some(handle)) = (&expr, self.handle.as_ref()) {
                    ValueExpr::parse(expr, &handle.process)?
                } else {
                    ValueExpr::Value
                };

                let mut stored;
                let results = &scan.results[..usize::min(scan.results.len(), self.limit)];
                // NB: possibly updated values.
                let mut current = results;

                if let Some(handle) = self.handle.as_ref() {
                    stored = current.to_vec();
                    handle.refresh_values(
                        &self.thread_pool,
                        &mut stored,
                        None,
                        ty,
                        NoopProgress,
                        &value_expr,
                    )?;
                    current = &stored;
                }

                writeln!(self.term, "Scan: {}", self.current_scan)?;

                Self::print(
                    &mut self.term,
                    scan.results.len(),
                    current.iter().map(|r| &**r).enumerate(),
                    &value_expr,
                    verbose,
                )?;
            }
            Action::Watch {
                limit,
                filter,
                change_length,
                incremental,
            } => {
                if let Some(limit) = limit {
                    self.limit = limit;
                }

                self.watch(filter.as_ref(), change_length, incremental)?;
            }
            Action::Del { index } => {
                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => bail!("No active scan"),
                };

                if index < scan.results.len() {
                    let result = scan.results.swap_remove(index);
                    writeln!(self.term, "deleted:")?;
                    writeln!(self.term, "{} = {}", result.pointer, result.last())?;
                }
            }
            Action::Add { mut pointer, ty } => {
                let Application {
                    ref mut scans,
                    ref current_scan,
                    ..
                } = self;

                let scan = scans.entry(current_scan.clone()).or_default();

                let ty = ty.unwrap_or(Type::U32);

                let (last_address, value) = match self.handle.as_ref() {
                    Some(handle) => {
                        let mut proxy = handle.address_proxy(&pointer);
                        let address = proxy.follow_default()?;
                        let value = proxy.eval(ty)?;
                        (address, value)
                    }
                    None => (None, Value::None(ty)),
                };

                pointer.last_address = last_address;

                if let Some(handle) = &self.handle {
                    let base = match pointer.base {
                        PointerBase::Address { address } => {
                            handle.address_to_pointer_base(address)?
                        }
                        other => other,
                    };

                    pointer.base = base;
                }

                scan.results.push(Box::new(ScanResult::new(pointer, value)));
                scan.initial = false;
            }
            Action::Scan { filter, ty, config } => {
                self.scan(&filter, ty, config)?;
            }
            Action::Refresh { ty, expr, indexes } => {
                let handle = match self.handle.as_ref() {
                    Some(handle) => handle,
                    None => bail!("not attached to a process"),
                };

                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => bail!("no active scan to refresh!"),
                };

                let mut stored = vec![];
                let mut replace = vec![];
                let results = if !indexes.is_empty() {
                    for index in indexes {
                        if let Some(result) = scan.results.get_mut(index) {
                            stored.push(result.clone());
                            replace.push(index);
                        }
                    }

                    &mut stored[..]
                } else {
                    &mut scan.results[..]
                };

                if let (Some(expr), Some(handle)) = (&expr, self.handle.as_ref()) {
                    scan.value_expr = ValueExpr::parse(expr, &handle.process)?;
                };

                handle.refresh_values(
                    &self.thread_pool,
                    results,
                    None,
                    ty,
                    SimpleProgress::new(&mut self.term, "Refreshing values", None, &self.colors),
                    &scan.value_expr,
                )?;

                for (index, result) in replace.into_iter().zip(stored.into_iter()) {
                    scan.results[index] = result;
                }

                let len = usize::min(scan.results.len(), self.limit);
                Self::print(
                    &mut self.term,
                    scan.results.len(),
                    scan.results.iter().take(len).map(|r| &**r).enumerate(),
                    &scan.value_expr,
                    false,
                )?;
            }
            Action::PointerScan {
                name,
                address,
                append,
                value_type,
                max_depth,
                max_offset,
            } => {
                let value_type = value_type.unwrap_or(Type::None);

                let Self {
                    ref handle,
                    ref mut scans,
                    ref thread_pool,
                    ref colors,
                    ..
                } = *self;

                self.term.work(|term, token| {
                    Self::pointer_scan(
                        term,
                        token,
                        handle.as_ref(),
                        scans,
                        thread_pool,
                        colors,
                        name,
                        address,
                        append,
                        value_type,
                        max_depth,
                        max_offset,
                    )
                })?;
            }
            Action::Reset { name } => {
                let current_scan = &self.current_scan;

                let name = name
                    .as_ref()
                    .map(String::as_str)
                    .unwrap_or_else(|| current_scan.as_str());

                match self.scans.get_mut(name) {
                    Some(scan) => {
                        scan.results.clear();
                        scan.initial = true;
                    }
                    None => {
                        writeln!(self.term, "no scan in use")?;
                    }
                }
            }
            Action::Save {
                file,
                scan,
                format,
                compress,
                ..
            } => {
                if let Some(file) = file {
                    self.current_file = file;
                }

                if let Some(format) = format {
                    self.current_format = format;
                }

                if let Some(compress) = compress {
                    self.current_compress = compress;
                }

                self.save(scan.as_ref().map(String::as_str))?;
            }
            Action::Load {
                file,
                scan,
                append,
                format,
                compress,
                ..
            } => {
                if let Some(file) = file {
                    self.current_file = file;
                }

                if let Some(format) = format {
                    self.current_format = format;
                }

                if let Some(compress) = compress {
                    self.current_compress = compress;
                }

                self.load(scan.as_ref().map(String::as_str), append)?;
            }
            Action::Sort { reverse } => {
                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => bail!("No active scan"),
                };

                if reverse {
                    scan.results.sort_by(|a, b| a.pointer.cmp(&b.pointer));
                } else {
                    scan.results.sort_by(|a, b| b.pointer.cmp(&a.pointer));
                }
            }
        }

        Ok(false)
    }

    fn save(&mut self, scan: Option<&str>) -> anyhow::Result<()> {
        use flate2::{write::GzEncoder, Compression};

        let Self {
            ref mut scans,
            ref current_file,
            ref current_scan,
            ..
        } = self;

        let scan = scan.unwrap_or_else(|| current_scan.as_str());

        let count = {
            let scan = scans.entry(scan.to_string()).or_default();

            let mut f = File::create(current_file)
                .with_context(|| anyhow!("failed to create file `{}`", current_file.display()))?;

            if self.current_compress {
                let mut f = GzEncoder::new(&mut f, Compression::default());
                self.current_format.serialize(&mut f, scan)?;
                f.finish()?;
            } else {
                self.current_format.serialize(&mut f, scan)?
            };

            scan.results.len()
        };

        writeln!(
            self.term,
            "Saved {} results from scan `{}` to file `{}`",
            count,
            scan,
            current_file.display()
        )?;

        Ok(())
    }

    fn load(&mut self, scan: Option<&str>, append: bool) -> anyhow::Result<()> {
        use flate2::read::GzDecoder;

        let Self {
            ref mut scans,
            ref current_file,
            ref current_scan,
            ..
        } = self;

        let scan = scan.unwrap_or_else(|| current_scan.as_str());

        {
            let mut f = File::open(current_file)
                .with_context(|| anyhow!("Failed to open file `{}`", current_file.display()))?;

            let mut deserialized: Scan = if self.current_compress {
                self.current_format
                    .deserialize(&mut GzDecoder::new(&mut f))?
            } else {
                self.current_format.deserialize(&mut f)?
            };

            let scan = scans.entry(scan.to_string()).or_default();

            if append {
                scan.results.append(&mut deserialized.results);
            } else {
                scan.results = deserialized.results;
            }

            scan.value_expr = deserialized.value_expr;
            scan.initial = deserialized.initial;
            scan.aligned = deserialized.aligned;
        }

        writeln!(
            self.term,
            "Loaded scan `{}` from file `{}`",
            scan,
            current_file.display()
        )?;

        Ok(())
    }

    fn watch(
        &mut self,
        filter: Option<&FilterExpr>,
        change_length: usize,
        incremental: bool,
    ) -> anyhow::Result<()> {
        use std::{thread, time::Duration};

        let thread_pool = &self.thread_pool;

        let handle = match self.handle.as_ref() {
            Some(handle) => handle,
            None => bail!("not attached to a process"),
        };

        let scan = self.scans.entry(self.current_scan.to_string()).or_default();

        if scan.results.is_empty() {
            writeln!(self.term, "cannot watch, scan is empty")?;
            return Ok(());
        }

        let refresh_rate = Duration::from_millis(500);

        let limit = self.limit;

        self.term.clear(ClearType::All)?;

        let removed = self.term.work(|term, token| {
            if incremental {
                incremental_refresh_loop(
                    &*thread_pool,
                    &handle,
                    term,
                    token,
                    scan,
                    refresh_rate,
                    filter,
                    limit,
                )
            } else {
                refresh_loop(
                    &*thread_pool,
                    &handle,
                    term,
                    token,
                    scan,
                    refresh_rate,
                    change_length,
                    filter,
                    limit,
                )
            }
        })?;

        if removed > 0 {
            writeln!(
                self.term,
                "removed {} filtered items from scan `{}`",
                removed, self.current_scan
            )?;
        }

        return Ok(());

        struct State {
            initial: Value,
            values: VecDeque<(Instant, Value)>,
            changed: bool,
            removed: bool,
            capped: usize,
        }

        impl State {
            /// Get the last known value.
            pub fn last(&self) -> &Value {
                self.values.back().map(|v| &v.1).unwrap_or(&self.initial)
            }
        }

        fn incremental_refresh_loop(
            thread_pool: &rayon::ThreadPool,
            handle: &ProcessHandle,
            term: &mut Term,
            token: &Token,
            scan: &mut Scan,
            refresh_rate: Duration,
            filter: Option<&FilterExpr>,
            limit: usize,
        ) -> anyhow::Result<usize> {
            use std::fmt::Write as _;

            let mut last = Vec::with_capacity(limit);
            let mut offset = 0;
            let mut to_remove = Vec::new();
            let mut buf = String::new();

            term.clear(ClearType::All)?;
            term.goto(0, 0)?;
            term.hide()?;

            writeln!(
                term.output,
                "Refresh rate: {:?} (Press `q` or CTRL+C to stop watching)",
                refresh_rate,
            )?;

            while !token.test() && !scan.results.is_empty() {
                // wrap around.
                if offset >= limit {
                    offset = 0;
                }

                let len = usize::min(scan.results.len().saturating_sub(offset), limit);

                let mut results = &mut scan.results[..len];
                last.extend(results.iter().map(|v| v.last().clone()));

                handle.refresh_values(
                    thread_pool,
                    &mut results,
                    None,
                    None,
                    NoopProgress,
                    &scan.value_expr,
                )?;

                for (o, (last, result)) in last.iter().zip(results).enumerate() {
                    let index = offset + o;

                    let mut removed = false;
                    let mut changed = false;

                    if let Some(filter) = filter {
                        let mut proxy = handle.address_proxy(&result.pointer);
                        let ty = result.last().ty();

                        if let Test::False = filter.test(result.initial(), last, ty, &mut proxy)? {
                            to_remove.push(index);
                            removed = true;
                        } else {
                            result.pointer.last_address = proxy.follow_default()?;
                        }
                    }

                    if last != result.last() {
                        changed = true;
                    }

                    if removed || changed {
                        if let Some(address) = result.pointer.address() {
                            write!(buf, "{:03} : {} = ", index, address)?;
                        } else {
                            write!(buf, "{:03} : ? = ", index)?;
                        }

                        if changed {
                            write!(buf, "{} -> ", last)?;
                        }

                        if removed {
                            write!(
                                buf,
                                "{} (filtered)",
                                style::style(result.last()).with(style::Color::Red)
                            )?;
                        } else {
                            write!(
                                buf,
                                "{}",
                                style::style(result.last()).with(style::Color::Blue)
                            )?;
                        }

                        writeln!(term.output, "{}", buf)?;
                        buf.clear();
                    }
                }

                for i in to_remove.iter().rev() {
                    scan.results.swap_remove(*i);
                }

                last.clear();
                to_remove.clear();
                offset += limit;

                if !token.test() && !scan.results.is_empty() {
                    thread::sleep(refresh_rate);
                }
            }

            Ok(0)
        }

        fn refresh_loop(
            thread_pool: &rayon::ThreadPool,
            handle: &ProcessHandle,
            term: &mut Term,
            token: &Token,
            scan: &mut Scan,
            refresh_rate: Duration,
            change_length: usize,
            filter: Option<&FilterExpr>,
            limit: usize,
        ) -> anyhow::Result<usize> {
            let results = &scan.results[..usize::min(scan.results.len(), limit)];
            let mut results = results.to_vec();

            let line_below = results.len() as u16;
            let info_line = line_below;
            let error_line = info_line + 1;

            let mut states = results
                .iter()
                .map(|r| State {
                    initial: r.last().clone(),
                    values: VecDeque::new(),
                    changed: true,
                    removed: false,
                    capped: 0,
                })
                .collect::<Vec<_>>();

            let pointers = results
                .iter()
                .map(|r| r.pointer.clone())
                .collect::<Vec<_>>();

            let info = format!(
                "Refresh rate: {:?} (Press `q` or CTRL+C to stop watching)",
                refresh_rate,
            );

            term.clear(ClearType::All)?;
            term.goto(0, 0)?;

            term.move_to_line(info_line)?;
            writeln!(term, "{}", info)?;
            write_results(term, &pointers, &mut states)?;

            while !token.test() {
                if let Err(e) = handle.refresh_values(
                    thread_pool,
                    &mut results,
                    None,
                    None,
                    NoopProgress,
                    &scan.value_expr,
                ) {
                    term.move_to_line(error_line)?;
                    writeln!(term, "Error refreshing values: {}", e)?;
                }

                let mut any_changed = false;

                for (result, state) in results.iter_mut().zip(states.iter_mut()) {
                    if state.removed {
                        continue;
                    }

                    if let Some(filter) = filter {
                        let mut proxy = handle.address_proxy(&result.pointer);
                        let ty = result.last().ty();

                        if let Test::False =
                            filter.test(&state.initial, state.last(), ty, &mut proxy)?
                        {
                            any_changed = true;
                            state.removed = true;
                        } else {
                            result.pointer.last_address = proxy.follow_default()?;
                        }
                    }

                    if result.last() != state.last() {
                        state.changed = true;
                        any_changed = true;

                        state
                            .values
                            .push_back((Instant::now(), result.last().clone()));

                        while state.values.len() > change_length {
                            state.values.pop_front();
                            state.capped += 1;
                        }
                    }
                }

                if any_changed {
                    write_results(term, &pointers, &mut states)?;
                    term.goto(0, info_line)?;
                }

                term.hide()?;
                thread::sleep(refresh_rate);
            }

            let mut removed = 0;

            // remove removed items from watch.
            for (index, state) in states.into_iter().enumerate().rev() {
                if state.removed {
                    removed += 1;
                    scan.results.swap_remove(index);
                }
            }

            Ok(removed)
        }

        fn write_results(
            term: &mut Term,
            pointers: &[Pointer],
            states: &mut [State],
        ) -> anyhow::Result<()> {
            use std::fmt::Write as _;

            for (index, (pointer, state)) in pointers.iter().zip(states.iter_mut()).enumerate() {
                if !state.changed {
                    continue;
                }

                let mut buf = String::new();

                if let Some(address) = pointer.address() {
                    write!(buf, "{:03} : {} = ", index, address)?;
                } else {
                    write!(buf, "{:03} : ? = ", index)?;
                }

                let mut it = state.values.iter();
                let last = it.next_back();
                let mut last_change = None;

                write!(buf, "{}", state.initial)?;

                if state.capped > 0 {
                    write!(buf, " ... {}", state.capped)?;
                }

                while let Some((when, v)) = it.next() {
                    if let Some(last) = last_change {
                        let duration = when.duration_since(last);
                        write!(buf, " ({:.0?})", duration)?;
                    }

                    write!(buf, " -> {}", v)?;
                    last_change = Some(*when);
                }

                if let Some((when, last)) = last {
                    if let Some(last) = last_change {
                        let duration = when.duration_since(last);
                        write!(buf, " ({:.0?})", duration)?;
                    }

                    if !state.removed {
                        write!(
                            buf,
                            " -> {}",
                            style::style(last)
                                .with(style::Color::Blue)
                                .attribute(style::Attribute::Bold)
                        )?;
                    } else {
                        write!(buf, " -> {}", last)?;
                    }
                }

                if state.removed {
                    write!(
                        buf,
                        " {}",
                        style::style("(filtered)")
                            .with(style::Color::Red)
                            .attribute(style::Attribute::Bold)
                    )?;
                }

                term.move_to_line(index as u16)?;
                writeln!(term, "{}", buf)?;
                state.changed = false;
            }

            term.hide()?;
            term.flush()?;
            Ok(())
        }
    }

    /// Try to attach to some process.
    fn attach(&mut self) -> anyhow::Result<()> {
        // NB: storage for amended name, in case that's needed.
        let amended_name;

        let mut name = match self.process_name.as_ref() {
            Some(name) => name.as_str(),
            None => bail!("no process name to attach to"),
        };

        self.handle = match str::parse::<ProcessId>(name) {
            Ok(id) => ProcessHandle::open(id)?,
            Err(_) => {
                if !name.ends_with(".exe") {
                    amended_name = format!("{}.exe", name);
                    name = amended_name.as_str();
                }

                ProcessHandle::open_by_name(name)?
            }
        };

        if let Some(handle) = self.handle.as_mut() {
            handle.refresh_modules()?;
            handle.refresh_threads()?;

            writeln!(
                self.term,
                "attached to process `{}` ({})",
                handle.name, handle.process.process_id
            )?;

            return Ok(());
        }

        writeln!(
            self.term,
            "not attached: could not find any process matching `{}`",
            name
        )?;

        Ok(())
    }

    /// Perform a pointer scan towards the specified address.
    fn pointer_scan(
        term: &mut Term,
        token: &Token,
        handle: Option<&ProcessHandle>,
        scans: &mut HashMap<String, Scan>,
        thread_pool: &rayon::ThreadPool,
        colors: &BTreeMap<usize, style::Color>,
        name: String,
        needle: Address,
        append: bool,
        ty: Type,
        max_depth: Option<usize>,
        max_offset: Option<u64>,
    ) -> anyhow::Result<()> {
        type SVec = Vec<Offset>;

        let handle = match handle.as_ref() {
            Some(handle) => handle,
            None => bail!("not attached to a process"),
        };

        // FilterExpr to find all pointers.
        let pointers = FilterExpr::pointer(ValueExpr::Value, &handle.process)?;

        let mut scan = scans
            .entry(String::from("pointer-scan"))
            .or_insert_with(Scan::new);

        if scan.initial {
            scan.initial_scan(
                thread_pool,
                handle,
                &pointers,
                Type::Pointer,
                Some(token),
                SimpleProgress::new(term, "Finding pointers", None, colors),
                Default::default(),
            )?;

            scan.initial = false;
        }

        if token.test() {
            term.clear(ClearType::CurrentLine)?;
            writeln!(
                term.output,
                "{}",
                style::style("cancelled pointer scan").with(style::Color::Red),
            )?;
            return Ok(());
        }

        let mut forward = BTreeMap::new();
        let mut reverse = BTreeMap::new();

        for result in &scan.results {
            let to = match result.last().as_address() {
                Some(address) => address,
                None => continue,
            };

            let from = match result.pointer.follow_default(handle)? {
                Some(address) => address,
                None => continue,
            };

            forward.insert(from, to);
            reverse.insert(to, from);
        }

        let max_depth = max_depth.unwrap_or(7);
        let max_offset = Size::new(max_offset.unwrap_or(0x1000));

        term.clear(ClearType::CurrentLine)?;
        writeln!(term.output, "Using {} references", forward.len())?;
        writeln!(term.output, "Max Depth: {}", max_depth)?;
        writeln!(term.output, "Max Offset: 0x{}", max_offset)?;

        let mut results = Vec::new();

        let mut queue = VecDeque::new();
        queue.push_back((needle, 0usize, SVec::new()));

        // contains both all visited links and alternative paths for each address.
        let mut visited: HashMap<Address, Vec<SVec>> = HashMap::new();
        let mut visited_count = 0usize;

        let mut moved = false;

        while let Some((n, depth, path)) = queue.pop_front() {
            if token.test() {
                break;
            }

            let it = reverse.range(..=n).rev();

            for (from, hit) in it {
                let offset = n.offset_of(*from)?;

                if !offset.is_within(max_offset) {
                    break;
                }

                let mut path = path.clone();
                path.push(offset);

                match visited.entry(*hit) {
                    hash_map::Entry::Occupied(mut e) => {
                        e.get_mut().push(path);
                        visited_count += 1;
                        continue;
                    }
                    hash_map::Entry::Vacant(e) => {
                        e.insert(Vec::new());
                    }
                }

                match handle.find_location(*hit) {
                    Location::Module(module) => {
                        let mut path = path.clone();
                        let offset = hit.offset_of(module.range.base)?;
                        path.reverse();

                        let pointer = Pointer::new(
                            PointerBase::Module {
                                name: module.name.to_string(),
                                offset,
                            },
                            path.clone(),
                            Some(needle),
                        );

                        results.push(Box::new(ScanResult::new(pointer, Value::None(ty))));
                    }
                    // ignore thread stacks
                    Location::Thread(..) => {
                        continue;
                    }
                    _ => (),
                }

                let depth = depth + 1;

                if depth >= max_depth {
                    continue;
                }

                queue.push_back((*hit, depth, path));
            }

            // only write every one hundred to avoid spamming the terminal buffer.
            if queue.len() % 100 == 0 {
                if moved {
                    term.move_up(2)?;
                }

                term.clear(ClearType::CurrentLine)?;
                writeln!(
                    term.output,
                    "Queue Length: {}",
                    style::style(queue.len()).with(style::Color::Green),
                )?;
                term.clear(ClearType::CurrentLine)?;
                writeln!(
                    term.output,
                    "Found Pointers: {}",
                    style::style(results.len()).with(style::Color::Blue)
                )?;
                term.flush()?;
                moved = true;
            }
        }

        if !token.test() {
            writeln!(term.output, "Adding trailing backreferences")?;

            let mut moved = false;
            // add all tailing pointers.
            let mut additions = Vec::new();

            for (i, result) in results.iter().enumerate() {
                if token.test() {
                    break;
                }

                let mut address = result
                    .pointer
                    .base
                    .eval(handle)?
                    .ok_or_else(|| anyhow!("should resolve to address"))?;

                let mut path = SVec::new();
                let mut it = result.pointer.offsets.iter().copied();

                while !token.test() {
                    if let Some(extra) = visited.get(&address) {
                        visited_count -= extra.len();

                        for p in extra {
                            let mut path = path.clone();
                            path.extend(p.iter().rev().cloned());

                            let pointer =
                                Pointer::new(result.pointer.base.clone(), path, Some(needle));

                            additions.push(Box::new(ScanResult::new(pointer, Value::None(ty))));
                        }
                    }

                    if let Some(a) = forward.get(&address).copied() {
                        if let Some(o) = it.next() {
                            address = a.saturating_offset(o);
                            path.push(o);
                            continue;
                        }
                    }

                    break;
                }

                let rest = results.len() - (i + 1);

                if rest % 100 == 0 {
                    if moved {
                        term.move_up(2)?;
                    }

                    term.clear(ClearType::CurrentLine)?;
                    writeln!(
                        term.output,
                        "To Process: {}",
                        style::style(rest).with(style::Color::Green),
                    )?;

                    term.clear(ClearType::CurrentLine)?;
                    writeln!(
                        term.output,
                        "Found Pointers: {}",
                        style::style(results.len() + additions.len()).with(style::Color::Blue)
                    )?;

                    moved = true;
                }
            }

            results.append(&mut additions);
        }

        let len = results.len();

        let scan = scans.entry(name.to_string()).or_default();

        writeln!(term.output)?;

        if append {
            scan.results.append(&mut results);
            scan.initial = false;
            writeln!(term.output, "Appended {} paths to scan `{}`", len, name)?;
        } else {
            scan.results = results;
            scan.initial = false;
            writeln!(term.output, "Saved {} paths to scan `{}`", len, name)?;
        }

        writeln!(
            term.output,
            "1/3 {}",
            style::style(format!(
                "Freeing visited set w/ {} entries...",
                visited_count
            ))
            .with(style::Color::Yellow)
        )?;
        term.flush()?;
        drop(visited);
        term.move_up(1)?;
        term.clear(ClearType::CurrentLine)?;

        writeln!(
            term.output,
            "2/3 {}",
            style::style("Freeing forward set...").with(style::Color::Yellow)
        )?;
        term.flush()?;
        drop(forward);
        term.move_up(1)?;
        term.clear(ClearType::CurrentLine)?;

        writeln!(
            term.output,
            "3/3 {}",
            style::style("Freeing reverse set...").with(style::Color::Yellow)
        )?;
        term.flush()?;
        drop(reverse);
        term.move_up(1)?;
        term.clear(ClearType::CurrentLine)?;

        writeln!(
            term.output,
            "{}",
            style::style("Done!").with(style::Color::Green)
        )?;

        Ok(())
    }

    /// Scan memory using the given filter and the currently selected scan.
    fn scan(
        &mut self,
        filter: &FilterExpr,
        ty: Option<Type>,
        config: ScanConfig,
    ) -> anyhow::Result<()> {
        let handle = match self.handle.as_ref() {
            Some(handle) => handle,
            None => bail!("not attached to a process"),
        };

        if self.suspend || config.suspend {
            handle.process.suspend()?;
        }

        let Self {
            ref current_scan,
            ref thread_pool,
            ..
        } = self;

        let scan = self.scans.entry(current_scan.to_string()).or_default();

        let Self {
            ref mut term,
            ref colors,
            ..
        } = *self;

        let result = if scan.initial {
            let ty = match ty {
                Some(ty) => ty,
                None => filter
                    .value_type_of()?
                    .ok_or_else(|| anyhow!("cannot determine type of value"))?,
            };

            let mut c = InitialScanConfig::default();
            c.modules_only = config.modules_only;
            c.tasks = config.tasks;
            c.buffer_size = config.buffer_size;

            let result = term.work(|term, token| {
                scan.initial_scan(
                    thread_pool,
                    handle,
                    filter,
                    ty,
                    Some(token),
                    SimpleProgress::new(term, "Performing initial scan", Some(token), colors),
                    c,
                )
            });

            scan.initial = false;

            result
        } else {
            term.work(|term, token| {
                scan.scan(
                    thread_pool,
                    handle,
                    ty,
                    Some(token),
                    SimpleProgress::new(term, "Rescanning", Some(token), colors),
                    filter,
                )
            })
        };

        if self.suspend || config.suspend {
            handle.process.resume()?;
        }

        result?;

        let len = usize::min(scan.results.len(), self.limit);
        Self::print(
            &mut self.term,
            scan.results.len(),
            scan.results[..len].iter().map(|r| &**r).enumerate(),
            &scan.value_expr,
            false,
        )?;

        Ok(())
    }

    /// Print information on the current process, if any.
    fn process(
        &mut self,
        include_modules: bool,
        include_threads: bool,
        include_memory: bool,
    ) -> anyhow::Result<()> {
        let handle = match self.handle.as_ref() {
            Some(handle) => handle,
            None => bail!("no process attached"),
        };

        writeln!(self.term, "Name: {}", handle.name)?;

        writeln!(self.term, "Process id: {}", handle.process.process_id)?;
        writeln!(self.term, "Pointer width: {}", handle.process.pointer_width)?;
        writeln!(self.term, "64 bit: {}", handle.process.is_64bit)?;

        if include_modules {
            writeln!(self.term, "Modules:")?;

            for module in &handle.modules {
                writeln!(self.term, " - {:?}", module)?;
            }
        } else {
            writeln!(self.term, "Modules: *only shown with --modules*")?;
        }

        if include_threads {
            writeln!(self.term, "Threads:")?;

            for thread in &handle.threads {
                writeln!(self.term, " - {:?}", thread)?;
            }
        } else {
            writeln!(self.term, "Threads: *only shown with --threads*")?;
        }

        if include_memory {
            writeln!(self.term, "Memory Regions:")?;

            for region in handle.process.virtual_memory_regions() {
                let region = region?;

                let MemoryInformation {
                    range,
                    state,
                    ty,
                    protect,
                } = region;

                writeln!(
                    self.term,
                    " - {ty:?} [{}-{}] {protect:?} {state:?}",
                    range.base,
                    range.base.saturating_add(range.size),
                    ty = ty,
                    protect = protect,
                    state = state,
                )?;
            }
        } else {
            writeln!(self.term, "Memory Regions: *only shown with --memory*")?;
        }

        Ok(())
    }

    /// Print the current state of the scan.
    fn print<'a>(
        term: &mut Term,
        len: usize,
        results: impl IntoIterator<Item = (usize, &'a ScanResult)>,
        value_expr: &ValueExpr,
        verbose: bool,
    ) -> anyhow::Result<()> {
        use std::fmt::Write as _;

        let mut buf = String::new();
        let mut count = 0;

        for (index, result) in results {
            count += 1;

            buf.clear();

            write!(buf, "{:>03} : {}", index, result.pointer)?;

            if ValueExpr::Value != *value_expr {
                write!(buf, " : {}", value_expr)?;
            }

            write!(buf, " = ")?;
            write!(buf, "{}", result.last())?;

            if verbose {
                write!(buf, " as {}", result.last().ty())?;
            }

            term.clear(ClearType::CurrentLine)?;
            writeln!(term, "{}", buf)?;
        }

        if count == 0 {
            term.clear(ClearType::CurrentLine)?;
            writeln!(term, "no matching addresses")?;
            return Ok(());
        }

        if count < len {
            term.clear(ClearType::CurrentLine)?;
            writeln!(term, "... {} more omitted", len - count)?;
        }

        Ok(())
    }

    fn app() -> clap::App<'static, 'static> {
        use clap::{App, Arg};

        let app = App::new("ptscan");
        let app = app.subcommand(
            App::new("process")
                .alias("ps")
                .about("Show information on the current process")
                .arg(
                    Arg::with_name("modules")
                        .help("Include very detailed module information (.exe and .dll).")
                        .long("modules"),
                )
                .arg(
                    Arg::with_name("threads")
                        .help("Include very detailed thread information.")
                        .long("threads"),
                )
                .arg(
                    Arg::with_name("memory")
                        .help("Include very detailed memory information.")
                        .long("memory"),
                ),
        );

        let app = app.subcommand(
            App::new("refresh")
                .alias("r")
                .about("Refresh values in a scan, optionally setting a new type for them.")
                .arg(type_argument(
                    "The type of the filter, if it can't be derived.",
                ))
                .arg(
                    Arg::with_name("index")
                        .help("The result index to refresh (optional).")
                        .long("index")
                        .short("i")
                        .value_name("index")
                        .takes_value(true)
                        .multiple(true),
                )
                .arg(
                    Arg::with_name("expr")
                        .help("Value expression to use when printing.")
                        .value_name("expr")
                        .multiple(true),
                ),
        );

        let app = app.subcommand(
            App::new("scan")
                .about("Perform a scan")
                .arg(
                    Arg::with_name("list")
                        .help("List available scans.")
                        .long("list")
                        .short("l"),
                )
                .arg(
                    Arg::with_name("new")
                        .help("Create a new scan.")
                        .long("new")
                        .short("n")
                        .value_name("name")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("del")
                        .help("Delete the scan named <name>.")
                        .long("del")
                        .short("d")
                        .value_name("name")
                        .takes_value(true),
                )
                .arg(type_argument("The type of the value to scan for."))
                .arg(
                    Arg::with_name("modules-only")
                        .help("Only include modules in the initial scan.")
                        .long("modules-only"),
                )
                .arg(
                    Arg::with_name("suspend")
                        .help("Suspend the process during the scan.")
                        .long("suspend"),
                )
                .arg(
                    Arg::with_name("tasks")
                        .help("Number of tasks to use when scanning.")
                        .long("tasks")
                        .value_name("tasks"),
                )
                .arg(
                    Arg::with_name("buffer-size")
                        .help("Buffer size to use in bytes for each worker thread.")
                        .long("buffer-size")
                        .value_name("bytes"),
                )
                .arg(
                    Arg::with_name("filter")
                        .help("The filter to apply.")
                        .value_name("filter")
                        .multiple(true),
                ),
        );

        let app = app.subcommand(
            App::new("attach")
                .about("Attach to a process")
                .arg(Arg::with_name("name").help("Name of the process to attach to.")),
        );

        let app = app.subcommand(App::new("suspend").about("Suspend the attached to process."));

        let app = app.subcommand(App::new("resume").about("Resume the attached to process."));

        let app = app.subcommand(App::new("exit").alias("q").about("Exit the application"));

        let app = app.subcommand(
            App::new("print")
                .alias("p")
                .about("Print current scan")
                .arg(
                    Arg::with_name("limit")
                        .help("Limit the number of results.")
                        .long("limit")
                        .value_name("number")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("verbose")
                        .help("Include detailed information on scan results.")
                        .long("verbose")
                        .short("v"),
                )
                .arg(
                    Arg::with_name("expr")
                        .help("Value expression to use when printing.")
                        .value_name("expr")
                        .multiple(true),
                )
                .arg(type_argument("The type of the expression to print.")),
        );

        let app = app.subcommand(App::new("clear").alias("cls").about("Clear the screen"));

        let app = app.subcommand(
            App::new("watch")
                .alias("w")
                .about("Watch the values in the current scan for changes.")
                .arg(
                    Arg::with_name("limit")
                        .help("Limit the number of results to show.")
                        .long("limit")
                        .value_name("number")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("incremental")
                        .help("Show results incrementally instead of all at once (recommended for big watch sets).")
                        .long("incremental")
                        .short("i")
                )
                .arg(
                    Arg::with_name("length")
                        .help("The number of value changes to show.")
                        .long("length")
                        .value_name("number")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("filter")
                        .help("Remove values which do not match the specified filter.")
                        .long("filter")
                        .value_name("filter")
                        .takes_value(true),
                ),
        );

        let app = app.subcommand(App::new("help").about("Print help"));

        let app = app.subcommand(
            App::new("reset").about("Reset a scan.").arg(
                Arg::with_name("scan")
                    .help("The scan to reset.")
                    .long("scan")
                    .value_name("scan"),
            ),
        );

        let app = app.subcommand(
            App::new("save")
                .about("Save a scan to a file.")
                .arg(
                    Arg::with_name("file")
                        .help("The file to save the scan to.")
                        .value_name("file"),
                )
                .arg(
                    Arg::with_name("scan")
                        .help("The scan to save.")
                        .long("scan")
                        .value_name("scan")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("no-compress")
                        .help("Disable compression.")
                        .long("no-compress"),
                )
                .arg(
                    Arg::with_name("compress")
                        .help("Enable compression.")
                        .long("compress"),
                )
                .arg(
                    Arg::with_name("format")
                        .help("The format to use when saving, valid formats are: cbor, yaml.")
                        .long("format")
                        .value_name("format")
                        .takes_value(true),
                ),
        );

        let app = app.subcommand(
            App::new("sort").about("Sort scan results.").arg(
                Arg::with_name("reverse")
                    .help("Reverse the sort order.")
                    .long("reverse"),
            ),
        );

        let app = app.subcommand(
            App::new("load")
                .about("Load a scan from a file.")
                .arg(
                    Arg::with_name("file")
                        .help("The file to load the scan from.")
                        .value_name("file"),
                )
                .arg(
                    Arg::with_name("scan")
                        .help("The scan to load the file as.")
                        .long("scan")
                        .value_name("scan")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("append")
                        .help("Append to the current scan instead of overwriting it.")
                        .long("append"),
                )
                .arg(
                    Arg::with_name("no-compress")
                        .help("Disable compression.")
                        .long("no-compress"),
                )
                .arg(
                    Arg::with_name("compress")
                        .help("Enable compression.")
                        .long("compress"),
                )
                .arg(
                    Arg::with_name("format")
                        .help("The format to use when loading, valid formats are: cbor, yaml.")
                        .long("format")
                        .value_name("format")
                        .takes_value(true),
                ),
        );

        let app = app.subcommand(
            App::new("del")
                .about("Delete an address from the current scan")
                .arg(
                    Arg::with_name("index")
                        .help("The index to delete.")
                        .value_name("index")
                        .required(true)
                        .takes_value(true),
                ),
        );

        let app = app.subcommand(
            App::new("switch")
                .alias("sw")
                .about("Switch to a different scan.")
                .arg(
                    Arg::with_name("scan")
                        .help("The scan to switch to.")
                        .required(true),
                ),
        );

        let app = app.subcommand(
            App::new("list")
                .alias("l")
                .alias("ls")
                .about("List available scans."),
        );

        let app = app.subcommand(
            App::new("add")
                .about("Add an address to the current scan")
                .arg(type_argument("The type of the pointer added to the scan."))
                .arg(
                    Arg::with_name("pointer")
                        .help("The address/pointer to delete.")
                        .value_name("pointer")
                        .multiple(true),
                ),
        );

        let app = app.subcommand(
            App::new("pointer-scan")
                .alias("pt")
                .about("Perform a pointer scan on the specified address.")
                .arg(
                    Arg::with_name("scan")
                        .help("The name of the scan in which to store result.")
                        .long("scan")
                        .takes_value(true)
                        .value_name("scan"),
                )
                .arg(
                    Arg::with_name("address")
                        .help("The address to scan for.")
                        .required(true),
                )
                .arg(type_argument("The type of the resulting value."))
                .arg(
                    Arg::with_name("max-depth")
                        .help("The max pointer depth to scan for.")
                        .long("max-depth")
                        .value_name("number")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("max-offset")
                        .help("The max offset from any given address to scan for.")
                        .long("max-offset")
                        .value_name("number")
                        .takes_value(true),
                ),
        );

        return app;

        fn type_argument<'help>(help: &'help str) -> Arg<'help, 'static> {
            Arg::with_name("type")
                .help(help)
                .long("type")
                .short("t")
                .value_name("type")
                .takes_value(true)
        }
    }

    /// Convert a line into an action.
    pub fn line_into_action(&mut self, m: &clap::ArgMatches<'_>) -> anyhow::Result<Action> {
        match m.subcommand() {
            ("process", Some(m)) => {
                let include_modules = m.is_present("modules");
                let include_threads = m.is_present("threads");
                let include_memory = m.is_present("memory");

                Ok(Action::Process {
                    include_modules,
                    include_threads,
                    include_memory,
                })
            }
            ("exit", _) => Ok(Action::Exit),
            ("help", _) => Ok(Action::Help),
            ("clear", _) => Ok(Action::Clear),
            ("print", Some(m)) => {
                let limit = m.value_of("limit").map(str::parse).transpose()?;

                let expr = match m.values_of("expr") {
                    Some(values) => Some(values.collect::<Vec<_>>().join(" ")),
                    None => None,
                };

                let ty = m.value_of("type").map(str::parse).transpose()?;
                let verbose = m.is_present("verbose");

                Ok(Action::Print {
                    limit,
                    expr,
                    ty,
                    verbose,
                })
            }
            ("watch", Some(m)) => {
                let limit = m.value_of("limit").map(str::parse).transpose()?;

                let filter = match m.value_of("filter") {
                    Some(filter) => {
                        let process = self
                            .handle
                            .as_ref()
                            .map(|h| &h.process)
                            .ok_or_else(|| anyhow!("must be attached to process"))?;

                        Some(FilterExpr::parse(&filter, process)?)
                    }
                    None => None,
                };

                let change_length = m
                    .value_of("length")
                    .map(str::parse)
                    .transpose()?
                    .unwrap_or(5);

                let incremental = m.is_present("incremental");

                Ok(Action::Watch {
                    limit,
                    filter,
                    change_length,
                    incremental,
                })
            }
            ("switch", Some(m)) => {
                let name = m
                    .value_of("scan")
                    .ok_or_else(|| anyhow!("misssing <scan>"))?
                    .to_string();

                Ok(Action::Switch { name })
            }
            ("list", _) => Ok(Action::List),
            ("scan", Some(m)) => {
                let ty = m.value_of("type").map(str::parse).transpose()?;

                let filter = match m.values_of("filter") {
                    Some(values) => values.collect::<Vec<_>>().join(" "),
                    None => String::new(),
                };

                let process = self
                    .handle
                    .as_ref()
                    .map(|h| &h.process)
                    .ok_or_else(|| anyhow!("must be attached to process"))?;

                let filter = FilterExpr::parse(&filter, process)?;

                let mut config = ScanConfig::default();
                config.modules_only = m.is_present("modules-only");
                config.suspend = m.is_present("suspend");
                config.tasks = m.value_of("tasks").map(str::parse).transpose()?;
                config.buffer_size = m.value_of("buffer-size").map(str::parse).transpose()?;
                Ok(Action::Scan { filter, ty, config })
            }

            ("refresh", Some(m)) => {
                let ty = match m.value_of("type").map(str::parse).transpose()? {
                    Some(ty) => Some(ty),
                    None => None,
                };

                let expr = match m.values_of("expr") {
                    Some(values) => Some(values.collect::<Vec<_>>().join(" ")),
                    None => None,
                };

                let indexes = match m.values_of("index") {
                    Some(values) => values
                        .map(|s| str::parse(s).map_err(Into::into))
                        .collect::<anyhow::Result<Vec<_>>>()?,
                    None => vec![],
                };

                Ok(Action::Refresh { ty, expr, indexes })
            }
            ("attach", Some(m)) => {
                let name = m.value_of("name").map(|s| s.to_string());
                Ok(Action::Attach(name))
            }
            ("suspend", _) => Ok(Action::Suspend),
            ("resume", _) => Ok(Action::Resume),
            ("reset", _) => {
                let name = m.value_of("scan").map(|s| s.to_string());

                Ok(Action::Reset { name })
            }
            ("save", Some(m)) => {
                let file = m.value_of("file").map(PathBuf::from);
                let scan = m.value_of("scan").map(|s| s.to_string());
                let format = m.value_of("format").map(str::parse).transpose()?;

                let compress = if m.is_present("no-compress") {
                    Some(false)
                } else if m.is_present("compress") {
                    Some(true)
                } else {
                    None
                };

                Ok(Action::Save {
                    file,
                    scan,
                    format,
                    compress,
                })
            }
            ("sort", Some(m)) => {
                let reverse = m.is_present("reverse");
                Ok(Action::Sort { reverse })
            }
            ("load", Some(m)) => {
                let file = m.value_of("file").map(PathBuf::from);

                let scan = m.value_of("scan").map(|s| s.to_string());
                let append = m.is_present("append");
                let format = m.value_of("format").map(str::parse).transpose()?;

                let compress = if m.is_present("no-compress") {
                    Some(false)
                } else if m.is_present("compress") {
                    Some(true)
                } else {
                    None
                };

                Ok(Action::Load {
                    file,
                    scan,
                    append,
                    format,
                    compress,
                })
            }
            ("del", Some(m)) => {
                let index = m
                    .value_of("index")
                    .map(str::parse)
                    .transpose()?
                    .ok_or_else(|| anyhow!("missing <index>"))?;
                Ok(Action::Del { index })
            }
            ("add", Some(m)) => {
                let ty = m.value_of("type").map(str::parse).transpose()?;

                let pointer = match m.values_of("pointer") {
                    Some(values) => values.collect::<Vec<_>>().join(" "),
                    None => String::new(),
                };

                let pointer = Pointer::parse(&pointer)?;
                Ok(Action::Add { ty, pointer })
            }
            ("pointer-scan", Some(m)) => {
                let name = m
                    .value_of("name")
                    .unwrap_or_else(|| &self.current_scan)
                    .to_string();

                let address = m
                    .value_of("address")
                    .map(str::parse)
                    .transpose()?
                    .ok_or_else(|| anyhow!("missing <address>"))?;

                let append = m.is_present("append");

                let value_type = m.value_of("type").map(str::parse).transpose()?;
                let max_depth = m.value_of("max-depth").map(str::parse).transpose()?;
                let max_offset = m.value_of("max-offset").map(str::parse).transpose()?;

                Ok(Action::PointerScan {
                    name,
                    address,
                    append,
                    value_type,
                    max_depth,
                    max_offset,
                })
            }
            _ => Ok(Action::Help),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct ScanConfig {
    modules_only: bool,
    suspend: bool,
    tasks: Option<usize>,
    buffer_size: Option<usize>,
}

#[derive(Debug, Clone, Copy)]
pub enum FileFormat {
    Cbor,
    Yaml,
}

impl FileFormat {
    /// Deserialize the given thing using the current file format.
    pub fn deserialize<T, R>(self, r: &mut R) -> anyhow::Result<T>
    where
        T: for<'a> serde::Deserialize<'a>,
        R: io::Read,
    {
        Ok(match self {
            Self::Cbor => serde_cbor::from_reader(r)?,
            Self::Yaml => serde_yaml::from_reader(r)?,
        })
    }

    /// Serialize the given thing using the current file format.
    pub fn serialize<T, W>(self, r: &mut W, value: &T) -> anyhow::Result<()>
    where
        T: serde::Serialize,
        W: io::Write,
    {
        match self {
            Self::Cbor => serde_cbor::to_writer(r, value)?,
            Self::Yaml => serde_yaml::to_writer(r, value)?,
        }

        Ok(())
    }
}

impl std::str::FromStr for FileFormat {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "yaml" => FileFormat::Yaml,
            "cbor" => FileFormat::Cbor,
            _ => bail!("invalid file format: {}", s),
        })
    }
}

#[derive(Debug)]
pub enum Action {
    /// Clear the screen.
    Clear,
    /// Print help.
    Help,
    /// Exit the application.
    Exit,
    /// Print information on the current process.
    Process {
        include_modules: bool,
        include_threads: bool,
        include_memory: bool,
    },
    /// Attach to a new, or re-attach to an old process.
    Attach(Option<String>),
    Suspend,
    Resume,
    /// Initialize or refine the existing scan.
    Scan {
        filter: FilterExpr,
        ty: Option<Type>,
        config: ScanConfig,
    },
    /// Reset the state of the scan.
    Reset {
        /// The name of the scan to reset.
        name: Option<String>,
    },
    /// Print results from scan.
    Print {
        limit: Option<usize>,
        /// Expression to use when printing.
        expr: Option<String>,
        /// The type to print the values as.
        ty: Option<Type>,
        /// Print results verbosely.
        verbose: bool,
    },
    /// Watch changes to the scan.
    Watch {
        limit: Option<usize>,
        filter: Option<FilterExpr>,
        change_length: usize,
        incremental: bool,
    },
    /// Delete the given address from the current scan.
    Del {
        index: usize,
    },
    /// Add the given address with the given type.
    Add {
        pointer: Pointer,
        ty: Option<Type>,
    },
    /// List all scans.
    List,
    /// Switch to the given scan.
    Switch {
        name: String,
    },
    /// Perform a pointer path scan with the given name, for the specified address.
    PointerScan {
        name: String,
        address: Address,
        append: bool,
        value_type: Option<Type>,
        max_depth: Option<usize>,
        max_offset: Option<u64>,
    },
    Save {
        file: Option<PathBuf>,
        scan: Option<String>,
        format: Option<FileFormat>,
        compress: Option<bool>,
    },
    Load {
        file: Option<PathBuf>,
        scan: Option<String>,
        /// If we should append to the current scan instead of overwriting it.
        append: bool,
        format: Option<FileFormat>,
        compress: Option<bool>,
    },
    Sort {
        reverse: bool,
    },
    Refresh {
        ty: Option<Type>,
        expr: Option<String>,
        indexes: Vec<usize>,
    },
}
