#![feature(backtrace)]

use anyhow::{anyhow, bail, Context as _};
use hashbrown::HashMap;
use ptscan::{
    Address, FilterExpr, InitialScanConfig, MemoryInformation, Pointer, PointerBase, PointerScan,
    PointerScanBackreferenceProgress, PointerScanInitialProgress, ProcessHandle, ProcessId,
    ProcessInfo as _, RawPointer, Scan, ScanProgress, ScanResult, Size, Test, Token, Type,
    TypeHint, Value, ValueExpr, ValueInfo,
};
use std::{
    collections::{BTreeMap, VecDeque},
    fmt,
    fs::File,
    io::{self, Write as _},
    path::PathBuf,
    sync::Arc,
    time::{Duration, Instant},
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
static DEFAULT_INCREMENTAL_LIMIT: usize = 10_000;

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
        app.load(None, false, None)?;
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
    current_compress: Option<bool>,
    /// The current active limit.
    limit: usize,
    /// The watch limit in use.
    incremental_limit: usize,
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
            current_format: FileFormat::default(),
            current_compress: None,
            limit: DEFAULT_LIMIT,
            incremental_limit: DEFAULT_INCREMENTAL_LIMIT,
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
                    writeln!(self.term, " - `{}` with {} result(s)", key, s.len())?;
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
                if_some,
            } => {
                if let Some(limit) = limit {
                    self.limit = limit;
                }

                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => bail!("No active scan"),
                };

                let updated_expr;

                let value_expr = if let (Some(expr), Some(handle)) = (&expr, self.handle.as_ref()) {
                    updated_expr = ValueExpr::parse(expr, &handle.process)?;
                    &updated_expr
                } else {
                    &scan.value_expr
                };

                let mut stored;
                let results = &scan[..usize::min(scan.len(), self.limit)];
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
                        value_expr,
                    )?;
                    current = &stored;
                }

                let filter = move |result: &&ScanResult| {
                    if !if_some {
                        true
                    } else {
                        result.last().is_some()
                    }
                };

                writeln!(self.term, "Scan: {}", self.current_scan)?;

                Self::print(
                    &mut self.term,
                    scan.len(),
                    current.iter().map(|r| &**r).filter(filter).enumerate(),
                    &scan.comments,
                    value_expr,
                    verbose,
                )?;
            }
            Action::Eval { expr } => {
                let handle = match self.handle.as_ref() {
                    Some(handle) => handle,
                    None => bail!("not attached to process"),
                };

                let expr = ValueExpr::parse(&expr, &handle.process)?;

                let expr_type = expr
                    .type_of(
                        TypeHint::NoHint,
                        TypeHint::NoHint,
                        TypeHint::NoHint,
                        TypeHint::NoHint,
                    )?
                    .ok_or_else(|| anyhow!("cannot determine type of expression"))?;

                let initial = Value::None;
                let initial = ValueInfo {
                    ty: Type::None,
                    value: &initial,
                };

                let last = Value::None;
                let last = ValueInfo {
                    ty: Type::None,
                    value: &last,
                };

                let mut proxy = handle.null_address_proxy();

                let value = expr
                    .type_check(Type::None, Type::None, Type::None, expr_type)?
                    .eval(initial, last, &mut proxy)?;

                writeln!(
                    self.term,
                    " => {ty} : {value}",
                    ty = expr_type,
                    value = value
                )?;
            }
            Action::Watch {
                limit,
                filter,
                change_length,
                incremental,
            } => {
                if let Some(limit) = limit {
                    if incremental {
                        self.incremental_limit = limit;
                    } else {
                        self.limit = limit;
                    }
                }

                self.watch(filter.as_ref(), change_length, incremental)?;
            }
            Action::Del { scan, mut indexes } => {
                let Self {
                    ref current_scan,
                    ref mut scans,
                    ..
                } = *self;

                let scan = scan
                    .as_ref()
                    .map(String::as_str)
                    .unwrap_or_else(|| current_scan.as_str());
                let scan = match scans.get_mut(scan) {
                    Some(scan) => scan,
                    None => bail!("No active scan"),
                };

                indexes.sort_by(|a, b| b.cmp(&a));

                for index in indexes {
                    if index < scan.len() {
                        let result = scan.results.swap_remove(index);
                        writeln!(self.term, "deleted:")?;
                        writeln!(self.term, "{} = {}", result.pointer.fancy(), result.last())?;
                    }
                }
            }
            Action::Add {
                mut pointer,
                ty,
                comment,
                scan,
            } => {
                let Application {
                    ref mut scans,
                    ref current_scan,
                    ..
                } = self;

                let scan = scan.unwrap_or_else(|| current_scan.clone());
                let scan = scans.entry(scan).or_default();

                let ty = ty.unwrap_or(Type::U32);

                let (last_address, value) = match self.handle.as_ref() {
                    Some(handle) => {
                        let mut proxy = handle.address_proxy(&pointer);
                        let address = proxy.follow_default()?;
                        let (value, _) = proxy.eval(ty)?;
                        (address, value)
                    }
                    None => (None, Value::None),
                };

                *pointer.last_address_mut() = last_address;

                if let Some(handle) = &self.handle {
                    let base = match pointer.base() {
                        PointerBase::Address { address } => {
                            handle.address_to_pointer_base(*address)?
                        }
                        other => other.clone(),
                    };

                    *pointer.base_mut() = base;
                }

                if let Some(comment) = comment {
                    scan.comments.insert(pointer.raw().clone(), comment);
                }

                scan.push(Box::new(ScanResult::new(pointer, ty, value)));
                scan.initial = false;
            }
            Action::Scan {
                filter,
                initial,
                ty,
                config,
            } => {
                self.scan(&filter, initial, ty, config)?;
            }
            Action::Refresh {
                ty,
                expr,
                indexes,
                limit,
            } => {
                if let Some(limit) = limit {
                    self.limit = limit;
                }

                let handle = match self.handle.as_ref() {
                    Some(handle) => handle,
                    None => bail!("not attached to a process"),
                };

                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => bail!("no active scan to refresh!"),
                };

                let mut stored = Vec::new();
                let mut replace = vec![];

                let results = if indexes.is_empty() {
                    &mut scan.results[..]
                } else {
                    for index in indexes {
                        if let Some(result) = scan.get_mut(index) {
                            stored.push(result.clone());
                            replace.push(index);
                        }
                    }

                    &mut stored[..]
                };

                let mut update_value_expr = None;

                let value_expr = if let (Some(expr), Some(handle)) = (&expr, self.handle.as_ref()) {
                    &*update_value_expr.get_or_insert(ValueExpr::parse(expr, &handle.process)?)
                } else {
                    &scan.value_expr
                };

                handle.refresh_values(
                    &self.thread_pool,
                    results,
                    None,
                    ty,
                    SimpleProgress::new(&mut self.term, "Refreshing values", None, &self.colors),
                    value_expr,
                )?;

                let len = usize::min(scan.len(), self.limit);

                Self::print(
                    &mut self.term,
                    scan.len(),
                    scan.iter().take(len).map(|r| &**r).enumerate(),
                    &scan.comments,
                    value_expr,
                    false,
                )?;

                for (index, result) in replace.into_iter().zip(stored.into_iter()) {
                    scan[index] = result;
                }

                if let Some(value_expr) = update_value_expr {
                    scan.value_expr = value_expr;
                }
            }
            Action::PointerScan {
                name,
                address,
                append,
                value_type,
                max_depth,
                max_offset,
            } => {
                let value_type = match value_type {
                    Some(value_type) => value_type,
                    None => {
                        match self.scans.get(&self.current_scan).and_then(|s| s.find_result_by_address(address)) {
                            Some(result) => result.last_type(),
                            None => bail!("Cannot determine type of destination. \
                            Either specify it using `--type` or use an address from the current scan."),
                        }
                    }
                };

                let Self {
                    ref mut handle,
                    ref mut scans,
                    ref thread_pool,
                    ref colors,
                    ..
                } = *self;

                self.term.work(|term, token| {
                    Self::pointer_scan(
                        term,
                        token,
                        handle.as_mut(),
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
                        scan.clear();
                    }
                    None => {
                        writeln!(self.term, "no scan in use")?;
                    }
                }

                writeln!(self.term, "Reset scan `{}`", name)?;
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
                    self.current_compress = Some(compress);
                }

                self.save(scan.as_ref().map(String::as_str))?;
            }
            Action::Load {
                file,
                scan,
                append,
                format,
                compress,
                comment,
                ..
            } => {
                if let Some(file) = file {
                    if !append {
                        self.current_file = file;
                    }
                }

                if let Some(format) = format {
                    self.current_format = format;
                }

                if let Some(compress) = compress {
                    self.current_compress = Some(compress);
                }

                self.load(
                    scan.as_ref().map(String::as_str),
                    append,
                    comment.as_ref().map(String::as_str),
                )?;
            }
            Action::Sort { reverse } => {
                if let Some(scan) = self.scans.get_mut(&self.current_scan) {
                    if reverse {
                        scan.sort_by(|a, b| b.pointer.cmp(&a.pointer));
                    } else {
                        scan.sort_by(|a, b| a.pointer.cmp(&b.pointer));
                    }
                }
            }
            Action::Comment { index, text } => {
                let Self {
                    ref current_scan,
                    ref mut scans,
                    ..
                } = *self;

                let scan = scans.entry(current_scan.clone()).or_insert_with(Scan::new);

                let result = match scan.results.get(index) {
                    Some(result) => result,
                    None => bail!("no result for index `{}`", index),
                };

                if text.is_empty() {
                    scan.comments.remove(result.pointer.raw());
                } else {
                    scan.comments.insert(result.pointer.raw().clone(), text);
                }
            }
        }

        Ok(false)
    }

    fn save(&mut self, scan: Option<&str>) -> anyhow::Result<()> {
        use lz4::EncoderBuilder;

        let Self {
            ref mut scans,
            ref current_file,
            ref current_scan,
            current_compress,
            ref current_format,
            ref colors,
            ..
        } = *self;

        let compress = current_compress
            .as_ref()
            .copied()
            .unwrap_or_else(|| current_format.default_compression());

        let scan = scan.unwrap_or_else(|| current_scan.as_str());

        let written = {
            let scan = scans.entry(scan.to_string()).or_default();

            let mut f = File::create(current_file)
                .with_context(|| anyhow!("failed to create file `{}`", current_file.display()))?;

            let written = self.term.work(|term, cancel| {
                let progress = SimpleProgress::new(term, "Saving...", Some(cancel), colors);

                let written = if compress {
                    let mut f = EncoderBuilder::new().level(4).build(f)?;
                    let written = current_format.serialize_scan(&mut f, scan, cancel, progress)?;
                    f.finish().1?;
                    written
                } else {
                    current_format.serialize_scan(&mut f, scan, cancel, progress)?
                };

                Ok(written)
            })?;

            written
        };

        writeln!(
            self.term,
            "Saved {} results from scan `{}` to file `{}`",
            written,
            scan,
            current_file.display()
        )?;

        Ok(())
    }

    fn load(
        &mut self,
        scan: Option<&str>,
        append: bool,
        comment: Option<&str>,
    ) -> anyhow::Result<()> {
        use lz4::Decoder;

        let Self {
            ref mut scans,
            ref current_file,
            ref current_scan,
            ref colors,
            ref current_format,
            ref current_compress,
            ..
        } = *self;

        let compress = current_compress
            .as_ref()
            .copied()
            .unwrap_or_else(|| current_format.default_compression());

        let scan = scan.unwrap_or_else(|| current_scan.as_str());

        let (read, errors) = {
            let mut f = File::open(current_file)
                .with_context(|| anyhow!("Failed to open file `{}`", current_file.display()))?;

            let (deserialized, errors) = self.term.work(|term, cancel| {
                let progress = SimpleProgress::new(term, "Loading...", Some(cancel), colors);

                if compress {
                    let mut f = Decoder::new(f)?;
                    current_format.deserialize_scan(&mut f, cancel, progress)
                } else {
                    current_format.deserialize_scan(&mut f, cancel, progress)
                }
            })?;

            let scan = scans.entry(scan.to_string()).or_default();

            if let Some(comment) = comment {
                for result in &deserialized.results {
                    scan.comments
                        .insert(result.pointer.raw().clone(), comment.to_owned());
                }
            }

            let read = deserialized.results.len();

            if append {
                scan.append_scan(deserialized);
            } else {
                scan.load_scan(deserialized);
            }

            (read, errors)
        };

        writeln!(
            self.term,
            "Loaded {read} results ({errors} errors) to scan `{scan}` from file `{file}`",
            read = read,
            errors = errors,
            scan = scan,
            file = current_file.display(),
        )?;

        Ok(())
    }

    fn watch(
        &mut self,
        filter: Option<&FilterExpr>,
        change_length: usize,
        incremental: bool,
    ) -> anyhow::Result<()> {
        use std::thread;

        let thread_pool = &self.thread_pool;

        let handle = match self.handle.as_ref() {
            Some(handle) => handle,
            None => bail!("not attached to a process"),
        };

        let scan = self.scans.entry(self.current_scan.to_string()).or_default();

        if scan.is_empty() {
            writeln!(self.term, "cannot watch, scan is empty")?;
            return Ok(());
        }

        let refresh_rate = Duration::from_millis(500);

        let incremental_limit = self.incremental_limit;
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
                    incremental_limit,
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
            ty: Type,
            initial: Value,
            values: VecDeque<(Instant, Value)>,
            changed: bool,
            removed: bool,
            capped: usize,
        }

        impl State {
            /// Get the initial value information.
            pub fn initial_info(&self) -> ValueInfo<'_> {
                ValueInfo {
                    ty: self.ty,
                    value: &self.initial,
                }
            }

            /// Get the last known value.
            pub fn last_info(&self) -> ValueInfo<'_> {
                let value = self.values.back().map(|v| &v.1).unwrap_or(&self.initial);

                ValueInfo { ty: self.ty, value }
            }
        }

        fn incremental_refresh_loop(
            thread_pool: &rayon::ThreadPool,
            handle: &ProcessHandle,
            term: &mut Term,
            cancel: &Token,
            scan: &mut Scan,
            refresh_rate: Duration,
            filter: Option<&FilterExpr>,
            limit: usize,
        ) -> anyhow::Result<usize> {
            use std::fmt::Write as _;

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

            let start = Instant::now();

            while !cancel.test() && !scan.is_empty() {
                // wrap around.
                if offset >= limit {
                    offset = 0;
                }

                let len = usize::min(scan.len().saturating_sub(offset), limit);

                let mut results = &mut scan.results[..len];
                let previous = results.iter().cloned().collect::<Vec<_>>();

                handle.refresh_values(
                    thread_pool,
                    &mut results,
                    None,
                    None,
                    NoopProgress,
                    &scan.value_expr,
                )?;

                for (o, (prev, result)) in previous.into_iter().zip(results).enumerate() {
                    if cancel.test() {
                        break;
                    }

                    let index = offset + o;

                    let mut removed = false;
                    let mut changed = false;

                    if let Some(filter) = filter {
                        let mut proxy = handle.address_proxy(&result.pointer);

                        if let Test::False = filter.test(
                            result.initial_info(),
                            prev.last_info(),
                            prev.last_type(),
                            &mut proxy,
                        )? {
                            to_remove.push(index);
                            removed = true;
                        } else {
                            *result.pointer.last_address_mut() = proxy.follow_default()?;
                        }
                    }

                    if prev.last() != result.last() {
                        changed = true;
                    }

                    if removed || changed {
                        if let Some(d) = Instant::now().checked_duration_since(start) {
                            write!(buf, "{} : ", FormatDuration(d))?;
                        }

                        if let Some(address) = result.pointer.address() {
                            write!(buf, "{:03} : {}", index, address)?;
                        } else {
                            write!(buf, "{:03} : ?", index)?;
                        }

                        if let Some(comment) = scan.comments.get(result.pointer.raw()) {
                            write!(buf, " /* {} */", comment)?;
                        }

                        write!(buf, " =")?;

                        if changed {
                            write!(buf, " {} ->", prev)?;
                        }

                        if removed {
                            write!(
                                buf,
                                " {} (filtered)",
                                style::style(result.last()).with(style::Color::Red)
                            )?;
                        } else {
                            write!(
                                buf,
                                " {}",
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

                to_remove.clear();
                offset += limit;

                if scan.is_empty() {
                    break;
                }

                thread::sleep(refresh_rate);
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
            let results = &scan[..usize::min(scan.len(), limit)];
            let mut results = results.to_vec();

            let line_below = results.len() as u16;
            let info_line = line_below;
            let error_line = info_line + 1;

            let mut states = results
                .iter()
                .map(|r| State {
                    ty: r.initial_type(),
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
            write_results(term, &pointers, &mut states, &scan.comments)?;

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
                        let ty = result.last_type();

                        if let Test::False =
                            filter.test(state.initial_info(), state.last_info(), ty, &mut proxy)?
                        {
                            any_changed = true;
                            state.removed = true;
                        } else {
                            *result.pointer.last_address_mut() = proxy.follow_default()?;
                        }
                    }

                    if result.last() != state.last_info().value {
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
                    write_results(term, &pointers, &mut states, &scan.comments)?;
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
            comments: &HashMap<RawPointer, String>,
        ) -> anyhow::Result<()> {
            use std::fmt::Write as _;

            for (index, (pointer, state)) in pointers.iter().zip(states.iter_mut()).enumerate() {
                if !state.changed {
                    continue;
                }

                let mut buf = String::new();

                if let Some(address) = pointer.address() {
                    write!(buf, "{:03} : {}", index, address)?;
                } else {
                    write!(buf, "{:03} : ?", index)?;
                }

                if let Some(comment) = comments.get(pointer.raw()) {
                    write!(buf, " /* {} */", comment)?;
                }

                let mut it = state.values.iter();
                let last = it.next_back();
                let mut last_change = None;

                write!(buf, " = {}", state.initial)?;

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
                term.clear(ClearType::CurrentLine)?;
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
        handle: Option<&mut ProcessHandle>,
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
        let handle = match handle {
            Some(handle) => handle,
            None => bail!("not attached to a process"),
        };

        // FilterExpr to find all pointers.
        let pointers = FilterExpr::pointer(ValueExpr::Value, &handle.process)?;

        let mut scan = scans
            .entry(String::from("pointer-scan"))
            .or_insert_with(Scan::new);

        if scan.initial {
            handle
                .refresh_threads()
                .with_context(|| anyhow!("failed to refresh threads before scan"))?;

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

        let mut pointer_scan = PointerScan::new(thread_pool, handle, token);
        pointer_scan.max_depth = max_depth.unwrap_or(7);
        pointer_scan.max_offset = Size::new(max_offset.unwrap_or(0x1000));

        pointer_scan.build_references(scan.iter())?;

        term.clear(ClearType::CurrentLine)?;
        writeln!(
            term.output,
            "Using {} references",
            pointer_scan.forward.len()
        )?;
        writeln!(term.output, "Max Depth: {}", pointer_scan.max_depth)?;
        writeln!(term.output, "Max Offset: 0x{}", pointer_scan.max_offset)?;

        let mut results = Vec::new();
        pointer_scan.scan(needle, &mut results, &mut InitialProgress::new(term))?;

        if !token.test() {
            writeln!(term.output, "Adding trailing backreferences")?;
            pointer_scan.backreference_scan(
                ty,
                needle,
                &mut results,
                &mut BackreferenceProgress::new(term),
            )?;
        }

        let len = results.len();

        let scan = scans.entry(name.to_string()).or_default();

        writeln!(term.output)?;

        if append {
            scan.append(&mut results);
            scan.initial = false;
            writeln!(term.output, "Appended {} paths to scan `{}`", len, name)?;
        } else {
            scan.set(results);
            scan.initial = false;
            writeln!(term.output, "Saved {} paths to scan `{}`", len, name)?;
        }

        writeln!(
            term.output,
            "1/3 {}",
            style::style(format!(
                "Freeing visited set w/ {} entries...",
                pointer_scan.visited_count
            ))
            .with(style::Color::Yellow)
        )?;
        term.flush()?;
        pointer_scan.visited = Default::default();
        term.move_up(1)?;
        term.clear(ClearType::CurrentLine)?;

        writeln!(
            term.output,
            "2/3 {}",
            style::style("Freeing forward set...").with(style::Color::Yellow)
        )?;
        term.flush()?;
        pointer_scan.forward = Default::default();
        term.move_up(1)?;
        term.clear(ClearType::CurrentLine)?;

        writeln!(
            term.output,
            "3/3 {}",
            style::style("Freeing reverse set...").with(style::Color::Yellow)
        )?;
        term.flush()?;
        pointer_scan.reverse = Default::default();
        term.move_up(1)?;
        term.clear(ClearType::CurrentLine)?;

        writeln!(
            term.output,
            "{}",
            style::style("Done!").with(style::Color::Green)
        )?;

        return Ok(());

        struct InitialProgress<'a> {
            term: &'a mut Term,
            moved: bool,
        }

        impl<'a> InitialProgress<'a> {
            fn new(term: &'a mut Term) -> Self {
                Self { term, moved: false }
            }
        }

        impl PointerScanInitialProgress for InitialProgress<'_> {
            fn report(&mut self, queue_len: usize, results: usize) -> anyhow::Result<()> {
                // only write every one hundred to avoid spamming the terminal buffer.
                if queue_len > 1000 && queue_len % 1000 != 0 {
                    return Ok(());
                }

                if self.moved {
                    self.term.move_up(2)?;
                }

                self.term.clear(ClearType::CurrentLine)?;
                writeln!(
                    self.term.output,
                    "Queue Length: {}",
                    style::style(queue_len).with(style::Color::Green),
                )?;
                self.term.clear(ClearType::CurrentLine)?;
                writeln!(
                    self.term.output,
                    "Found Pointers: {}",
                    style::style(results).with(style::Color::Blue)
                )?;
                self.term.flush()?;
                self.moved = true;
                Ok(())
            }
        }

        struct BackreferenceProgress<'a> {
            term: &'a mut Term,
            moved: bool,
        }

        impl<'a> BackreferenceProgress<'a> {
            fn new(term: &'a mut Term) -> Self {
                Self { term, moved: false }
            }
        }

        impl PointerScanBackreferenceProgress for BackreferenceProgress<'_> {
            fn report(&mut self, remaining: usize, results: usize) -> anyhow::Result<()> {
                if remaining % 100 != 0 {
                    return Ok(());
                }

                if self.moved {
                    self.term.move_up(2)?;
                }

                self.term.clear(ClearType::CurrentLine)?;
                writeln!(
                    self.term.output,
                    "To Process: {}",
                    style::style(remaining).with(style::Color::Green),
                )?;

                self.term.clear(ClearType::CurrentLine)?;
                writeln!(
                    self.term.output,
                    "Found Pointers: {}",
                    style::style(results).with(style::Color::Blue)
                )?;

                self.moved = true;
                Ok(())
            }
        }
    }

    /// Scan memory using the given filter and the currently selected scan.
    fn scan(
        &mut self,
        filter: &FilterExpr,
        initial: bool,
        ty: Option<Type>,
        config: ScanConfig,
    ) -> anyhow::Result<()> {
        let handle = match self.handle.as_mut() {
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

        let result = if initial || scan.initial {
            let ty = filter
                .value_type_of(ty.map(TypeHint::Explicit).unwrap_or(TypeHint::NoHint))?
                .ok_or_else(|| anyhow!("cannot determine type of value"))?;

            handle
                .refresh_threads()
                .with_context(|| anyhow!("failed to refresh threads before scan"))?;

            let mut c = InitialScanConfig::default();
            c.modules_only = config.modules_only;
            c.tasks = config.tasks;
            c.buffer_size = config.buffer_size;
            c.alignment = config.alignment;

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

        let len = usize::min(scan.len(), self.limit);
        Self::print(
            &mut self.term,
            scan.len(),
            scan[..len].iter().map(|r| &**r).enumerate(),
            &scan.comments,
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
                    range.base.saturating_add_size(range.size),
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
        results: impl IntoIterator<Item = (usize, &'a ScanResult)> + Send,
        comments: &HashMap<RawPointer, String>,
        value_expr: &ValueExpr,
        verbose: bool,
    ) -> anyhow::Result<()> {
        use std::fmt::Write as _;

        term.work(move |term, cancel| {
            let mut buf = String::new();
            let mut count = 0;

            for (index, result) in results {
                if cancel.test() {
                    break;
                }

                count += 1;

                buf.clear();

                write!(buf, "{:>03} : {}", index, result.pointer.fancy())?;

                if let Some(comment) = comments.get(result.pointer.raw()) {
                    write!(buf, " /* {} */", comment)?;
                }

                if ValueExpr::Value != *value_expr {
                    write!(buf, " : {}", value_expr)?;
                }

                write!(buf, " = ")?;
                write!(buf, "{}", result.last())?;

                if verbose {
                    write!(buf, " as {}", result.last_type())?;
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
        })?;

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
                )
                .arg(
                    Arg::with_name("limit")
                        .help("Limit the number of results.")
                        .long("limit")
                        .value_name("number")
                        .takes_value(true),
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
                    Arg::with_name("initial")
                        .help("If we should force an initial scan or not. An initial scan scans the whole memory.")
                        .long("init"),
                )
                .arg(
                    Arg::with_name("alignment")
                        .help("Alignment to use for the initial scan.")
                        .long("alignment")
                        .value_name("size"),
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
                    Arg::with_name("if-some")
                        .help("Only print values which are not none.")
                        .long("if-some"),
                )
                .arg(
                    Arg::with_name("expr")
                        .help("Value expression to use when printing.")
                        .value_name("expr")
                        .multiple(true),
                )
                .arg(type_argument("The type of the expression to print.")),
        );

        let app = app.subcommand(
            App::new("eval")
                .alias("e")
                .about("Evaluate the specified value expression")
                .arg(
                    Arg::with_name("expr")
                        .help("Value expression to evaluate.")
                        .value_name("expr")
                        .required(true)
                        .multiple(true),
                ),
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
                )
                .arg(
                    Arg::with_name("comment")
                        .help("Assign the specified comment to all loaded results.")
                        .long("comment")
                        .value_name("comment")
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
                        .multiple(true),
                )
                .arg(
                    Arg::with_name("scan")
                        .help("The scan to delete from.")
                        .long("scan")
                        .value_name("scan")
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
                .arg(
                    Arg::with_name("scan")
                        .help("The scan to add the result to.")
                        .long("scan")
                        .takes_value(true)
                        .value_name("scan"),
                )
                .arg(
                    Arg::with_name("comment")
                        .help("Comment to add to the result.")
                        .long("comment")
                        .takes_value(true)
                        .value_name("text"),
                )
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

        let app = app.subcommand(
            App::new("comment")
                .alias("c")
                .about("Set a comment for a result.")
                .arg(
                    Arg::with_name("index")
                        .help("The index to set the comment for.")
                        .value_name("index"),
                )
                .arg(
                    Arg::with_name("comment")
                        .help("The comment to set.")
                        .value_name("comment"),
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
                let if_some = m.is_present("if-some");

                Ok(Action::Print {
                    limit,
                    expr,
                    ty,
                    verbose,
                    if_some,
                })
            }
            ("eval", Some(m)) => {
                let expr = m
                    .values_of("expr")
                    .ok_or_else(|| anyhow!("missing <expr>"))?
                    .collect::<Vec<_>>()
                    .join(" ");

                Ok(Action::Eval { expr })
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

                let initial = m.is_present("initial");

                let mut config = ScanConfig::default();
                config.modules_only = m.is_present("modules-only");
                config.suspend = m.is_present("suspend");
                config.tasks = m.value_of("tasks").map(str::parse).transpose()?;
                config.buffer_size = m.value_of("buffer-size").map(str::parse).transpose()?;
                config.alignment = m.value_of("alignment").map(str::parse).transpose()?;
                Ok(Action::Scan {
                    filter,
                    initial,
                    ty,
                    config,
                })
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

                let limit = m.value_of("limit").map(str::parse).transpose()?;
                Ok(Action::Refresh {
                    ty,
                    expr,
                    indexes,
                    limit,
                })
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
                let comment = m.value_of("comment").map(String::from);

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
                    comment,
                })
            }
            ("del", Some(m)) => {
                let scan = m.value_of("scan").map(String::from);
                let indexes = match m.values_of("index") {
                    Some(values) => values
                        .map(|s| str::parse(s).map_err(anyhow::Error::from))
                        .collect::<anyhow::Result<Vec<usize>>>()?,
                    None => vec![],
                };

                Ok(Action::Del { scan, indexes })
            }
            ("add", Some(m)) => {
                let ty = m.value_of("type").map(str::parse).transpose()?;
                let comment = m.value_of("comment").map(String::from);
                let scan = m.value_of("scan").map(String::from);

                let pointer = match m.values_of("pointer") {
                    Some(values) => values.collect::<Vec<_>>().join(" "),
                    None => String::new(),
                };

                let pointer = Pointer::parse(&pointer)?;
                Ok(Action::Add {
                    ty,
                    pointer,
                    comment,
                    scan,
                })
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
            ("comment", Some(m)) => {
                let index = m
                    .value_of("index")
                    .map(str::parse)
                    .transpose()?
                    .ok_or_else(|| anyhow!("missing <index>"))?;

                let text = m
                    .value_of("comment")
                    .map(String::from)
                    .ok_or_else(|| anyhow!("missing <comment>"))?;

                Ok(Action::Comment { index, text })
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
    alignment: Option<usize>,
}

#[derive(Debug, Clone, Copy)]
pub enum FileFormat {
    Cbor,
    Yaml,
    Custom,
}

impl FileFormat {
    pub fn default_compression(self) -> bool {
        match self {
            Self::Cbor => true,
            Self::Yaml => false,
            Self::Custom => true,
        }
    }

    /// Deserialize the given thing using the current file format.
    pub fn deserialize_scan<R>(
        self,
        r: &mut R,
        cancel: &Token,
        progress: impl ScanProgress,
    ) -> anyhow::Result<(Scan, usize)>
    where
        R: io::Read,
    {
        Ok(match self {
            Self::Cbor => (serde_cbor::from_reader(r)?, 0),
            Self::Yaml => (serde_yaml::from_reader(r)?, 0),
            Self::Custom => custom_read_from(r, cancel, progress)?,
        })
    }

    /// Serialize the given thing using the current file format.
    pub fn serialize_scan<W>(
        self,
        w: &mut W,
        value: &Scan,
        cancel: &Token,
        progress: impl ScanProgress,
    ) -> anyhow::Result<usize>
    where
        W: io::Write,
    {
        match self {
            Self::Cbor => serde_cbor::to_writer(w, value)?,
            Self::Yaml => serde_yaml::to_writer(w, value)?,
            Self::Custom => return Ok(custom_write_to(w, value, cancel, progress)?),
        }

        Ok(value.results.len())
    }
}

impl Default for FileFormat {
    fn default() -> Self {
        Self::Custom
    }
}

/// Write the scan using a custom format that is considerably faster (but unsafe!) than typical serialization.
///
/// This format is _not_ portable.
fn custom_write_to<W: std::io::Write>(
    w: &mut W,
    scan: &Scan,
    cancel: &Token,
    mut progress: impl ScanProgress,
) -> anyhow::Result<usize> {
    use byteorder::{LittleEndian, WriteBytesExt as _};
    use std::convert::TryInto;

    let header = Header {
        aligned: scan.aligned,
        initial: scan.initial,
        value_expr: &scan.value_expr,
        comments: &scan.comments,
        results_len: scan.results.len(),
    };

    let mut buf = Vec::new();

    let mut p = std::usize::MAX;
    serde_cbor::to_writer(&mut buf, &header)?;
    w.write_u32::<LittleEndian>(buf.len().try_into()?)?;
    w.write_all(&buf)?;

    for (index, result) in scan.results.iter().enumerate() {
        if cancel.test() {
            return Ok(index);
        }

        buf.clear();

        serde_cbor::to_writer(&mut buf, result)?;
        w.write_u16::<LittleEndian>(buf.len().try_into()?)?;
        w.write_all(&buf)?;

        let next_p = (index * 100) / scan.results.len();

        if p != next_p {
            p = next_p;
            progress.report(p, index as u64)?;
        }
    }

    return Ok(scan.results.len());

    #[derive(Debug, serde::Serialize)]
    struct Header<'a> {
        #[serde(skip_serializing_if = "Option::is_none")]
        pub aligned: Option<bool>,
        pub initial: bool,
        #[serde(skip_serializing_if = "ValueExpr::is_default")]
        pub value_expr: &'a ValueExpr,
        #[serde(skip_serializing_if = "HashMap::is_empty")]
        pub comments: &'a HashMap<RawPointer, String>,
        pub results_len: usize,
    }
}

/// Write the scan using a custom format that is considerably faster (but unsafe!) than typical serialization.
///
/// This format is _not_ portable.
fn custom_read_from<R: std::io::Read>(
    r: &mut R,
    cancel: &Token,
    mut progress: impl ScanProgress,
) -> anyhow::Result<(Scan, usize)> {
    use byteorder::{LittleEndian, ReadBytesExt as _};
    use std::{convert::TryInto as _, io::Cursor};

    let mut p = std::usize::MAX;

    let mut buf = Vec::new();

    let len = r.read_u32::<LittleEndian>()?.try_into()?;
    buf.resize_with(len, u8::default);
    r.read_exact(&mut buf)?;
    let header: Header = serde_cbor::from_reader(&mut Cursor::new(&buf))?;

    let mut results = Vec::with_capacity(header.results_len);
    let mut errors = 0usize;

    for index in (0..header.results_len).take_while(|_| !cancel.test()) {
        let len = r.read_u16::<LittleEndian>()?.try_into()?;
        buf.resize_with(len, u8::default);
        r.read_exact(&mut buf)?;

        if let Ok(result) = serde_cbor::from_reader(&mut Cursor::new(&buf)) {
            results.push(result);
        } else {
            errors += 1;
        }

        let next_p = (index * 100) / header.results_len;

        if p != next_p {
            p = next_p;
            progress.report(p, index as u64)?;
        }
    }

    let scan = Scan {
        aligned: header.aligned,
        initial: header.initial,
        value_expr: header.value_expr,
        comments: header.comments,
        results,
    };

    return Ok((scan, errors));

    #[derive(Debug, serde::Deserialize)]
    struct Header {
        #[serde(default)]
        pub aligned: Option<bool>,
        pub initial: bool,
        #[serde(default)]
        pub value_expr: ValueExpr,
        #[serde(default)]
        pub comments: HashMap<RawPointer, String>,
        pub results_len: usize,
    }
}

impl std::str::FromStr for FileFormat {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "yaml" => FileFormat::Yaml,
            "cbor" => FileFormat::Cbor,
            "custom" => FileFormat::Custom,
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
        initial: bool,
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
        /// Only print values which are some.
        if_some: bool,
    },
    Eval {
        expr: String,
    },
    /// Watch changes to the scan.
    Watch {
        limit: Option<usize>,
        filter: Option<FilterExpr>,
        change_length: usize,
        incremental: bool,
    },
    /// Delete the given address from the specified or current scan.
    Del {
        scan: Option<String>,
        indexes: Vec<usize>,
    },
    /// Add the given address with the given type.
    Add {
        pointer: Pointer,
        ty: Option<Type>,
        comment: Option<String>,
        scan: Option<String>,
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
        /// Comment to apply to all loaded results.
        comment: Option<String>,
    },
    Sort {
        reverse: bool,
    },
    Refresh {
        ty: Option<Type>,
        expr: Option<String>,
        indexes: Vec<usize>,
        limit: Option<usize>,
    },
    Comment {
        index: usize,
        text: String,
    },
}

struct FormatDuration(Duration);

impl fmt::Display for FormatDuration {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut millis = self.0.as_millis();

        if millis >= 3_600_000 {
            write!(fmt, "{:02}:", millis / 3_600_000)?;
            millis = millis % 3_600_000;
        }

        write!(fmt, "{:02}", millis / 60_000)?;
        millis = millis % 60_000;
        write!(fmt, ":{:02}", millis / 1_000)?;
        millis = millis % 1_000;
        write!(fmt, ".{:03}", millis)?;
        Ok(())
    }
}
