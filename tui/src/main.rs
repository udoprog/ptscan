#![feature(backtrace)]

use anyhow::{anyhow, bail, Context as _};
use hashbrown::HashMap;
use ptscan::{
    filter, Address, Filter, InitialScanConfig, Location, MemoryInformation, Offset, Pointer,
    PointerBase, ProcessHandle, ProcessId, Scan, ScanProgress, ScanResult, Size, Test, Token, Type,
    Value, ValueExpr,
};
use std::{collections::VecDeque, fs::File, io, path::PathBuf, sync::Arc, time::Instant};

use crossterm::{
    cursor,
    input::{input, InputEvent, KeyEvent, TerminalInput},
    screen::RawScreen,
    style, terminal,
    terminal::ClearType,
    QueueableCommand,
};

static DEFAULT_LIMIT: usize = 10;

fn try_main() -> anyhow::Result<()> {
    let opts = ptscan::opts::opts();
    let thread_pool = Arc::new(
        rayon::ThreadPoolBuilder::new()
            .num_threads(num_cpus::get())
            .build()?,
    );

    let stdout = std::io::stdout();

    let term = Term {
        input: input(),
        stdout,
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
    term: &'a mut Term,
    what: &'static str,
    token: Option<&'a Token>,
}

impl<'a> SimpleProgress<'a> {
    fn new(term: &'a mut Term, what: &'static str, token: Option<&'a Token>) -> SimpleProgress<'a> {
        Self { term, what, token }
    }
}

impl<'a> ScanProgress for SimpleProgress<'a> {
    fn report_bytes(&mut self, _: Size) -> anyhow::Result<()> {
        // NB: do nothing
        Ok(())
    }

    fn report(&mut self, percentage: usize, count: u64) -> anyhow::Result<()> {
        use std::iter;

        self.term.save_position()?;
        self.term.clear(ClearType::CurrentLine)?;

        let repr = iter::repeat('#').take(percentage / 10).collect::<String>();

        self.term.print(format!(
            "{}: {}% ({} results): {}",
            repr, percentage, count, self.what
        ))?;

        if let Some(token) = self.token {
            if token.test() {
                self.term.print(" (cancelled)")?;
            }
        }

        self.term.restore_position()?;
        self.term.flush()?;
        Ok(())
    }
}

/// Very simple terminal wrapper around crossterm.
struct Term {
    input: TerminalInput,
    stdout: std::io::Stdout,
}

impl Term {
    /// Write a single line to the terminal.
    fn print<D>(&mut self, line: D) -> anyhow::Result<()>
    where
        D: std::fmt::Display + Clone,
    {
        use crossterm::style::style;
        self.stdout.queue(style::PrintStyledContent(style(line)))?;
        Ok(())
    }

    /// Write a single line to the terminal.
    fn print_line<D>(&mut self, line: D) -> anyhow::Result<()>
    where
        D: std::fmt::Display + Clone,
    {
        self.stdout
            .queue(style::PrintStyledContent(style::style(line)))?;
        self.stdout
            .queue(style::PrintStyledContent(style::style("\n")))?;
        self.flush()?;
        Ok(())
    }

    /// Perform an panicky write to the specified line, indicating that an error happened.
    fn write_on_line<D>(&mut self, row: u16, line: D) -> anyhow::Result<()>
    where
        D: std::fmt::Display + Clone,
    {
        self.stdout.queue(cursor::MoveTo(0, row))?;
        self.print_line(line)?;
        Ok(())
    }

    /// Hide the terminal cursor.
    fn hide(&mut self) -> anyhow::Result<()> {
        self.stdout.queue(cursor::Hide)?;
        Ok(())
    }

    /// Show the terminal cursor.
    fn show(&mut self) -> anyhow::Result<()> {
        self.stdout.queue(cursor::Show)?;
        Ok(())
    }

    /// Queue up command to go to the given position in terminal.
    fn goto(&mut self, col: u16, row: u16) -> anyhow::Result<()> {
        self.stdout.queue(cursor::MoveTo(col, row))?;
        Ok(())
    }

    /// Queue up a clear command.
    fn clear(&mut self, ty: ClearType) -> anyhow::Result<()> {
        self.stdout.queue(terminal::Clear(ty))?;
        Ok(())
    }

    fn save_position(&mut self) -> anyhow::Result<()> {
        self.stdout.queue(cursor::SavePosition)?;
        Ok(())
    }

    fn restore_position(&mut self) -> anyhow::Result<()> {
        self.stdout.queue(cursor::RestorePosition)?;
        Ok(())
    }

    /// Flush the output.
    fn flush(&mut self) -> anyhow::Result<()> {
        use std::io::Write as _;
        self.stdout.flush()?;
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
        }
    }

    /// Run the application in a loop.
    pub fn run(&mut self) -> anyhow::Result<()> {
        use ptscan::utils::Words;
        let mut app = Self::app();

        loop {
            self.term.print("ptscan> ")?;
            self.term.flush()?;
            let line = self.term.input.read_line()?;

            let it = std::iter::once(String::from("ptscan")).chain(Words::new(&line));

            let m = match app.get_matches_from_safe_borrow(it) {
                Ok(m) => m,
                Err(e) => {
                    self.term.print_line(format!("{}", e.message))?;
                    continue;
                }
            };

            let action = match self.line_into_action(&m) {
                Ok(action) => action,
                Err(e) => {
                    self.term.print_line(format!("{}", e))?;
                    continue;
                }
            };

            match self.apply_action(&mut app, action) {
                Ok(true) => return Ok(()),
                Ok(false) => {}
                Err(e) => {
                    self.term.print_line(format!("{}", e))?;

                    for cause in e.chain().skip(1) {
                        self.term.print_line(format!("caused by: {}", cause))?;

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
                    self.term.print_line("No scans")?;
                    return Ok(false);
                }

                self.term.print_line(format!("Scans:"))?;

                for (key, s) in &self.scans {
                    self.term
                        .print_line(format!(" - `{}` with {} result(s)", key, s.len()))?;
                }
            }
            Action::Switch { name } => {
                self.term.print_line(format!("Switch to scan `{}`", name))?;
                self.current_scan = name;
            }
            Action::Exit => {
                return Ok(true);
            }
            Action::Help => {
                let mut out = Vec::new();
                app.write_long_help(&mut out)?;
                self.term.print_line(String::from_utf8(out)?)?;
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
            Action::Print { limit, expr, ty } => {
                if let Some(limit) = limit {
                    self.limit = limit;
                }

                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => bail!("No active scan"),
                };

                let value_expr = if let (Some(expr), Some(handle)) = (&expr, self.handle.as_ref()) {
                    ValueExpr::parse(expr, ty, &handle.process)?
                } else {
                    ValueExpr::Value
                };

                let mut stored;
                let mut results = &scan[..usize::min(scan.len(), self.limit)];

                if let Some(handle) = self.handle.as_ref() {
                    stored = results.to_vec();
                    handle.refresh_values(
                        &self.thread_pool,
                        &mut stored,
                        None,
                        ty,
                        NoopProgress,
                        &value_expr,
                    )?;
                    results = &stored;
                }

                self.term
                    .print_line(format!("Scan: {}", self.current_scan))?;

                Self::print(
                    &mut self.term,
                    scan.len(),
                    results.iter().map(|r| &**r).enumerate(),
                    &scan.value_expr,
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
                    self.term.print_line("deleted:")?;
                    self.print_result(&*result)?;
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
                    Some(handle) => handle.address_proxy(&pointer).eval(ty)?,
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

                scan.push(Box::new(ScanResult { pointer, value }));
                scan.initial = false;
            }
            Action::Scan { filter, config } => {
                self.scan(&filter, config)?;
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
                let mut results = &mut scan.results[..];

                if !indexes.is_empty() {
                    for index in indexes {
                        if let Some(result) = scan.results.get_mut(index) {
                            stored.push(result.clone());
                            replace.push(index);
                        }
                    }

                    results = &mut stored[..];
                }

                if let (Some(expr), Some(handle)) = (&expr, self.handle.as_ref()) {
                    scan.value_expr = ValueExpr::parse(expr, ty, &handle.process)?;
                };

                handle.refresh_values(
                    &self.thread_pool,
                    results,
                    None,
                    ty,
                    SimpleProgress::new(&mut self.term, "Refreshing values", None),
                    &scan.value_expr,
                )?;

                for (index, result) in replace.into_iter().zip(stored.into_iter()) {
                    scan.results[index] = result;
                }

                let len = usize::min(scan.len(), self.limit);
                Self::print(
                    &mut self.term,
                    scan.len(),
                    scan.results.iter().take(len).map(|r| &**r).enumerate(),
                    &scan.value_expr,
                )?;
            }
            Action::PointerScan {
                name,
                address,
                append,
                value_type,
            } => {
                let value_type = value_type.unwrap_or(Type::None);
                self.pointer_scan(name, address, append, value_type)?;
            }
            Action::Reset { name } => {
                let name = name
                    .as_ref()
                    .map(String::as_str)
                    .unwrap_or(self.current_scan.as_str());

                match self.scans.get_mut(name) {
                    Some(scan) => {
                        scan.clear();
                        scan.initial = true;
                    }
                    None => {
                        self.term.print_line(format!("no scan in use"))?;
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
            Action::Sort => {
                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => bail!("No active scan"),
                };

                scan.results.sort_by(|a, b| a.pointer.cmp(&b.pointer));
            }
        }

        Ok(false)
    }

    fn save(&mut self, scan: Option<&str>) -> anyhow::Result<()> {
        use flate2::{write::GzEncoder, Compression};

        let Self {
            ref mut scans,
            ref current_file,
            ..
        } = self;

        let scan = scan.unwrap_or(self.current_scan.as_str());

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

        self.term.print_line(format!(
            "Saved {} results from scan `{}` to file `{}`",
            count,
            scan,
            current_file.display()
        ))?;

        Ok(())
    }

    fn load(&mut self, scan: Option<&str>, append: bool) -> anyhow::Result<()> {
        use flate2::read::GzDecoder;

        let Self {
            ref mut scans,
            ref current_file,
            ..
        } = self;

        let scan = scan.unwrap_or(self.current_scan.as_str());

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

        self.term.print_line(format!(
            "Loaded scan `{}` from file `{}`",
            scan,
            current_file.display()
        ))?;

        Ok(())
    }

    fn watch(
        &mut self,
        filter: Option<&Filter>,
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
            self.term.print_line("cannot watch, scan is empty")?;
            return Ok(());
        }

        let refresh_rate = Duration::from_millis(500);
        let results = &scan[..usize::min(scan.len(), self.limit)];
        let mut updates = results.to_vec();

        let line_below = results.len() as u16;
        let info_line = line_below;
        let error_line = info_line + 1;

        let mut states = updates
            .iter()
            .map(|r| {
                let mut values = VecDeque::new();
                values.push_back((Instant::now(), r.value.clone()));
                State {
                    values,
                    changed: true,
                    removed: false,
                    last: None,
                }
            })
            .collect::<Vec<_>>();

        let borrowed_stated = &mut states;
        let value_expr = &scan.value_expr;

        self.term.clear(ClearType::All)?;

        self.term.work(|term, token| {
            refresh_loop(
                term,
                token,
                &*thread_pool,
                info_line,
                error_line,
                refresh_rate,
                &mut updates,
                borrowed_stated,
                &handle,
                change_length,
                filter,
                incremental,
                value_expr,
            )
        })?;

        let mut removed = 0;

        // remove removed items from watch.
        for (index, state) in states.into_iter().enumerate().rev() {
            if state.removed {
                removed += 1;
                scan.results.swap_remove(index);
            }
        }

        if removed > 0 {
            self.term.print_line(format!(
                "removed {} filtered items from scan `{}`",
                removed, self.current_scan
            ))?;
        }

        return Ok(());

        struct State {
            values: VecDeque<(Instant, Value)>,
            changed: bool,
            removed: bool,
            last: Option<Value>,
        }

        fn refresh_loop(
            term: &mut Term,
            token: &Token,
            thread_pool: &rayon::ThreadPool,
            info_line: u16,
            error_line: u16,
            refresh_rate: Duration,
            updates: &mut [Box<ScanResult>],
            states: &mut [State],
            handle: &ProcessHandle,
            change_length: usize,
            filter: Option<&Filter>,
            incremental: bool,
            value_expr: &ValueExpr,
        ) -> anyhow::Result<()> {
            let none = Value::default();

            let pointers = updates
                .iter()
                .map(|r| r.pointer.clone())
                .collect::<Vec<_>>();

            let info = format!(
                "Refresh rate: {:?} (Press `q` or CTRL+C to stop watching)",
                refresh_rate,
            );

            if incremental {
                term.print_line(info)?;
            } else {
                term.write_on_line(info_line, info)?;
            }

            write_results(term, &pointers, states, incremental)?;

            while !token.test() {
                if let Err(e) = handle.refresh_values(
                    thread_pool,
                    updates,
                    None,
                    None,
                    NoopProgress,
                    value_expr,
                ) {
                    let m = format!("Error refreshing values: {}", e);

                    if incremental {
                        term.print_line(m)?;
                    } else {
                        term.write_on_line(error_line, m)?;
                    }
                }

                let mut any_changed = false;

                for (result, state) in updates.iter().zip(states.iter_mut()) {
                    if state.removed {
                        continue;
                    }

                    if let Some(filter) = filter {
                        let proxy = handle.address_proxy(&result.pointer);

                        if let Some(last) = &mut state.last {
                            if let Test::False = filter.test(last, proxy)? {
                                any_changed = true;
                                state.removed = true;
                            }

                            if *last != result.value {
                                *last = result.value.clone();
                            }
                        } else {
                            if let Test::False = filter.test(&none, proxy)? {
                                any_changed = true;
                                state.removed = true;
                            }

                            state.last = Some(result.value.clone());
                        }
                    }

                    if let Some(last) = state.values.back().map(|v| &v.1) {
                        if result.value != *last {
                            state.changed = true;
                            any_changed = true;

                            state
                                .values
                                .push_back((Instant::now(), result.value.clone()));

                            while state.values.len() > change_length {
                                state.values.pop_front();
                            }
                        }
                    }
                }

                if any_changed {
                    write_results(term, &pointers, states, incremental)?;
                    term.goto(0, info_line)?;
                }

                term.hide()?;
                thread::sleep(refresh_rate);
            }

            Ok(())
        }

        fn write_results(
            term: &mut Term,
            pointers: &[Pointer],
            states: &mut [State],
            incremental: bool,
        ) -> anyhow::Result<()> {
            use std::fmt::Write as _;

            for (index, (pointer, state)) in pointers.iter().zip(states.iter_mut()).enumerate() {
                if !state.changed {
                    continue;
                }

                let mut buf = String::new();

                if state.removed {
                    // TODO: color read.
                    // write!(buf, "{}", Colored::Fg(Color::Red))?;
                }

                if let Some(address) = pointer.address() {
                    write!(buf, "{:03} : {} = ", index, address)?;
                } else {
                    write!(buf, "{:03} : ? = ", index)?;
                }

                let mut it = state.values.iter();
                let last = it.next_back();
                let mut last_change = None;

                while let Some((when, v)) = it.next() {
                    if let Some(last) = last_change {
                        let duration = when.duration_since(last);
                        write!(buf, " ({:.0?}) -> ", duration)?;
                    }

                    write!(buf, "{}", v)?;
                    last_change = Some(*when);
                }

                if let Some((when, last)) = last {
                    if let Some(last) = last_change {
                        let duration = when.duration_since(last);
                        write!(buf, " ({:.0?}) -> ", duration)?;
                    }

                    write!(buf, "{}", last)?;
                }

                if state.removed {
                    write!(buf, " (filtered)")?;
                }

                if !incremental {
                    term.goto(0, index as u16)?;
                    term.clear(ClearType::CurrentLine)?;
                }

                term.print_line(buf)?;
                state.changed = false;
            }

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

            self.term.print_line(format!(
                "attached to process `{}` ({})",
                handle.name, handle.process.process_id
            ))?;

            return Ok(());
        }

        self.term.print_line(format!(
            "not attached: could not find any process matching `{}`",
            name
        ))?;

        Ok(())
    }

    /// Perform a pointer scan towards the specified address.
    fn pointer_scan(
        &mut self,
        name: String,
        needle: Address,
        append: bool,
        ty: Type,
    ) -> anyhow::Result<()> {
        use std::collections::{BTreeMap, HashSet};

        let handle = match self.handle.as_ref() {
            Some(handle) => handle,
            None => bail!("not attached to a process"),
        };

        // Filter to find all pointers.
        let pointers = Filter::pointer(Type::Pointer, ValueExpr::Value, &handle.process)?;

        let mut scan = Scan::new();

        scan.initial_scan(
            &self.thread_pool,
            handle,
            &pointers,
            None,
            SimpleProgress::new(&mut self.term, "Performing initial pointer scan", None),
            Default::default(),
        )?;

        self.term.print_line(format!(""))?;

        let mut forward = BTreeMap::new();
        let mut reverse = BTreeMap::new();

        for result in &scan.results {
            let value = result.value.as_address()?;

            if let Some(address) = result.pointer.base.eval(handle)? {
                forward.insert(address, value);
                reverse.insert(value, address);
            }
        }

        let max_depth = 7;
        let max_offset = Size::new(0x100);

        let mut results = Vec::new();

        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back((needle, 0u16, smallvec::SmallVec::<[Offset; 16]>::new()));

        while let Some((n, depth, path)) = queue.pop_front() {
            if !visited.insert(n) {
                continue;
            }

            if path.len() >= max_depth {
                continue;
            }

            let it = reverse.range(..=n).rev();

            for (from, hit) in it {
                let offset = n.offset_of(*from)?;

                if !offset.is_within(max_offset) {
                    break;
                }

                let mut path = path.clone();
                path.push(offset);

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
                            Some(*hit),
                        );

                        self.term.print_line(format!("{}", pointer))?;

                        results.push(Box::new(ScanResult {
                            pointer,
                            value: Value::None(ty),
                        }));
                    }
                    // ignore thread stacks
                    Location::Thread(..) => {
                        continue;
                    }
                    _ => (),
                }

                queue.push_back((*hit, depth + 1, path));
            }
        }

        let Self { ref mut scans, .. } = self;

        let len = results.len();

        let scan = scans.entry(name.to_string()).or_default();

        if append {
            scan.results.append(&mut results);
            self.term
                .print_line(format!("Appended {} paths to scan `{}`", len, name))?;
        } else {
            scan.results = results;
            self.term
                .print_line(format!("Saved {} paths to scan `{}`", len, name))?;
        }

        Ok(())
    }

    /// Scan memory using the given filter and the currently selected scan.
    fn scan(&mut self, filter: &filter::Filter, config: ScanConfig) -> anyhow::Result<()> {
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

        let result = if scan.initial {
            let mut c = InitialScanConfig::default();
            c.modules_only = config.modules_only;

            let result = self.term.work(|term, token| {
                scan.initial_scan(
                    thread_pool,
                    handle,
                    filter,
                    Some(token),
                    SimpleProgress::new(term, "Performing initial scan", Some(token)),
                    c,
                )
            });

            scan.initial = false;

            result
        } else {
            self.term.work(|term, token| {
                scan.scan(
                    thread_pool,
                    handle,
                    filter,
                    Some(token),
                    SimpleProgress::new(term, "Rescanning", Some(token)),
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
            &scan.value_expr,
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

        self.term.print_line(format!("Name: {}", handle.name))?;

        self.term
            .print_line(format!("Process id: {}", handle.process.process_id))?;
        self.term
            .print_line(format!("Pointer width: {}", handle.process.pointer_width))?;
        self.term
            .print_line(format!("64 bit: {}", handle.process.is_64bit))?;

        if include_modules {
            self.term.print_line(format!("Modules:"))?;

            for module in &handle.modules {
                self.term.print_line(format!(" - {:?}", module))?;
            }
        } else {
            self.term
                .print_line(format!("Modules: *only shown with --modules*"))?;
        }

        if include_threads {
            self.term.print_line(format!("Threads:"))?;

            for thread in &handle.threads {
                self.term.print_line(format!(" - {:?}", thread))?;
            }
        } else {
            self.term
                .print_line(format!("Threads: *only shown with --threads*"))?;
        }

        if include_memory {
            self.term.print_line(format!("Memory Regions:"))?;

            for region in handle.process.virtual_memory_regions() {
                let region = region?;

                let MemoryInformation {
                    range,
                    state,
                    ty,
                    protect,
                } = region;

                self.term.print_line(format!(
                    " - {ty:?} [{}-{}] {protect:?} {state:?}",
                    range.base,
                    range.base.saturating_add(range.size),
                    ty = ty,
                    protect = protect,
                    state = state,
                ))?;
            }
        } else {
            self.term
                .print_line(format!("Memory Regions: *only shown with --memory*"))?;
        }

        Ok(())
    }

    fn print_result(&mut self, result: &ScanResult) -> anyhow::Result<()> {
        self.term
            .print_line(format!("{} = {}", result.pointer, result.value))?;
        Ok(())
    }

    /// Print the current state of the scan.
    fn print<'a>(
        term: &mut Term,
        len: usize,
        results: impl IntoIterator<Item = (usize, &'a ScanResult)>,
        value_expr: &ValueExpr,
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

            write!(buf, " = {}", result.value)?;

            term.clear(ClearType::CurrentLine)?;
            term.print_line(&buf)?;
        }

        if count == 0 {
            term.clear(ClearType::CurrentLine)?;
            term.print_line("no matching addresses")?;
            return Ok(());
        }

        if count < len {
            term.clear(ClearType::CurrentLine)?;
            term.print_line(format!("... {} more omitted", len - count))?;
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
                .arg(type_argument(
                    "The type of the filter, if it can't be derived.",
                ))
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
            App::new("reset").about("Reset the current scan.").arg(
                Arg::with_name("scan")
                    .help("The scan to reset.")
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

        let app = app.subcommand(App::new("sort").about("Sort scan results."));

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
                .arg(type_argument("The type of the resulting value.")),
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

                return Ok(Action::Process {
                    include_modules,
                    include_threads,
                    include_memory,
                });
            }
            ("exit", _) => return Ok(Action::Exit),
            ("help", _) => return Ok(Action::Help),
            ("clear", _) => return Ok(Action::Clear),
            ("print", Some(m)) => {
                let limit = m.value_of("limit").map(str::parse).transpose()?;

                let expr = match m.values_of("expr") {
                    Some(values) => Some(values.collect::<Vec<_>>().join(" ")),
                    None => None,
                };

                let ty = m.value_of("type").map(str::parse).transpose()?;
                return Ok(Action::Print { limit, expr, ty });
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

                        Some(Filter::parse(&filter, None, process)?)
                    }
                    None => None,
                };

                let change_length = m
                    .value_of("length")
                    .map(str::parse)
                    .transpose()?
                    .unwrap_or(5);

                let incremental = m.is_present("incremental");

                return Ok(Action::Watch {
                    limit,
                    filter,
                    change_length,
                    incremental,
                });
            }
            ("switch", Some(m)) => {
                let name = m
                    .value_of("scan")
                    .ok_or_else(|| anyhow!("misssing <scan>"))?
                    .to_string();

                return Ok(Action::Switch { name });
            }
            ("list", _) => return Ok(Action::List),
            ("scan", Some(m)) => {
                let ty = match m.value_of("type").map(str::parse).transpose()? {
                    Some(ty) => Some(ty),
                    None => None,
                };

                let filter = match m.values_of("filter") {
                    Some(values) => values.collect::<Vec<_>>().join(" "),
                    None => String::new(),
                };

                let process = self
                    .handle
                    .as_ref()
                    .map(|h| &h.process)
                    .ok_or_else(|| anyhow!("must be attached to process"))?;

                let filter = Filter::parse(&filter, ty, process)?;

                let mut config = ScanConfig::default();
                config.modules_only = m.is_present("modules-only");
                config.suspend = m.is_present("suspend");
                return Ok(Action::Scan { filter, config });
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
                        .into_iter()
                        .map(|s| str::parse(s).map_err(Into::into))
                        .collect::<anyhow::Result<Vec<_>>>()?,
                    None => vec![],
                };

                return Ok(Action::Refresh { ty, expr, indexes });
            }
            ("attach", Some(m)) => {
                let name = m.value_of("name").map(|s| s.to_string());
                return Ok(Action::Attach(name));
            }
            ("suspend", _) => {
                return Ok(Action::Suspend);
            }
            ("resume", _) => {
                return Ok(Action::Resume);
            }
            ("reset", _) => {
                let name = m.value_of("scan").map(|s| s.to_string());

                return Ok(Action::Reset { name });
            }
            ("save", Some(m)) => {
                let file = m.value_of("file").map(PathBuf::from);
                let scan = m.value_of("scan").map(|s| s.to_string());
                let format = m.value_of("format").map(str::parse).transpose()?;

                let mut compress = None;

                if m.is_present("no-compress") {
                    compress = Some(false);
                }

                if m.is_present("compress") {
                    compress = Some(true);
                }

                return Ok(Action::Save {
                    file,
                    scan,
                    format,
                    compress,
                });
            }
            ("sort", _) => return Ok(Action::Sort),
            ("load", Some(m)) => {
                let file = m.value_of("file").map(PathBuf::from);

                let scan = m.value_of("scan").map(|s| s.to_string());
                let append = m.is_present("append");
                let format = m.value_of("format").map(str::parse).transpose()?;

                let mut compress = None;

                if m.is_present("no-compress") {
                    compress = Some(false);
                }

                if m.is_present("compress") {
                    compress = Some(true);
                }

                return Ok(Action::Load {
                    file,
                    scan,
                    append,
                    format,
                    compress,
                });
            }
            ("del", Some(m)) => {
                let index = m
                    .value_of("index")
                    .map(str::parse)
                    .transpose()?
                    .ok_or_else(|| anyhow!("missing <index>"))?;
                return Ok(Action::Del { index });
            }
            ("add", Some(m)) => {
                let ty = m.value_of("type").map(str::parse).transpose()?;

                let pointer = match m.values_of("pointer") {
                    Some(values) => values.collect::<Vec<_>>().join(" "),
                    None => String::new(),
                };

                let pointer = Pointer::parse(&pointer)?;
                return Ok(Action::Add { ty, pointer });
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

                return Ok(Action::PointerScan {
                    name,
                    address,
                    append,
                    value_type,
                });
            }
            _ => return Ok(Action::Help),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct ScanConfig {
    modules_only: bool,
    suspend: bool,
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
        Ok(match self {
            Self::Cbor => serde_cbor::to_writer(r, value)?,
            Self::Yaml => serde_yaml::to_writer(r, value)?,
        })
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
        filter: filter::Filter,
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
    },
    /// Watch changes to the scan.
    Watch {
        limit: Option<usize>,
        filter: Option<Filter>,
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
    Sort,
    Refresh {
        ty: Option<Type>,
        expr: Option<String>,
        indexes: Vec<usize>,
    },
}
