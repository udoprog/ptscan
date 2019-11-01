#![feature(backtrace)]

use anyhow::{anyhow, bail, Context as _};
use hashbrown::HashMap;
use ptscan::{
    filter, scan, Address, Filter, Location, MemoryInformation, Offset, Pointer, PointerBase,
    ProcessHandle, ProcessId, ScanResult, Size, Token, Type, Value, ValueExpr,
};
use serde::{Deserialize, Serialize};
use std::{fs::File, io, path::PathBuf, sync::Arc, time::Instant};

use crossterm::{
    ClearType, Color, Colored, Crossterm, InputEvent, KeyEvent, RawScreen, Terminal,
    TerminalCursor, TerminalInput,
};

static DEFAULT_LIMIT: usize = 10;

fn try_main() -> anyhow::Result<()> {
    let opts = ptscan::opts::opts();
    let thread_pool = Arc::new(
        rayon::ThreadPoolBuilder::new()
            .num_threads(num_cpus::get())
            .build()?,
    );

    let crossterm = Crossterm::new();
    let terminal = crossterm.terminal();
    let input = crossterm.input();
    let cursor = crossterm.cursor();

    let term = Term {
        terminal,
        input,
        cursor,
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

impl scan::Progress for NoopProgress {
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
}

impl<'a> SimpleProgress<'a> {
    fn new(term: &'a mut Term, what: &'static str) -> SimpleProgress<'a> {
        Self { term, what }
    }
}

impl<'a> scan::Progress for SimpleProgress<'a> {
    fn report_bytes(&mut self, _: Size) -> anyhow::Result<()> {
        // NB: do nothing
        Ok(())
    }

    fn report(&mut self, percentage: usize, count: u64) -> anyhow::Result<()> {
        use std::iter;

        self.term.cursor.save_position()?;
        self.term.terminal.clear(ClearType::CurrentLine)?;

        let repr = iter::repeat('#').take(percentage / 10).collect::<String>();

        self.term.terminal.write(format!(
            "{}: {}% ({} results): {}",
            repr, percentage, count, self.what
        ))?;

        self.term.cursor.restore_position()?;
        Ok(())
    }
}

/// Very simple terminal wrapper around crossterm.
struct Term {
    terminal: Terminal,
    input: TerminalInput,
    cursor: TerminalCursor,
}

impl Term {
    /// Write a single line to the terminal.
    fn write_line(&mut self, line: impl std::fmt::Display) -> anyhow::Result<()> {
        println!("{}", line);
        Ok(())
    }

    /// Perform an panicky write to the specified line, indicating that an error happened.
    fn write_on_line(&mut self, row: u16, line: impl std::fmt::Display) -> anyhow::Result<()> {
        self.cursor.goto(0, row)?;
        self.write_line(line)?;
        Ok(())
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
    scans: HashMap<String, scan::Scan>,
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
            self.term.terminal.write("ptscan> ")?;
            let line = self.term.input.read_line()?;

            let it = std::iter::once(String::from("ptscan")).chain(Words::new(&line));

            let m = match app.get_matches_from_safe_borrow(it) {
                Ok(m) => m,
                Err(e) => {
                    self.term.write_line(format!("{}", e.message))?;
                    continue;
                }
            };

            let action = match self.line_into_action(&m) {
                Ok(action) => action,
                Err(e) => {
                    self.term.write_line(format!("{}", e))?;
                    continue;
                }
            };

            match self.apply_action(&mut app, action) {
                Ok(true) => return Ok(()),
                Ok(false) => {}
                Err(e) => {
                    self.term.write_line(format!("{}", e))?;

                    for cause in e.chain().skip(1) {
                        self.term.write_line(format!("caused by: {}", cause))?;

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
            Action::ScanList => {
                if self.scans.is_empty() {
                    self.term.write_line("No scans")?;
                    return Ok(false);
                }

                self.term.write_line(format!("Scans:"))?;

                for (key, s) in &self.scans {
                    self.term
                        .write_line(format!(" - `{}` with {} result(s)", key, s.len()))?;
                }
            }
            Action::ScanNew { name } => {
                if self.scans.contains_key(&name) {
                    bail!("there is already a scan named `{}`", name);
                }

                self.scans
                    .insert(name.clone(), scan::Scan::new(&self.thread_pool));
                self.current_scan = name;
            }
            Action::ScanDel { name } => {
                if self.scans.remove(&name).is_none() {
                    bail!("no such scan `{}`", name);
                }

                self.term.write_line(format!("removed scan `{}`", name))?;
            }
            Action::ScanSwitch { name } => {
                self.term.write_line(format!("Switch to scan `{}`", name))?;
                self.current_scan = name;
            }
            Action::Exit => {
                return Ok(true);
            }
            Action::Help => {
                let mut out = Vec::new();
                app.write_long_help(&mut out)?;
                self.term.write_line(String::from_utf8(out)?)?;
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
            Action::Print { limit } => {
                if let Some(limit) = limit {
                    self.limit = limit;
                }

                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => bail!("No active scan"),
                };

                let mut stored;
                let mut results = &scan[..usize::min(scan.len(), self.limit)];

                if let Some(handle) = self.handle.as_ref() {
                    stored = results.to_vec();
                    handle.refresh_values(
                        &self.thread_pool,
                        &mut stored,
                        None,
                        None,
                        NoopProgress,
                    )?;
                    results = &stored;
                }

                self.term
                    .write_line(format!("Scan: {}", self.current_scan))?;

                Self::print(
                    &mut self.term,
                    scan.len(),
                    results.iter().map(|r| &**r).enumerate(),
                )?;
            }
            Action::Watch { limit } => {
                if let Some(limit) = limit {
                    self.limit = limit;
                }

                self.watch()?;
            }
            Action::Del { index } => {
                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => bail!("No active scan"),
                };

                if index < scan.results.len() {
                    let result = scan.results.swap_remove(index);
                    self.term.write_line("deleted:")?;
                    self.print_result(&*result)?;
                }
            }
            Action::Add { mut pointer, ty } => {
                let Application {
                    ref mut scans,
                    ref thread_pool,
                    ref current_scan,
                    ..
                } = self;

                let scan = scans
                    .entry(current_scan.clone())
                    .or_insert_with(|| scan::Scan::new(thread_pool));

                if let Some(ty) = ty {
                    scan.last_type = Some(ty);
                }

                let ty = scan.last_type.unwrap_or(Type::U32);

                let value = match self.handle.as_ref() {
                    Some(handle) => handle.address_proxy(&pointer).eval(ty)?,
                    None => Value::None(ty),
                };

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
            }
            Action::Scan { filter, config } => {
                self.scan(&filter, None, config)?;
            }
            Action::Refresh { new_type } => {
                let handle = match self.handle.as_ref() {
                    Some(handle) => handle,
                    None => bail!("not attached to a process"),
                };

                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => bail!("no active scan to refresh!"),
                };

                handle.refresh_values(
                    &self.thread_pool,
                    &mut scan.results,
                    None,
                    new_type,
                    SimpleProgress::new(&mut self.term, "Refreshing values"),
                )?;

                if let Some(new_type) = new_type {
                    scan.last_type = Some(new_type);
                }

                println!("");

                let len = usize::min(scan.len(), self.limit);
                Self::print(
                    &mut self.term,
                    scan.len(),
                    scan.results.iter().take(len).map(|r| &**r).enumerate(),
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
            Action::Reset => match self.scans.get_mut(&self.current_scan) {
                Some(scan) => {
                    scan.clear();
                    scan.initial = true;
                    scan.last_type = None;
                }
                None => {
                    self.term.write_line(format!("no scan in use"))?;
                }
            },
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
            Action::Set(pointer, value) => {
                let handle = match self.handle.as_ref() {
                    Some(handle) => handle,
                    None => bail!("not attached to a process"),
                };

                let address = pointer.follow_default(handle)?;

                if let Some(address) = address {
                    let mut buf = vec![0u8; value.size(&handle.process)];
                    value.encode(&handle.process, &mut buf)?;
                    handle.process.write_process_memory(address, &buf)?;
                } else {
                    self.term
                        .write_line(format!("Invalid address to write to"))?;
                }
            }
        }

        Ok(false)
    }

    fn save(&mut self, scan: Option<&str>) -> anyhow::Result<()> {
        use flate2::{write::GzEncoder, Compression};

        let Self {
            ref thread_pool,
            ref mut scans,
            ref current_file,
            ..
        } = self;

        let scan = scan.unwrap_or(self.current_scan.as_str());

        let count = {
            let scan = scans
                .entry(scan.to_string())
                .or_insert_with(|| scan::Scan::new(thread_pool));

            let serialized = SerializedScan {
                results: &scan.results,
                last_type: scan.last_type,
            };

            let mut f = File::create(current_file)
                .with_context(|| anyhow!("failed to create file `{}`", current_file.display()))?;

            if self.current_compress {
                let mut f = GzEncoder::new(&mut f, Compression::default());
                self.current_format.serialize(&mut f, &serialized)?;
                f.finish()?;
            } else {
                self.current_format.serialize(&mut f, &serialized)?
            };

            scan.results.len()
        };

        self.term.write_line(format!(
            "Saved {} results from scan `{}` to file `{}`",
            count,
            scan,
            current_file.display()
        ))?;

        return Ok(());

        #[derive(Serialize)]
        struct SerializedScan<'a> {
            results: &'a [Box<ScanResult>],
            #[serde(default, skip_serializing_if = "Option::is_none")]
            last_type: Option<Type>,
        }
    }

    fn load(&mut self, scan: Option<&str>, append: bool) -> anyhow::Result<()> {
        use flate2::read::GzDecoder;

        let Self {
            ref thread_pool,
            ref mut scans,
            ref current_file,
            ..
        } = self;

        let scan = scan.unwrap_or(self.current_scan.as_str());

        {
            let mut f = File::open(current_file)
                .with_context(|| anyhow!("Failed to open file `{}`", current_file.display()))?;

            let deserialized: DeserializedScan = if self.current_compress {
                self.current_format
                    .deserialize(&mut GzDecoder::new(&mut f))?
            } else {
                self.current_format.deserialize(&mut f)?
            };

            let scan = scans
                .entry(scan.to_string())
                .or_insert_with(|| scan::Scan::new(thread_pool));

            if append {
                scan.results.extend(deserialized.results);
            } else {
                scan.results = deserialized.results;
            }

            scan.last_type = deserialized.last_type;
            scan.initial = false;
        }

        self.term.write_line(format!(
            "Loaded scan `{}` from file `{}`",
            scan,
            current_file.display()
        ))?;

        return Ok(());

        #[derive(Deserialize)]
        struct DeserializedScan {
            results: Vec<Box<ScanResult>>,
            #[serde(default, skip_serializing_if = "Option::is_none")]
            last_type: Option<Type>,
        }
    }

    fn watch(&mut self) -> anyhow::Result<()> {
        use std::{
            sync::atomic::{AtomicBool, Ordering},
            thread,
            time::Duration,
        };

        let _raw = RawScreen::into_raw_mode()?;

        let thread_pool = &self.thread_pool;

        let handle = match self.handle.as_ref() {
            Some(handle) => handle,
            None => bail!("not attached to a process"),
        };

        let scan = self
            .scans
            .entry(self.current_scan.to_string())
            .or_insert_with(|| scan::Scan::new(thread_pool));

        if scan.results.is_empty() {
            self.term.write_line("cannot watch, scan is empty")?;
            return Ok(());
        }

        let refresh_rate = Duration::from_millis(500);
        let results = &scan[..usize::min(scan.len(), self.limit)];
        let mut updates = results.to_vec();

        self.term.cursor.hide()?;
        self.term.terminal.clear(ClearType::All)?;

        let mut stdin = self.term.input.read_sync();

        let line_below = results.len() as u16;
        let info_line = line_below + 1;
        let error_line = line_below + 2;

        write_results(&mut self.term, &results)?;

        // NB: do a little borrow dance to support the scoped thread with split borrows.
        let exit = AtomicBool::new(false);
        let exit = &exit;
        let term = &mut self.term;

        rayon::scope(|s| {
            s.spawn(move |_| {
                refresh_loop(
                    &*thread_pool,
                    info_line,
                    error_line,
                    refresh_rate,
                    &exit,
                    term,
                    &mut updates,
                    &handle,
                )
                .expect("refresh loop panicked");
            });

            loop {
                if let Some(event) = stdin.next() {
                    match event {
                        InputEvent::Keyboard(KeyEvent::Char(c)) => match c {
                            'q' => break,
                            _ => (),
                        },
                        InputEvent::Keyboard(KeyEvent::Ctrl('c')) => break,
                        _ => (),
                    }
                }
            }

            exit.store(true, Ordering::Release);
        });

        self.term.cursor.goto(0, info_line)?;
        self.term.terminal.clear(ClearType::CurrentLine)?;
        self.term.cursor.show()?;
        return Ok(());

        fn refresh_loop(
            thread_pool: &rayon::ThreadPool,
            info_line: u16,
            error_line: u16,
            refresh_rate: Duration,
            exit: &AtomicBool,
            term: &mut Term,
            updates: &mut [Box<ScanResult>],
            handle: &ProcessHandle,
        ) -> anyhow::Result<()> {
            let mut info_changed = true;

            let pointers = updates
                .iter()
                .map(|r| r.pointer.clone())
                .collect::<Vec<_>>();

            let mut values = updates
                .iter()
                .map(|r| (vec![(Instant::now(), r.value.clone())], false))
                .collect::<Vec<_>>();

            while !exit.load(Ordering::Acquire) {
                if info_changed {
                    term.write_on_line(
                        info_line,
                        format!(
                            "Refresh rate: {:?} (Press `q` or CTRL+C to stop watching)",
                            refresh_rate,
                        ),
                    )?;

                    info_changed = false;
                }

                if let Err(e) =
                    handle.refresh_values(thread_pool, updates, None, None, NoopProgress)
                {
                    term.write_on_line(error_line, format!("Error refreshing values: {}", e))?;
                }

                let mut any = false;

                for (update, (values, changed)) in updates.iter().zip(values.iter_mut()) {
                    if update.value != values[values.len() - 1].1 {
                        *changed = true;
                        any = true;
                        values.push((Instant::now(), update.value.clone()));
                    }
                }

                if any {
                    write_updates(term, &pointers, &mut values)?;
                }

                term.cursor.hide()?;
                thread::sleep(refresh_rate);
            }

            Ok(())
        }

        fn write_results(term: &mut Term, current: &[Box<ScanResult>]) -> anyhow::Result<()> {
            for (row, result) in current.iter().enumerate() {
                term.cursor.goto(0, row as u16)?;
                term.terminal.clear(ClearType::CurrentLine)?;
                term.write_line(format!("{} = {}", result.pointer, result.value))?;
            }

            Ok(())
        }

        fn write_updates(
            term: &mut Term,
            pointers: &[Pointer],
            updates: &mut [(Vec<(Instant, Value)>, bool)],
        ) -> anyhow::Result<()> {
            use std::fmt::Write as _;

            for (row, (pointer, (values, changed))) in
                pointers.iter().zip(updates.iter_mut()).enumerate()
            {
                if !*changed {
                    continue;
                }

                term.cursor.goto(0, row as u16)?;
                term.terminal.clear(ClearType::CurrentLine)?;

                let mut buf = String::new();

                write!(buf, "{} = ", pointer)?;

                let mut it = values.iter();

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

                    write!(
                        buf,
                        "{}{}{}",
                        Colored::Fg(Color::Red),
                        last,
                        Colored::Fg(Color::Reset)
                    )?;
                }

                term.write_line(buf)?;
                *changed = false;
            }

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

            self.term.write_line(format!(
                "attached to process `{}` ({})",
                handle.name, handle.process.process_id
            ))?;

            return Ok(());
        }

        self.term.write_line(format!(
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
        use std::collections::{BTreeMap, HashSet, VecDeque};

        let handle = match self.handle.as_ref() {
            Some(handle) => handle,
            None => bail!("not attached to a process"),
        };

        // Filter to find all pointers.
        let pointers = Filter::pointer(Type::Pointer, ValueExpr::Value, &handle.process)?;

        let mut scan = scan::Scan::new(&self.thread_pool);

        scan.initial_scan(
            handle,
            &pointers,
            None,
            SimpleProgress::new(&mut self.term, "Performing initial pointer scan"),
            Default::default(),
        )?;

        self.term.write_line(format!(""))?;

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

                        self.term.write_line(format!("{}", pointer))?;

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

        let Self {
            ref mut scans,
            ref thread_pool,
            ..
        } = self;

        let len = results.len();

        let scan = scans
            .entry(name.to_string())
            .or_insert_with(|| scan::Scan::new(thread_pool));

        scan.last_type = Some(Type::Pointer);

        if append {
            scan.results.extend(results);
            self.term
                .write_line(format!("Appended {} paths to scan `{}`", len, name))?;
        } else {
            scan.results = results;
            self.term
                .write_line(format!("Saved {} paths to scan `{}`", len, name))?;
        }

        Ok(())
    }

    /// Scan memory using the given filter and the currently selected scan.
    fn scan(
        &mut self,
        filter: &filter::Filter,
        cancel: Option<&Token>,
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
            ref thread_pool,
            ref current_scan,
            ..
        } = self;

        let scan = self
            .scans
            .entry(current_scan.to_string())
            .or_insert_with(|| scan::Scan::new(thread_pool));

        let result = if scan.initial {
            let mut c = scan::InitialScanConfig::default();
            c.modules_only = config.modules_only;

            let result = scan.initial_scan(
                handle,
                filter,
                cancel,
                SimpleProgress::new(&mut self.term, "Performing initial scan"),
                c,
            );

            scan.initial = false;

            result
        } else {
            scan.scan(
                handle,
                filter,
                cancel,
                SimpleProgress::new(&mut self.term, "Rescanning"),
            )
        };

        if self.suspend || config.suspend {
            handle.process.resume()?;
        }

        println!("");
        result?;

        let len = usize::min(scan.len(), self.limit);
        Self::print(
            &mut self.term,
            scan.len(),
            scan[..len].iter().map(|r| &**r).enumerate(),
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

        self.term.write_line(format!("Name: {}", handle.name))?;

        self.term
            .write_line(format!("Process id: {}", handle.process.process_id))?;
        self.term
            .write_line(format!("Pointer width: {}", handle.process.pointer_width))?;
        self.term
            .write_line(format!("64 bit: {}", handle.process.is_64bit))?;

        if include_modules {
            self.term.write_line(format!("Modules:"))?;

            for module in &handle.modules {
                self.term.write_line(format!(" - {:?}", module))?;
            }
        } else {
            self.term
                .write_line(format!("Modules: *only shown with --modules*"))?;
        }

        if include_threads {
            self.term.write_line(format!("Threads:"))?;

            for thread in &handle.threads {
                self.term.write_line(format!(" - {:?}", thread))?;
            }
        } else {
            self.term
                .write_line(format!("Threads: *only shown with --threads*"))?;
        }

        if include_memory {
            self.term.write_line(format!("Memory Regions:"))?;

            for region in handle.process.virtual_memory_regions() {
                let region = region?;

                let MemoryInformation {
                    range,
                    state,
                    ty,
                    protect,
                } = region;

                self.term.write_line(format!(
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
                .write_line(format!("Memory Regions: *only shown with --memory*"))?;
        }

        Ok(())
    }

    fn print_result(&mut self, result: &ScanResult) -> anyhow::Result<()> {
        self.term
            .write_line(format!("{} = {}", result.pointer, result.value))?;
        Ok(())
    }

    /// Print the current state of the scan.
    fn print<'a>(
        term: &mut Term,
        len: usize,
        results: impl IntoIterator<Item = (usize, &'a ScanResult)>,
    ) -> anyhow::Result<()> {
        let mut count = 0;

        for (index, result) in results {
            count += 1;
            term.write_line(format!(
                "{:>03} : {} = {}",
                index, result.pointer, result.value
            ))?;
        }

        if count < len {
            term.write_line(format!("... {} more omitted", len - count))?;
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
                .about("Refresh values in a scan, optionally setting a new type for them.")
                .arg(type_argument(
                    "The type of the filter, if it can't be derived.",
                )),
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

        let app = app.subcommand(App::new("exit").about("Exit the application"));

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
                ),
        );

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
                ),
        );

        let app = app.subcommand(App::new("help").about("Print help"));

        let app = app.subcommand(App::new("reset").about("Reset the current scan."));

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
            ("print", Some(m)) => {
                let limit = m.value_of("limit").map(str::parse).transpose()?;
                return Ok(Action::Print { limit });
            }
            ("watch", Some(m)) => {
                let limit = m.value_of("limit").map(str::parse).transpose()?;
                return Ok(Action::Watch { limit });
            }
            ("switch", Some(m)) => {
                let name = m
                    .value_of("scan")
                    .ok_or_else(|| anyhow!("misssing <scan>"))?
                    .to_string();

                return Ok(Action::ScanSwitch { name });
            }
            ("list", _) => return Ok(Action::ScanList),
            ("scan", Some(m)) => {
                if let Some(name) = m.value_of("new") {
                    return Ok(Action::ScanNew {
                        name: name.to_string(),
                    });
                }

                if m.is_present("del") {
                    return Ok(Action::ScanDel {
                        name: self.current_scan.to_string(),
                    });
                }

                if let Some(name) = m.value_of("del") {
                    return Ok(Action::ScanDel {
                        name: name.to_string(),
                    });
                }

                let ty = match m.value_of("type").map(str::parse).transpose()? {
                    Some(ty) => Some(ty),
                    None => None,
                };

                let filter = match m.values_of("filter") {
                    Some(values) => values.collect::<Vec<_>>().join(" "),
                    None => String::new(),
                };

                let process = self.handle.as_ref().map(|h| &h.process);
                let matcher = filter::parse_matcher(&filter, process)?;

                let last_type = self.scans.get(&self.current_scan).and_then(|s| s.last_type);

                let ty = ty
                    .or_else(|| matcher.type_hint())
                    .or(last_type)
                    .ok_or_else(|| anyhow!("cannot determine type of filter"))?;

                let filter = filter::Filter::new(ty, matcher);

                let mut config = ScanConfig::default();
                config.modules_only = m.is_present("modules-only");
                config.suspend = m.is_present("suspend");
                return Ok(Action::Scan { filter, config });
            }

            ("refresh", Some(m)) => {
                let new_type = match m.value_of("type").map(str::parse).transpose()? {
                    Some(ty) => Some(ty),
                    None => None,
                };

                return Ok(Action::Refresh { new_type });
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
                return Ok(Action::Reset);
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
    Reset,
    /// Print results from scan.
    Print {
        limit: Option<usize>,
    },
    /// Watch changes to the scan.
    Watch {
        limit: Option<usize>,
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
    /// Write the value at the given address.
    Set(Pointer, Value),
    /// List all scans.
    ScanList,
    /// Create a new scan with the given name.
    ScanNew {
        name: String,
    },
    /// Delete the scan with the given name.
    ScanDel {
        name: String,
    },
    /// Switch to the given scan.
    ScanSwitch {
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
        new_type: Option<Type>,
    },
}
