#![feature(backtrace)]

use anyhow::{anyhow, bail, Context as _};
use hashbrown::HashMap;
use ptscan::{
    filter, scan, Address, Filter, Location, MemoryInformation, Offset, Pointer, PointerBase,
    Process, ProcessHandle, ScanResult, Size, Token, Type, Value, ValueExpr,
};
use serde::{Deserialize, Serialize};
use std::{fs::File, io, path::PathBuf, sync::Arc, time::Instant};

use crossterm::{
    ClearType, Color, Colored, Crossterm, InputEvent, KeyEvent, Terminal, TerminalCursor,
};

static HELP: &'static str = include_str!("help.md");
static DEFAULT_LIMIT: usize = 10;

fn try_main() -> anyhow::Result<()> {
    let opts = ptscan::opts::opts();
    let thread_pool = Arc::new(rayon::ThreadPoolBuilder::new().build()?);

    let mut app = Application::new(io::stdin(), io::stdout(), thread_pool, opts.suspend);
    app.process_name = opts.process.clone();

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

struct SimpleProgress<W> {
    out: W,
    what: &'static str,
}

impl<W> SimpleProgress<W> {
    fn new(out: W, what: &'static str) -> SimpleProgress<W> {
        Self { out, what }
    }
}

impl<W> scan::Progress for SimpleProgress<W>
where
    W: io::Write,
{
    fn report_bytes(&mut self, _: Size) -> anyhow::Result<()> {
        // NB: do nothing
        Ok(())
    }

    fn report(&mut self, percentage: usize, count: u64) -> anyhow::Result<()> {
        use std::iter;

        write!(self.out, "\r")?;
        self.out.flush()?;

        let repr = iter::repeat('#').take(percentage / 10).collect::<String>();
        write!(
            self.out,
            "{}: {}% ({} results): {}",
            repr, percentage, count, self.what
        )?;
        self.out.flush()?;

        Ok(())
    }
}

pub struct Application<R, W> {
    r: R,
    w: W,
    thread_pool: Arc<rayon::ThreadPool>,
    suspend: bool,
    /// Process name that we are attached to.
    process_name: Option<String>,
    /// Process that we are currently attached to.
    handle: Option<ProcessHandle>,
    scans: HashMap<String, scan::Scan>,
    current_scan: String,
    current_file: Option<PathBuf>,
    current_format: FileFormat,
    /// If the save file should be compressed or not.
    current_compress: bool,
}

impl<R, W> Application<R, W>
where
    R: io::Read,
    W: Send + io::Write,
{
    /// Create a new application with the given options.
    pub fn new(
        r: R,
        w: W,
        thread_pool: Arc<rayon::ThreadPool>,
        suspend: bool,
    ) -> Application<R, W> {
        Application {
            r,
            w,
            thread_pool,
            suspend,
            process_name: None,
            handle: None,
            scans: HashMap::new(),
            current_scan: String::from("default"),
            current_file: Some(PathBuf::from("state")),
            current_format: FileFormat::Cbor,
            current_compress: true,
        }
    }

    /// Run the application in a loop.
    pub fn run(&mut self) -> anyhow::Result<()> {
        use ptscan::utils::Words;
        let mut app = Self::app();

        loop {
            write!(self.w, "ptscan> ")?;
            self.w.flush()?;

            let line = self.line()?;

            let it = std::iter::once(String::from("ptscan")).chain(Words::new(&line));

            let m = match app.get_matches_from_safe_borrow(it) {
                Ok(m) => m,
                Err(e) => {
                    writeln!(self.w, "{}", e.message)?;
                    continue;
                }
            };

            let action = match self.line_into_action(&m) {
                Ok(action) => action,
                Err(e) => {
                    writeln!(self.w, "{}", e)?;
                    continue;
                }
            };

            match self.apply_action(&mut app, action) {
                Ok(true) => return Ok(()),
                Ok(false) => {}
                Err(e) => {
                    writeln!(self.w, "{}", e)?;

                    for cause in e.chain().skip(1) {
                        writeln!(self.w, "caused by: {}", cause)?;

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
                writeln!(self.w, "Scans:")?;

                for (key, s) in &self.scans {
                    writeln!(self.w, "{} - {} result(s)", key, s.len())?;
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

                writeln!(self.w, "removed scan `{}`", name)?;
            }
            Action::ScanSwitch { name } => {
                self.current_scan = name;
            }
            Action::Exit => {
                return Ok(true);
            }
            Action::Help => {
                app.write_long_help(&mut self.w)?;
                writeln!(self.w, "")?;
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
                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => {
                        writeln!(self.w, "no scan in use")?;
                        return Ok(false);
                    }
                };

                let limit = limit.unwrap_or(DEFAULT_LIMIT);
                let mut stored;
                let mut results = &scan[..usize::min(scan.len(), limit)];

                if let Some(handle) = self.handle.as_ref() {
                    stored = results.to_vec();

                    handle.refresh_values(&self.thread_pool, &mut stored, None, NoopProgress)?;

                    results = &stored;
                    println!("");
                }

                Self::print(&mut self.w, scan.len(), results)?;
            }
            Action::Watch { limit } => {
                self.watch(limit)?;
            }
            Action::Del { pointer } => {
                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => bail!("no active scan"),
                };

                scan.remove_by_pointer(&pointer);
            }
            Action::Add { pointer, ty } => {
                let Application {
                    ref mut scans,
                    ref thread_pool,
                    ref current_scan,
                    ..
                } = self;

                let scan = scans
                    .entry(current_scan.clone())
                    .or_insert_with(|| scan::Scan::new(thread_pool));

                let value = match self.handle.as_ref() {
                    Some(handle) => handle.address_proxy(&pointer).eval(ty)?,
                    None => Value::None(ty),
                };

                scan.push(Box::new(scan::ScanResult { pointer, value }));
            }
            Action::Scan { filter, config } => {
                self.scan(&filter, None, config)?;
            }
            Action::PointerScan {
                name,
                address,
                value_type,
            } => {
                let value_type = value_type.unwrap_or(Type::None);
                self.pointer_scan(name, address, value_type)?;
            }
            Action::Reset => match self.scans.get_mut(&self.current_scan) {
                Some(scan) => {
                    scan.clear();
                    scan.initial = true;
                    scan.last_type = None;
                }
                None => writeln!(self.w, "no scan in use")?,
            },
            Action::Save {
                file,
                scan,
                format,
                compress,
                ..
            } => {
                use flate2::{write::GzEncoder, Compression};

                let file = file
                    .as_ref()
                    .or_else(|| self.current_file.as_ref())
                    .ok_or_else(|| anyhow!("no file open, you need to specify <file>"))?
                    .to_owned();

                let scan = scan
                    .as_ref()
                    .map(String::as_str)
                    .unwrap_or(self.current_scan.as_str());

                let format = format.unwrap_or(self.current_format);
                let compress = compress.unwrap_or(self.current_compress);

                let Self {
                    ref thread_pool,
                    ref mut scans,
                    ..
                } = self;

                let count = {
                    let scan = scans
                        .entry(scan.to_string())
                        .or_insert_with(|| scan::Scan::new(thread_pool));

                    let serialized = SerializedScan {
                        results: &scan.results,
                        last_type: scan.last_type,
                    };

                    let mut f = File::create(&file)
                        .with_context(|| anyhow!("failed to create file `{}`", file.display()))?;

                    if compress {
                        let mut f = GzEncoder::new(&mut f, Compression::default());
                        format.serialize(&mut f, &serialized)?;
                        f.finish()?;
                    } else {
                        format.serialize(&mut f, &serialized)?
                    };

                    scan.results.len()
                };

                writeln!(
                    self.w,
                    "Saved {} results from scan `{}` to file `{}`",
                    count,
                    scan,
                    file.display()
                )?;

                self.current_file = Some(file);
                self.current_format = format;
                self.current_compress = compress;

                #[derive(Serialize)]
                struct SerializedScan<'a> {
                    results: &'a [Box<ScanResult>],
                    #[serde(default, skip_serializing_if = "Option::is_none")]
                    last_type: Option<Type>,
                }
            }
            Action::Load {
                file,
                scan,
                append,
                format,
                compress,
                ..
            } => {
                use flate2::read::GzDecoder;

                let file = file
                    .as_ref()
                    .or_else(|| self.current_file.as_ref())
                    .ok_or_else(|| anyhow!("no file open, you need to specify <file>"))?
                    .to_owned();

                let scan = scan
                    .as_ref()
                    .map(String::as_str)
                    .unwrap_or(self.current_scan.as_str());

                let format = format.unwrap_or(self.current_format);
                let compress = compress.unwrap_or(self.current_compress);

                let Self {
                    ref thread_pool,
                    ref mut scans,
                    ..
                } = self;

                {
                    let mut f = File::open(&file)
                        .with_context(|| anyhow!("Failed to open file `{}`", file.display()))?;

                    let deserialized: DeserializedScan = if compress {
                        format.deserialize(&mut GzDecoder::new(&mut f))?
                    } else {
                        format.deserialize(&mut f)?
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

                writeln!(
                    self.w,
                    "Loaded scan `{}` from file `{}`",
                    scan,
                    file.display()
                )?;

                self.current_file = Some(file);
                self.current_format = format;
                self.current_compress = compress;

                #[derive(Deserialize)]
                struct DeserializedScan {
                    results: Vec<Box<ScanResult>>,
                    #[serde(default, skip_serializing_if = "Option::is_none")]
                    last_type: Option<Type>,
                }
            }
            Action::Sort => {
                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => bail!("no active scan"),
                };

                scan.results.sort_by(|a, b| a.pointer.cmp(&b.pointer));
            }
            Action::Set(pointer, value) => {
                let handle = match self.handle.as_ref() {
                    Some(handle) => handle,
                    None => bail!("not attached to a process"),
                };

                let address = pointer.follow(handle, |a, buf| {
                    handle
                        .process
                        .read_process_memory(a, buf)
                        .map_err(Into::into)
                })?;

                if let Some(address) = address {
                    let mut buf = vec![0u8; value.size(&handle.process)];
                    value.encode(&handle.process, &mut buf)?;
                    handle.process.write_process_memory(address, &buf)?;
                } else {
                    writeln!(self.w, "Invalid address to write to")?;
                }
            }
        }

        Ok(false)
    }

    fn watch(&mut self, limit: Option<usize>) -> anyhow::Result<()> {
        use std::{thread, time::Duration};

        let thread_pool = &self.thread_pool;

        let handle = match self.handle.as_ref() {
            Some(handle) => handle,
            None => bail!("not attached to a process"),
        };

        let scan = self
            .scans
            .entry(self.current_scan.to_string())
            .or_insert_with(|| scan::Scan::new(thread_pool));

        let crossterm = Crossterm::new();
        let terminal = crossterm.terminal();
        let cursor = crossterm.cursor();
        let input = crossterm.input();

        let refresh_rate = Duration::from_millis(10);
        let limit = limit.unwrap_or(DEFAULT_LIMIT);
        let results = &scan[..usize::min(scan.len(), limit)];
        let mut updates = results.to_vec();

        let pointers = updates
            .iter()
            .map(|r| r.pointer.clone())
            .collect::<Vec<_>>();

        let mut values = updates
            .iter()
            .map(|r| (vec![(Instant::now(), r.value.clone())], false))
            .collect::<Vec<_>>();

        cursor.hide()?;
        terminal.clear(ClearType::All)?;

        let mut stdin = input.read_async();

        cursor.goto(0, (results.len() as u16) + 1)?;
        terminal.write(format!(
            "Refresh rate: {:?} (Press `q` to stop watching)",
            refresh_rate,
        ))?;
        write_results(&cursor, &terminal, &results)?;

        loop {
            if let Some(key_event) = stdin.next() {
                if process_input_event(key_event) {
                    break;
                }
            }

            thread::sleep(refresh_rate);
            handle.refresh_values(&self.thread_pool, &mut updates, None, NoopProgress)?;

            let mut any = false;

            for (update, (values, changed)) in updates.iter().zip(values.iter_mut()) {
                if update.value != values[values.len() - 1].1 {
                    *changed = true;
                    any = true;
                    values.push((Instant::now(), update.value.clone()));
                }
            }

            if any {
                write_updates(&cursor, &terminal, &pointers, &mut values)?;
            }
        }

        cursor.show()?;
        terminal.clear(ClearType::All)?;
        return Ok(());

        fn write_results(
            cursor: &TerminalCursor,
            terminal: &Terminal,
            current: &[Box<ScanResult>],
        ) -> anyhow::Result<()> {
            for (row, result) in current.iter().enumerate() {
                cursor.goto(0, row as u16)?;
                terminal.clear(ClearType::CurrentLine)?;
                terminal.write(format!("{} = {}", result.pointer, result.value))?;
            }

            Ok(())
        }

        fn write_updates(
            cursor: &TerminalCursor,
            terminal: &Terminal,
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

                cursor.goto(0, row as u16)?;
                terminal.clear(ClearType::CurrentLine)?;

                let mut buf = String::new();

                write!(buf, "{} = ", pointer)?;

                let mut it = values.iter();

                let last = it.next_back();

                let mut last_change = None;

                while let Some((when, v)) = it.next() {
                    if let Some(last) = last_change {
                        let duration = when.duration_since(last);
                        write!(buf, "({:.3?}) ", duration)?;
                    }

                    write!(buf, "{} -> ", v)?;
                    last_change = Some(*when);
                }

                if let Some((when, last)) = last {
                    if let Some(last) = last_change {
                        let duration = when.duration_since(last);
                        write!(buf, "({:.3?}) ", duration)?;
                    }

                    write!(
                        buf,
                        "{}{}{}",
                        Colored::Fg(Color::Red),
                        last,
                        Colored::Fg(Color::Reset)
                    )?;
                }

                terminal.write(buf)?;
                *changed = false;
            }

            Ok(())
        }

        fn process_input_event(event: InputEvent) -> bool {
            println!("got event: {:?}", event);

            match event {
                InputEvent::Keyboard(KeyEvent::Char('q')) => true,
                _ => false,
            }
        }
    }

    /// Try to attach to some process.
    fn attach(&mut self) -> anyhow::Result<()> {
        let name = match self.process_name.as_ref() {
            Some(name) => name,
            None => bail!("no process name to attach to"),
        };

        self.handle = ProcessHandle::open_by_name(name)?;

        if let Some(handle) = self.handle.as_mut() {
            handle.refresh_modules()?;
            handle.refresh_threads()?;
            return Ok(());
        }

        writeln!(
            self.w,
            "not attached: could not find any process matching `{}`",
            name
        )?;
        Ok(())
    }

    /// Perform a pointer scan towards the specified address.
    fn pointer_scan(&mut self, name: String, needle: Address, ty: Type) -> anyhow::Result<()> {
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
            SimpleProgress::new(&mut self.w, "Performing initial pointer scan"),
            Default::default(),
        )?;

        writeln!(self.w, "")?;

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
                        );

                        writeln!(self.w, "{}", pointer)?;

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

        writeln!(self.w, "Saved {} paths to scan `{}`", results.len(), name)?;

        let scan = scan::Scan::from_results(&self.thread_pool, Type::Pointer, results);
        self.scans.insert(name, scan);
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
                SimpleProgress::new(&mut self.w, "Performing initial scan"),
                c,
            );

            scan.initial = false;

            result
        } else {
            scan.scan(
                handle,
                filter,
                cancel,
                SimpleProgress::new(&mut self.w, "Rescanning"),
            )
        };

        if self.suspend || config.suspend {
            handle.process.resume()?;
        }

        println!("");
        result?;

        let len = usize::min(scan.len(), DEFAULT_LIMIT);
        Self::print(&mut self.w, scan.len(), &scan[..len])?;
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

        writeln!(
            self.w,
            "Name: {}",
            handle
                .name
                .as_ref()
                .map(|n| n.as_str())
                .unwrap_or("*unknown*")
        )?;

        writeln!(self.w, "Process id: {}", handle.process.process_id)?;
        writeln!(self.w, "Pointer width: {}", handle.process.pointer_width)?;
        writeln!(self.w, "64 bit: {}", handle.process.is_64bit)?;

        if include_modules {
            writeln!(self.w, "Modules:")?;

            for module in &handle.modules {
                writeln!(self.w, " - {:?}", module)?;
            }
        } else {
            writeln!(self.w, "Modules: *only shown with --modules*")?;
        }

        if include_threads {
            writeln!(self.w, "Threads:")?;

            for thread in &handle.threads {
                writeln!(self.w, " - {:?}", thread)?;
            }
        } else {
            writeln!(self.w, "Threads: *only shown with --threads*")?;
        }

        if include_memory {
            writeln!(self.w, "Memory Regions:")?;

            for region in handle.process.virtual_memory_regions() {
                let region = region?;

                let MemoryInformation {
                    range,
                    state,
                    ty,
                    protect,
                } = region;

                writeln!(
                    self.w,
                    " - {ty:?} [{}-{}] {protect:?} {state:?}",
                    range.base,
                    range.base.saturating_add(range.size),
                    ty = ty,
                    protect = protect,
                    state = state,
                )?;
            }
        } else {
            writeln!(self.w, "Memory Regions: *only shown with --memory*")?;
        }

        Ok(())
    }

    /// Print the current state of the scan.
    fn print(
        w: &mut impl io::Write,
        len: usize,
        results: &[Box<scan::ScanResult>],
    ) -> anyhow::Result<()> {
        if len > results.len() {
            writeln!(
                w,
                "found {} addresses (only showing {})",
                len,
                results.len()
            )?;
        } else {
            writeln!(w, "found {} addresses", results.len())?;
        };

        for result in results {
            writeln!(w, "{} = {}", result.pointer, result.value)?;
        }

        Ok(())
    }

    /// Run the given procedure while suspending the process (if application is configured to do so).
    pub fn with_scan(
        &mut self,
        handle: &ProcessHandle,
        mut proc: impl FnMut(&mut scan::Scan, &Process, &mut W) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        let scan = match self.scans.get_mut(&self.current_scan) {
            Some(scan) => scan,
            None => return Ok(()),
        };

        if self.suspend {
            handle.process.suspend()?;
        }

        let ret = proc(scan, &handle.process, &mut self.w);

        if self.suspend {
            handle.process.resume()?;
        }

        ret
    }

    pub fn line(&mut self) -> anyhow::Result<String> {
        use std::io::{BufRead, BufReader};

        let mut line = String::new();

        // neat trick :)
        BufReader::new(&mut self.r).read_line(&mut line)?;

        Ok(line)
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
                .arg(
                    Arg::with_name("switch")
                        .help("Switch to a specific scan.")
                        .long("switch")
                        .short("s")
                        .value_name("name")
                        .takes_value(true),
                )
                .arg(
                    Arg::with_name("type")
                        .help("The type of the filter, if it can't be derived.")
                        .long("type")
                        .takes_value(true),
                )
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
                .arg(Arg::with_name("filter").help("The filter to apply.")),
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
                    Arg::with_name("pointer")
                        .help("The address/pointer to delete.")
                        .required(true),
                ),
        );

        let app = app.subcommand(
            App::new("add")
                .about("Add an address to the current scan")
                .arg(
                    Arg::with_name("type")
                        .help("The type of the pointer added to the scan.")
                        .required(true),
                )
                .arg(
                    Arg::with_name("pointer")
                        .help("The address/pointer to delete.")
                        .required(true),
                ),
        );

        let app = app.subcommand(
            App::new("pointer-scan")
                .alias("pt")
                .about("Perform a pointer scan on the specified address.")
                .arg(
                    Arg::with_name("name")
                        .help(
                            "The name of the scan in which to store result (default: \"default\").",
                        )
                        .long("name"),
                )
                .arg(
                    Arg::with_name("address")
                        .help("The address to scan for.")
                        .required(true),
                )
                .arg(
                    Arg::with_name("type")
                        .help("The type of the resulting value.")
                        .value_name("type")
                        .takes_value(true),
                ),
        );

        app
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
            ("scan", Some(m)) => {
                if m.is_present("list") {
                    return Ok(Action::ScanList);
                }

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

                if let Some(name) = m.value_of("switch") {
                    return Ok(Action::ScanSwitch {
                        name: name.to_string(),
                    });
                }

                let ty = match m.value_of("type").map(str::parse).transpose()? {
                    Some(ty) => Some(ty),
                    None => None,
                };

                let filter = m
                    .value_of("filter")
                    .ok_or_else(|| anyhow!("missing <filter>"))?;

                let process = self.handle.as_ref().map(|h| &h.process);
                let matcher = filter::parse_matcher(&filter, process)?;

                let last_type = self.scans.get(&self.current_scan).and_then(|s| s.last_type);

                let ty = last_type
                    .or_else(|| matcher.type_hint())
                    .or(ty)
                    .ok_or_else(|| anyhow!("cannot determine type of filter"))?;

                let filter = filter::Filter::new(ty, matcher);

                let mut config = ScanConfig::default();
                config.modules_only = m.is_present("modules-only");
                config.suspend = m.is_present("suspend");
                return Ok(Action::Scan { filter, config });
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
                let pointer = m
                    .value_of("pointer")
                    .ok_or_else(|| anyhow!("missing <pointer>"))?;
                let pointer = Pointer::parse(pointer)?;
                return Ok(Action::Del { pointer });
            }
            ("add", Some(m)) => {
                let ty = m
                    .value_of("type")
                    .map(str::parse)
                    .transpose()?
                    .ok_or_else(|| anyhow!("missing <type>"))?;
                let pointer = m
                    .value_of("pointer")
                    .ok_or_else(|| anyhow!("missing <pointer>"))?;
                let pointer = Pointer::parse(pointer)?;
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

                let value_type = m.value_of("type").map(str::parse).transpose()?;

                return Ok(Action::PointerScan {
                    name,
                    address,
                    value_type,
                });
            }
            _ => return Ok(Action::Help),
        }
    }

    /// Print help messages.
    pub fn print_help(w: &mut impl io::Write) -> anyhow::Result<()> {
        for line in HELP.lines() {
            writeln!(w, "{}", line)?;
        }

        Ok(())
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
        pointer: Pointer,
    },
    /// Add the given address with the given type.
    Add {
        pointer: Pointer,
        ty: Type,
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
}
