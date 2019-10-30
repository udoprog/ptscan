#![feature(backtrace)]

use anyhow::bail;
use hashbrown::HashMap;
use ptscan::{
    filter, scan, Address, Filter, Location, Offset, Process, ProcessHandle, Size, Token, Type,
    Value, ValueExpr,
};
use std::{io, sync::Arc};

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
        }
    }

    /// Run the application in a loop.
    pub fn run(&mut self) -> anyhow::Result<()> {
        loop {
            write!(self.w, "ptscan> ")?;
            self.w.flush()?;

            let line = self.line()?;

            let action = match self.line_into_action(&line) {
                Ok(action) => action,
                Err(e) => {
                    writeln!(self.w, "{}", e)?;
                    continue;
                }
            };

            match self.apply_action(action) {
                Ok(true) => return Ok(()),
                Ok(false) => {}
                Err(e) => {
                    writeln!(self.w, "{}", e)?;
                    continue;
                }
            }
        }
    }

    /// Returns a boolean indicating if the process should exit.
    fn apply_action(&mut self, action: Action) -> anyhow::Result<bool> {
        match action {
            Action::ScansList => {
                writeln!(self.w, "Scans:")?;

                for (key, s) in &self.scans {
                    writeln!(self.w, "{} - {} result(s)", key, s.len())?;
                }
            }
            Action::ScansNew(name) => {
                if self.scans.contains_key(&name) {
                    bail!("there is already a scan named `{}`", name);
                }

                self.scans
                    .insert(name.clone(), scan::Scan::new(&self.thread_pool));
                self.current_scan = name;
            }
            Action::ScansDel(name) => {
                if self.scans.remove(&name).is_none() {
                    bail!("no such scan `{}`", name);
                }

                writeln!(self.w, "removed scan `{}`", name)?;
            }
            Action::ScansSet(name) => {
                if !self.scans.contains_key(&name) {
                    bail!("there is no scan named `{}`", name);
                }

                self.current_scan = name;
            }
            Action::Exit => {
                return Ok(true);
            }
            Action::Help => {
                Self::print_help(&mut self.w)?;
            }
            Action::Process {
                include_memory_regions,
            } => {
                self.process(include_memory_regions)?;
            }
            Action::Attach(name) => {
                if name.is_some() {
                    self.process_name = name;
                }

                self.attach()?;
            }
            Action::Print(limit) => {
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

                    handle.process.refresh_values(
                        &self.thread_pool,
                        &mut stored,
                        None,
                        SimpleProgress::new(&mut self.w, "Refreshing values"),
                    )?;

                    results = &stored;
                    println!("");
                }

                Self::print(&mut self.w, self.handle.as_ref(), scan.len(), results)?;
            }
            Action::Del(address) => {
                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => bail!("no active scan"),
                };

                scan.remove_by_address(address);
            }
            Action::Add(address, ty) => {
                let handle = match self.handle.as_ref() {
                    Some(handle) => handle,
                    None => bail!("not attached to a process"),
                };

                let Application {
                    ref mut scans,
                    ref thread_pool,
                    ref current_scan,
                    ..
                } = self;

                let scan = scans
                    .entry(current_scan.clone())
                    .or_insert_with(|| scan::Scan::new(thread_pool));

                let value = handle.process.address_proxy(address).eval(ty)?;
                scan.push(scan::ScanResult { address, value });
            }
            Action::Scan(filter) => {
                self.scan(&filter, None)?;
            }
            Action::PointerScan(address) => {
                self.pointer_scan(address)?;
            }
            Action::Reset => match self.scans.get_mut(&self.current_scan) {
                Some(scan) => {
                    scan.clear();
                    scan.initial = false;
                }
                None => writeln!(self.w, "no scan in use")?,
            },
            Action::Set(address, value) => {
                let handle = match self.handle.as_ref() {
                    Some(handle) => handle,
                    None => bail!("not attached to a process"),
                };

                let mut buf = vec![0u8; value.size(&handle.process)];
                value.encode(&handle.process, &mut buf)?;
                handle.process.write_process_memory(address, &buf)?;
            }
        }

        Ok(false)
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
    fn pointer_scan(&mut self, needle: Address) -> anyhow::Result<()> {
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
        )?;

        writeln!(self.w, "")?;

        let mut forward = BTreeMap::new();
        let mut reverse = BTreeMap::new();

        for result in &scan.results {
            let value = result.value.as_address()?;
            forward.insert(result.address, value);
            reverse.insert(value, result.address);
        }

        let max_depth = 7;
        let max_offset = Size::new(0x100);

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

                        write!(self.w, "{}{}", module.name, offset)?;

                        for o in path {
                            write!(self.w, " -> {}", o)?;
                        }

                        writeln!(self.w, "")?;
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

        writeln!(self.w, "Found {} pointers", forward.len())?;
        Ok(())
    }

    /// Scan memory using the given filter and the currently selected scan.
    fn scan(&mut self, filter: &filter::Filter, cancel: Option<&Token>) -> anyhow::Result<()> {
        let handle = match self.handle.as_ref() {
            Some(handle) => handle,
            None => bail!("not attached to a process"),
        };

        if self.suspend {
            handle.process.suspend()?;
        }

        let res = match self.scans.get(&self.current_scan) {
            Some(scan) => scan.scan(
                &handle.process,
                filter,
                cancel,
                SimpleProgress::new(&mut self.w, "Rescanning"),
            ),
            None => {
                let mut scan = scan::Scan::new(&self.thread_pool);

                let res = scan.initial_scan(
                    handle,
                    filter,
                    cancel,
                    SimpleProgress::new(&mut self.w, "Performing initial scan"),
                );

                res.map(move |()| scan)
            }
        };

        if self.suspend {
            handle.process.resume()?;
        }

        println!("");
        let scan = res?;

        let len = usize::min(scan.len(), DEFAULT_LIMIT);

        Self::print(&mut self.w, self.handle.as_ref(), scan.len(), &scan[..len])?;

        self.scans.insert(self.current_scan.clone(), scan);
        Ok(())
    }

    /// Print information on the current process, if any.
    fn process(&mut self, include_memory_regions: bool) -> anyhow::Result<()> {
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

        writeln!(self.w, "Modules:")?;

        for module in &handle.modules {
            writeln!(self.w, "{:?}", module)?;
        }

        writeln!(self.w, "Threads:")?;

        for thread in &handle.threads {
            writeln!(self.w, "{:?}", thread)?;
        }

        if include_memory_regions {
            writeln!(self.w, "Memory Regions:")?;

            for region in handle.process.virtual_memory_regions() {
                let region = region?;

                if region.is_writable() {
                    writeln!(self.w, "{:?}", region)?;
                }
            }
        }

        Ok(())
    }

    /// Print the current state of the scan.
    fn print(
        w: &mut impl io::Write,
        handle: Option<&ProcessHandle>,
        len: usize,
        results: &[scan::ScanResult],
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
            if let Some(handle) = handle.as_ref() {
                writeln!(w, "{} = {}", result.address.display(handle), result.value)?;
            } else {
                writeln!(w, "{} = {}", result.address, result.value)?;
            }
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

    /// Convert a line into an action.
    pub fn line_into_action(&mut self, line: &str) -> anyhow::Result<Action> {
        // NB: filter the entire command to remove empty stuff.
        let command = line
            .split(" ")
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>();

        if command.len() == 0 {
            bail!("missing command");
        }

        match command[0] {
            "exit" => return Ok(Action::Exit),
            "h" | "help" => return Ok(Action::Help),
            "p" | "print" => {
                let command = &command[1..];

                let limit = if command.len() > 0 {
                    Some(str::parse(command[0])?)
                } else {
                    None
                };

                return Ok(Action::Print(limit));
            }
            "del" => {
                let command = &command[1..];

                if command.len() != 1 {
                    bail!("expected <address>");
                }

                let address = str::parse(command[0])?;
                return Ok(Action::Del(address));
            }
            "add" => {
                let command = &command[1..];

                if command.len() < 1 {
                    bail!("expected <address>");
                }

                let ty = if command.len() > 1 {
                    str::parse(command[1])?
                } else {
                    Type::I32
                };

                let address = str::parse(command[0])?;
                return Ok(Action::Add(address, ty));
            }
            "s" | "scan" => {
                let command = &command[1..];

                if command.len() < 2 {
                    bail!("Usage: scan <type> <filter>");
                }

                let ty = str::parse(command[0])?;

                let filter = self.parse_filter(ty, &command[1..])?;
                return Ok(Action::Scan(filter));
            }
            "pt" | "pointer-scan" => {
                let command = &command[1..];

                if command.len() != 1 {
                    bail!("expected <address>");
                }

                let address = str::parse(command[0])?;
                return Ok(Action::PointerScan(address));
            }
            "ps" | "process" => {
                return Ok(Action::Process {
                    include_memory_regions: false,
                })
            }
            "attach" => {
                let command = &command[1..];

                let name = if command.len() > 0 {
                    Some(command[0].to_string())
                } else {
                    None
                };

                return Ok(Action::Attach(name));
            }
            "reset" => {
                return Ok(Action::Reset);
            }
            "set" => {
                let command = &command[1..];

                if command.len() != 2 {
                    bail!("Usage: set <address> <value>");
                }

                let address = str::parse(command[0])?;
                let value = str::parse(command[1])?;
                return Ok(Action::Set(address, value));
            }
            "scans" => {
                let command = &command[1..];

                if command.len() == 0 {
                    return Ok(Action::ScansList);
                }

                match command[0] {
                    "list" => return Ok(Action::ScansList),
                    "new" => {
                        let command = &command[1..];

                        if command.len() != 1 {
                            bail!("missing <name>");
                        }

                        return Ok(Action::ScansNew(command[0].to_string()));
                    }
                    "del" => {
                        let command = &command[1..];

                        let name = if command.is_empty() {
                            self.current_scan.to_owned()
                        } else {
                            command[0].to_string()
                        };

                        return Ok(Action::ScansDel(name));
                    }
                    "set" => {
                        let command = &command[1..];

                        if command.len() != 1 {
                            bail!("missing <name>");
                        }

                        return Ok(Action::ScansSet(command[0].to_string()));
                    }
                    other => bail!("not a `scans` sub-command: {}", other),
                }
            }
            command => bail!("no such command: {}", command),
        }
    }

    /// Parse the filter from commandline input.
    ///
    /// Note: only supports simple predicates for now.
    pub fn parse_filter(&mut self, ty: Type, rest: &[&str]) -> anyhow::Result<filter::Filter> {
        let process = self.handle.as_ref().map(|h| &h.process);
        let rest = rest.join(" ");
        filter::parse(&rest, ty, process)
    }

    /// Print help messages.
    pub fn print_help(w: &mut impl io::Write) -> anyhow::Result<()> {
        for line in HELP.lines() {
            writeln!(w, "{}", line)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum Action {
    /// Print help.
    Help,
    /// Exit the application.
    Exit,
    /// Print information on the current process.
    Process { include_memory_regions: bool },
    /// Attach to a new, or re-attach to an old process.
    Attach(Option<String>),
    /// Initialize or refine the existing scan.
    Scan(filter::Filter),
    /// Reset the state of the scan.
    Reset,
    /// Print results from scan.
    Print(Option<usize>),
    /// Delete the given address from the current scan.
    Del(Address),
    /// Add the given address with the given type.
    Add(Address, Type),
    /// Write the value at the given address.
    Set(Address, Value),
    /// List all scans.
    ScansList,
    /// Create a new scan with the given name.
    ScansNew(String),
    /// Delete the scan with the given name.
    ScansDel(String),
    /// Switch to the given scan.
    ScansSet(String),
    /// Perform a pointer scan for the specified address.
    PointerScan(Address),
}
