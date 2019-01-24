use hashbrown::HashMap;
use ptscan::{scan, scanner, Address, Location, Process, ProcessHandle, ScanResult};
use std::{io, sync::Arc};

static HELP: &'static str = include_str!("help.md");
static DEFAULT_LIMIT: usize = 10;

fn try_main() -> Result<(), failure::Error> {
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

fn main() -> Result<(), failure::Error> {
    if let Err(e) = try_main() {
        eprintln!("{}", e);
        eprintln!("{}", e.backtrace());

        for c in e.iter_chain().skip(1) {
            eprintln!("Caused by: {}", c);

            if let Some(bt) = c.backtrace() {
                eprintln!("Backtrace: {}", bt);
            }
        }
    }

    Ok(())
}

struct SimpleProgress<W> {
    out: W,
}

impl<W> SimpleProgress<W> {
    fn new(out: W) -> SimpleProgress<W> {
        Self { out: out }
    }
}

impl<W> scanner::Progress for SimpleProgress<W>
where
    W: io::Write,
{
    fn report_bytes(&mut self, _: usize) -> Result<(), failure::Error> {
        // NB: do nothing
        Ok(())
    }

    fn report(&mut self, percentage: usize) -> Result<(), failure::Error> {
        use std::iter;

        write!(self.out, "\r")?;
        self.out.flush()?;

        let repr = iter::repeat('#').take(percentage / 10).collect::<String>();
        write!(self.out, "{}: {}%", repr, percentage)?;
        self.out.flush()?;

        Ok(())
    }

    fn done(&mut self, interrupted: bool) -> Result<(), failure::Error> {
        if interrupted {
            writeln!(self.out, " interrupted!")?;
        } else {
            writeln!(self.out, "")?;
        }

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
    scans: HashMap<String, scanner::Scanner>,
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
    pub fn run(&mut self) -> Result<(), failure::Error> {
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
    fn apply_action(&mut self, action: Action) -> Result<bool, failure::Error> {
        match action {
            Action::ScansList => {
                writeln!(self.w, "Scans:")?;

                for (key, s) in &self.scans {
                    writeln!(self.w, "{} - {} result(s)", key, s.results.len())?;
                }
            }
            Action::ScansNew(name) => {
                if self.scans.contains_key(&name) {
                    failure::bail!("there is already a scan named `{}`", name);
                }

                self.scans
                    .insert(name.clone(), scanner::Scanner::new(&self.thread_pool));
                self.current_scan = name;
            }
            Action::ScansDel(name) => {
                if self.scans.remove(&name).is_none() {
                    failure::bail!("no such scan `{}`", name);
                }

                writeln!(self.w, "removed scan `{}`", name)?;
            }
            Action::Exit => {
                return Ok(true);
            }
            Action::Help => {
                Self::print_help(&mut self.w)?;
            }
            Action::Process => {
                self.process()?;
            }
            Action::Attach(name) => {
                if name.is_some() {
                    self.process_name = name;
                }

                self.attach()?;
            }
            Action::Print(limit) => {
                if let Some(handle) = self.handle.as_ref() {
                    if let Some(scan) = self.scans.get_mut(&self.current_scan) {
                        scan.refresh(&handle.process)?;
                    }
                }

                self.print(limit)?;
            }
            Action::Del(address) => {
                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => failure::bail!("no active scan"),
                };

                for result in &mut scan.results {
                    if result.address == address {
                        result.killed = true;
                    }
                }
            }
            Action::Add(address, ty) => {
                let handle = match self.handle.as_ref() {
                    Some(handle) => handle,
                    None => failure::bail!("not attached to a process"),
                };

                let scan = match self.scans.get_mut(&self.current_scan) {
                    Some(scan) => scan,
                    None => failure::bail!("no active scan"),
                };

                let mut buf = vec![0u8; ty.size()];
                let value = handle.process.read_memory_of_type(address, ty, &mut buf)?;

                scan.results.push(scanner::ScanResult {
                    address,
                    value,
                    current: None,
                    killed: false,
                });
            }
            Action::Scan(predicate) => {
                self.scan(&*predicate)?;
            }
            Action::Reset => match self.scans.get_mut(&self.current_scan) {
                Some(scanner) => {
                    scanner.results.clear();
                    scanner.initial = false;
                }
                None => writeln!(self.w, "no scanner in use")?,
            },
            Action::Set(address, value) => {
                let handle = match self.handle.as_ref() {
                    Some(handle) => handle,
                    None => failure::bail!("not attached to a process"),
                };

                let mut buf = vec![0u8; value.ty().size()];
                value.encode(&mut buf);
                handle.process.write_process_memory(address, &buf)?;
            }
        }

        Ok(false)
    }

    /// Try to attach to some process.
    fn attach(&mut self) -> Result<(), failure::Error> {
        let name = match self.process_name.as_ref() {
            Some(name) => name,
            None => failure::bail!("no process name to attach to"),
        };

        self.handle = ProcessHandle::open_by_name(name)?;

        if self.handle.is_none() {
            writeln!(
                self.w,
                "not attached: could not find any process matching `{}`",
                name
            )?;
        }

        Ok(())
    }

    /// Scan memory using the given predicate and the currently selected scanner.
    fn scan(&mut self, predicate: &(dyn scan::Predicate)) -> Result<(), failure::Error> {
        let handle = match self.handle.as_ref() {
            Some(handle) => handle,
            None => failure::bail!("not attached to a process"),
        };

        let scanner = match self.scans.entry(self.current_scan.clone()) {
            hashbrown::hash_map::Entry::Occupied(e) => e.into_mut(),
            hashbrown::hash_map::Entry::Vacant(e) => {
                e.insert(scanner::Scanner::new(&self.thread_pool))
            }
        };

        if self.suspend {
            handle.process.suspend()?;
        }

        // NB: defer unpacking the result until we have at least tried to resume the process.
        let res = if !scanner.initial {
            let res =
                scanner.initial_scan(&handle.process, predicate, SimpleProgress::new(&mut self.w));

            if res.is_ok() {
                scanner.initial = true;
            }

            res
        } else {
            scanner.rescan(&handle.process, predicate)
        };

        if self.suspend {
            handle.process.resume()?;
        }

        res?;
        self.print(None)?;
        Ok(())
    }

    /// Print information on the current process, if any.
    fn process(&mut self) -> Result<(), failure::Error> {
        let handle = match self.handle.as_ref() {
            Some(handle) => handle,
            None => failure::bail!("no process attached"),
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
        writeln!(self.w, "Modules:")?;

        for module in &handle.modules {
            writeln!(self.w, "{:?}", module)?;
        }

        writeln!(self.w, "Threads:")?;

        for thread in &handle.threads {
            writeln!(self.w, "{:?}", thread)?;
        }

        Ok(())
    }

    /// Print the current state of the scanner.
    fn print(&mut self, limit: Option<usize>) -> Result<(), failure::Error> {
        let results = match self.scans.get(&self.current_scan) {
            Some(scanner) => &scanner.results,
            None => {
                writeln!(self.w, "no scanner in use")?;
                return Ok(());
            }
        };

        let limit = limit.unwrap_or(DEFAULT_LIMIT);

        if results.len() > limit {
            writeln!(
                self.w,
                "found {} addresses (only showing 10)",
                results.len()
            )?;
        } else {
            writeln!(self.w, "found {} addresses", results.len())?;
        };

        for result in results.iter().take(limit) {
            let ScanResult {
                address,
                ref value,
                ref current,
                ..
            } = *result;

            let status = match result.killed {
                true => " (*dead*)",
                false => "",
            };

            let info = match current.as_ref() {
                // How to display values that have changed.
                Some(current) if current != value => format!("{} => {}{}", value, current, status),
                Some(_) | None => format!("{}{}", value, status),
            };

            if let Some(handle) = self.handle.as_ref() {
                match handle.find_location(address)? {
                    Location::Module(module) => {
                        let offset = address.offset_of(module.range.base)?;
                        writeln!(self.w, "{}{}: {} = {}", module.name, offset, address, info)?;
                    }
                    Location::Thread(thread) => {
                        let base = thread
                            .stack_exit
                            .as_ref()
                            .cloned()
                            .unwrap_or(thread.stack.base);

                        let offset = address.offset_of(base)?;
                        writeln!(
                            self.w,
                            "THREADSTACK{}{}: {} = {}",
                            thread.id, offset, address, info
                        )?;
                    }
                    Location::None => {
                        writeln!(self.w, "{} = {}", address, info)?;
                    }
                }
            } else {
                writeln!(self.w, "{} = {}", address, info)?;
            }
        }

        Ok(())
    }

    /// Run the given procedure while suspending the process (if application is configured to do so).
    pub fn with_scanner(
        &mut self,
        handle: &ProcessHandle,
        mut proc: impl FnMut(&mut scanner::Scanner, &Process, &mut W) -> Result<(), failure::Error>,
    ) -> Result<(), failure::Error> {
        let scanner = match self.scans.get_mut(&self.current_scan) {
            Some(scanner) => scanner,
            None => return Ok(()),
        };

        if self.suspend {
            handle.process.suspend()?;
        }

        let ret = proc(scanner, &handle.process, &mut self.w);

        if self.suspend {
            handle.process.resume()?;
        }

        ret
    }

    pub fn line(&mut self) -> Result<String, failure::Error> {
        use std::io::{BufRead, BufReader};

        let mut line = String::new();

        // neat trick :)
        BufReader::new(&mut self.r).read_line(&mut line)?;

        Ok(line)
    }

    /// Convert a line into an action.
    pub fn line_into_action(&mut self, line: &str) -> Result<Action, failure::Error> {
        // NB: filter the entire command to remove empty stuff.
        let command = line
            .split(" ")
            .map(|s| s.trim())
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>();

        if command.len() == 0 {
            failure::bail!("missing command");
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
                    failure::bail!("expected <address>");
                }

                let address = str::parse(command[0])?;
                return Ok(Action::Del(address));
            }
            "add" => {
                let command = &command[1..];

                if command.len() < 1 {
                    failure::bail!("expected <address>");
                }

                let ty = if command.len() > 1 {
                    str::parse(command[1])?
                } else {
                    scan::Type::I32
                };

                let address = str::parse(command[0])?;
                return Ok(Action::Add(address, ty));
            }
            "s" | "scan" => {
                let command = &command[1..];

                if command.len() < 1 {
                    failure::bail!("Usage: scan <predicate>");
                }

                let predicate = self.parse_predicate(command)?;
                return Ok(Action::Scan(predicate));
            }
            "ps" | "process" => return Ok(Action::Process),
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
                    failure::bail!("Usage: set <address> <value>");
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
                            failure::bail!("missing <name>");
                        }

                        return Ok(Action::ScansNew(command[0].to_string()));
                    }
                    "del" => {
                        let command = &command[1..];

                        if command.len() != 1 {
                            failure::bail!("missing <name>");
                        }

                        return Ok(Action::ScansDel(command[0].to_string()));
                    }
                    other => failure::bail!("not a `scans` sub-command: {}", other),
                }
            }
            command => failure::bail!("no such command: {}", command),
        }
    }

    /// Parse the predicate from commandline input.
    ///
    /// Note: only supports simple predicates for now.
    pub fn parse_predicate(
        &mut self,
        rest: &[&str],
    ) -> Result<Box<dyn scan::Predicate>, failure::Error> {
        let op = rest[0];

        match op {
            "pointer" => {
                let handle = match self.handle.as_ref() {
                    Some(handle) => handle,
                    None => failure::bail!("not attached to a process"),
                };

                return Ok(Box::new(handle.pointee_predicate()?));
            }
            "dec" | "smaller" => return Ok(Box::new(scan::Dec)),
            "inc" | "bigger" => return Ok(Box::new(scan::Inc)),
            "changed" => return Ok(Box::new(scan::Changed)),
            _ => {}
        }

        if rest.len() < 2 {
            failure::bail!("expected: <op> <value>");
        }

        let value: scan::Value = str::parse(rest[1])?;

        let predicate: Box<dyn scan::Predicate> = match op {
            "eq" => Box::new(scan::Eq(value)),
            "gt" => Box::new(scan::Gt(value)),
            "gte" => Box::new(scan::Gte(value)),
            "lt" => Box::new(scan::Lt(value)),
            "lte" => Box::new(scan::Lte(value)),
            other => failure::bail!("bad <op>: {}", other),
        };

        Ok(predicate)
    }

    /// Print help messages.
    pub fn print_help(w: &mut impl io::Write) -> Result<(), failure::Error> {
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
    Process,
    /// Attach to a new, or re-attach to an old process.
    Attach(Option<String>),
    /// Initialize or refine the existing scan.
    Scan(Box<dyn scan::Predicate>),
    /// Reset the state of the scan.
    Reset,
    /// Print results from scan.
    Print(Option<usize>),
    /// Delete the given address from the current scan.
    Del(Address),
    /// Add the given address with the given type.
    Add(Address, scan::Type),
    /// Write the value at the given address.
    Set(Address, scan::Value),
    /// List all scans.
    ScansList,
    /// Create a new scanner with the given name.
    ScansNew(String),
    /// Delete the scanner with the given name.
    ScansDel(String),
}
