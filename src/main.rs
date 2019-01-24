use failure::ResultExt;
use ptscan::{
    scan, scanner, system_processes, system_threads, Address, Location, ProcessHandle, ProcessId,
    ProcessName, ScanResult,
};
use std::{collections::HashMap, io, sync::Arc};

#[derive(Debug, failure::Fail)]
enum MainError {
    #[fail(display = "failed to open process: {}", _0)]
    Open(ProcessId),
    #[fail(display = "failed to refresh threads for process: {}", _0)]
    RefreshThreads(ProcessName),
}

fn try_main() -> Result<(), failure::Error> {
    let thread_pool = Arc::new(rayon::ThreadPoolBuilder::new().build()?);

    let opts = ptscan::opts::opts();

    let name = match opts.process.as_ref() {
        Some(name) => name.as_str(),
        None => failure::bail!("missing `--process` argument"),
    };

    let handle = match find_process_by_name(name)? {
        Some(handle) => handle,
        None => failure::bail!("no process matching name `{}`", name),
    };

    let mut scanner = handle.scanner(&thread_pool);
    let mut app = Application::new(io::stdin(), io::stdout(), opts.suspend);
    app.run(&handle, &mut scanner)?;
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

/// Find the first process matching the given name.
fn find_process_by_name(name: &str) -> Result<Option<ProcessHandle>, failure::Error> {
    let mut threads = HashMap::<_, Vec<_>>::new();

    for t in system_threads()? {
        threads.entry(t.process_id()).or_default().push(t);
    }

    for pid in system_processes()? {
        let mut handle = match ProcessHandle::open(pid).with_context(|_| MainError::Open(pid))? {
            Some(handle) => handle,
            // process cannot be opened.
            None => continue,
        };

        let handle_name = match handle.name.as_ref() {
            // NB: can't find by name if we can't decode the name from the process :(.
            None => continue,
            Some(handle_name) => handle_name.as_str(),
        };

        if handle_name != name {
            continue;
        }

        handle
            .refresh_threads(&threads)
            .with_context(|_| MainError::RefreshThreads(handle.name()))?;

        return Ok(Some(handle));
    }

    Ok(None)
}

pub struct Application<R, W> {
    r: R,
    w: W,
    suspend: bool,
}

impl<R, W> Application<R, W>
where
    R: io::Read,
    W: Send + io::Write,
{
    /// Create a new application with the given options.
    pub fn new(r: R, w: W, suspend: bool) -> Application<R, W> {
        Application { r, w, suspend }
    }

    pub fn run<'handle>(
        &mut self,
        handle: &'handle ProcessHandle,
        scanner: &mut scanner::Scanner<'handle>,
    ) -> Result<(), failure::Error> {
        let mut scan_in_progress = false;

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

            match action {
                Action::Exit => {
                    return Ok(());
                }
                Action::Help => {
                    Self::print_help(&mut self.w)?;
                }
                Action::Print => {
                    // NB: always refresh before printing to show most up-to-date values.
                    Self::with_process(self.suspend, handle, || scanner.refresh())?;
                    Self::print_results(&mut self.w, handle, &scanner.results)?;
                }
                Action::Scan(predicate) => {
                    if scan_in_progress {
                        Self::with_process(self.suspend, handle, || scanner.rescan(&*predicate))?;
                    } else {
                        Self::with_process(self.suspend, handle, || {
                            scanner.scan_for_value(&*predicate, SimpleProgress::new(&mut self.w))
                        })?;

                        scan_in_progress = true;
                    }

                    writeln!(self.w, "found {} addresses", scanner.results.len())?;
                }
                Action::Reset => {
                    scanner.results.clear();
                    scan_in_progress = false;
                }
                Action::Set(address, value) => {
                    let mut buf = vec![0u8; value.ty().size()];
                    value.encode(&mut buf);
                    handle.process.write_process_memory(address, &buf)?;
                }
            }
        }
    }

    /// Run the given procedure while suspending the process (if application is configured to do so).
    pub fn with_process<T>(
        suspend: bool,
        handle: &ProcessHandle,
        mut proc: impl FnMut() -> Result<T, failure::Error>,
    ) -> Result<T, failure::Error> {
        if suspend {
            handle.process.suspend()?;
        }

        let ret = proc();

        if suspend {
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
            "p" | "print" => return Ok(Action::Print),
            "s" | "scan" => {
                let command = &command[1..];

                if command.len() < 2 {
                    failure::bail!("Usage: rescan <op> [value]");
                }

                let predicate = Self::parse_predicate(command)?;
                return Ok(Action::Scan(predicate));
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
            command => failure::bail!("no such command: {}", command),
        }
    }

    /// Parse the predicate from commandline input.
    ///
    /// Note: only supports simple predicates for now.
    pub fn parse_predicate(rest: &[&str]) -> Result<Box<dyn scan::Predicate>, failure::Error> {
        let op = rest[0];
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
        writeln!(w, "Usage:")?;
        writeln!(w, "help - print this help message")?;
        writeln!(w, "exit - exit the application")?;
        writeln!(
            w,
            "initial <op> <value> - perform an initial scan for the given <value>"
        )?;
        writeln!(
            w,
            "rescan <op> <value> - perform a refinement scan for the given <value>"
        )?;
        writeln!(
            w,
            "set <address> <value> - set the value of the given memory location"
        )?;
        writeln!(w, "")?;
        writeln!(w, "<op> above is one of: eq, gt, gte, lt, lte.")?;
        writeln!(
            w,
            "<value> is can be a numeric value suffixed by its type. If omitted, the default type is i32."
        )?;
        Ok(())
    }

    /// Print the results of a scan.
    pub fn print_results<'a>(
        out: &mut impl io::Write,
        handle: &ProcessHandle,
        results: &[ScanResult],
    ) -> Result<(), failure::Error> {
        if results.len() > 10 {
            writeln!(out, "Found {} addresses (only showing 10)", results.len())?;
        } else {
            writeln!(out, "Found {} addresses", results.len())?;
        };

        for result in results.iter().take(10) {
            let ScanResult {
                address,
                ref value,
                ref current,
                ..
            } = *result;

            let info = match current.as_ref() {
                // How to display values that have changed.
                Some(current) if current != value => format!("{} => {}", value, current),
                Some(_) | None => format!("{}", value),
            };

            match handle.find_location(address)? {
                Location::Module(module) => {
                    let offset = address.offset_of(module.range.base)?;
                    writeln!(out, "{}{}: {} = {}", module.name, offset, address, info)?;
                }
                Location::Thread(thread) => {
                    let base = thread
                        .stack_exit
                        .as_ref()
                        .cloned()
                        .unwrap_or(thread.stack.base);

                    let offset = address.offset_of(base)?;
                    writeln!(
                        out,
                        "THREADSTACK{}{}: {} = {}",
                        thread.id, offset, address, info
                    )?;
                }
                Location::None => {
                    writeln!(out, "{} = {}", address, info)?;
                }
            }
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
    /// Initialize or refine the existing scan.
    Scan(Box<dyn scan::Predicate>),
    /// Reset the state of the scan.
    Reset,
    /// Print results from scan.
    Print,
    /// Write the value at the given address.
    Set(Address, scan::Value),
}
