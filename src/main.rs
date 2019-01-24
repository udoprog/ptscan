use failure::ResultExt;
use ptscan::{
    scan, scanner, system_processes, system_threads, Location, ProcessHandle, ProcessId,
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

/// Read some input from the terminal.
fn input<T: std::str::FromStr>(prompt: &str) -> Result<Option<T>, failure::Error> {
    use std::io::{BufRead, BufReader, Write};

    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    write!(stdout, "[{}] ", prompt)?;
    stdout.flush()?;

    let stdin = io::stdin();
    let mut stdin = BufReader::new(stdin.lock());

    let mut input = String::new();
    stdin.read_line(&mut input)?;

    let input = input.trim();

    if input.is_empty() {
        return Ok(None);
    }

    let out = T::from_str(input).map_err(|_| failure::format_err!("failed to parse input"))?;

    Ok(Some(out))
}

fn try_main() -> Result<(), failure::Error> {
    let thread_pool = Arc::new(rayon::ThreadPoolBuilder::new().build()?);

    let opts = ptscan::opts::opts();

    let mut threads = HashMap::<_, Vec<_>>::new();

    for t in system_threads()? {
        threads.entry(t.process_id()).or_default().push(t);
    }

    let initial = match input::<u32>("initial value")? {
        None => return Ok(()),
        Some(initial) => initial,
    };

    let scan_type = scan::Type::U32;
    let predicate = scan::Eq(scan::Value::U32(initial));

    for pid in system_processes()? {
        let mut handle = match ProcessHandle::open(pid).with_context(|_| MainError::Open(pid))? {
            Some(handle) => handle,
            // process cannot be opened.
            None => continue,
        };

        if let Some(process) = opts.process.as_ref().map(|n| n.as_str()) {
            let matches = match handle.name.as_ref().map(|n| n.as_str()) {
                Some(name) => process == name,
                None => false,
            };

            if !matches {
                continue;
            }
        }

        handle
            .refresh_threads(&threads)
            .with_context(|_| MainError::RefreshThreads(handle.name()))?;

        let mut scanner = handle.scanner(&thread_pool);

        println!("Starting scan...");
        // handle.process.suspend()?;

        {
            let stdout = io::stdout();
            scanner.scan_for_value(scan_type, &predicate, SimpleProgress::new(stdout))?;
        }

        loop {
            // handle.process.resume()?;
            print_results(&handle, &scanner.results)?;

            let next = match input::<u32>("next value")? {
                None => return Ok(()),
                Some(next) => next,
            };

            let predicate = scan::Eq(scan::Value::U32(next));
            scanner.rescan(&predicate)?;
        }
    }

    Ok(())
}

fn print_results<'a>(handle: &ProcessHandle, results: &[ScanResult]) -> Result<(), failure::Error> {
    if results.len() > 10 {
        println!("Found {} addresses (only showing 10)", results.len());
    } else {
        println!("Found {} addresses", results.len());
    };

    for result in results.iter().take(10) {
        let ScanResult {
            address,
            ref value,
            active,
            ..
        } = *result;

        let info = match active {
            true => format!("{}", value),
            false => format!("*{}", value),
        };

        match handle.find_location(address)? {
            Location::Module(module) => {
                let offset = address.offset_of(module.range.base)?;
                println!("{}{}: {} = {}", module.name, offset, address, info);
            }
            Location::Thread(thread) => {
                let base = thread
                    .stack_exit
                    .as_ref()
                    .cloned()
                    .unwrap_or(thread.stack.base);

                let offset = address.offset_of(base)?;
                println!("THREADSTACK{}{}: {} = {}", thread.id, offset, address, info);
            }
            Location::None => {
                println!("{} = {}", address, info);
            }
        }
    }

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
