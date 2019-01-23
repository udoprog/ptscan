use failure::ResultExt;
use ptscan::{
    scan, system_processes, system_threads, Location, ProcessHandle, ProcessId, ProcessName,
    ScanResult,
};
use std::{collections::HashMap, sync::Arc};

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

    let mut threads = HashMap::<_, Vec<_>>::new();

    for t in system_threads()? {
        threads.entry(t.process_id()).or_default().push(t);
    }

    let scan_type = scan::Type::U32;
    let predicate = scan::Gt(scan::Value::U32(0x1000));
    /*let predicate = scan::All(
        vec![
            Box::new(scan::Gt(scan::Value::U32(0))),
            Box::new(scan::Lt(scan::Value::U32(4))),
        ]
    );*/

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
        scanner.scan_for_value(scan_type, &predicate)?;
        println!(
            "Found {} addresses (only showing 100)",
            scanner.results.len()
        );
        print_results(&handle, scanner.results.iter().take(100))?;
    }

    Ok(())
}

fn print_results<'a>(
    handle: &ProcessHandle,
    results: impl IntoIterator<Item = &'a ScanResult>,
) -> Result<(), failure::Error> {
    for result in results {
        let ScanResult {
            address, ref value, ..
        } = *result;

        match handle.find_location(address)? {
            Location::Module(module) => {
                let offset = address.offset_of(module.range.base)?;
                println!("{}{}: {} = {}", module.name, offset, address, value);
            }
            Location::Thread(thread) => {
                let base = thread
                    .stack_exit
                    .as_ref()
                    .cloned()
                    .unwrap_or(thread.stack.base);

                let offset = address.offset_of(base)?;
                println!(
                    "THREADSTACK{}{}: {} = {}",
                    thread.id, offset, address, value
                );
            }
            Location::None => {
                println!("{} = {}", address, value);
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

    println!("press [enter] to exit");
    let mut input = String::new();
    std::io::stdin().read_line(&mut input)?;
    Ok(())
}
