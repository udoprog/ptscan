use failure::ResultExt;
use ptscan::{
    system_processes, system_threads, Location, ProcessHandle, ProcessId, ProcessName, Size,
};
use std::collections::HashMap;

#[derive(Debug, failure::Fail)]
enum MainError {
    #[fail(display = "failed to open process: {}", _0)]
    Open(ProcessId),
    #[fail(display = "failed to refresh threads for process: {}", _0)]
    RefreshThreads(ProcessName),
}

fn try_main() -> Result<(), failure::Error> {
    let opts = ptscan::opts::opts();

    let mut threads = HashMap::<_, Vec<_>>::new();

    for t in system_threads()? {
        threads.entry(t.process_id()).or_default().push(t);
    }

    let buffer_size = Size::new(0x100u64);
    let needle = 0xDEADBEEFu32;

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

        for t in &handle.threads {
            println!("{:?}", t);
        }

        for location in handle.scan_for_value(buffer_size, needle)? {
            match handle.find_location(location)? {
                Location::Module(module) => {
                    let offset = location.offset_of(module.range.base)?;
                    println!("{}{}: {}", module.name, offset, location);
                }
                Location::Thread(thread) => {
                    let base = thread
                        .stack_exit
                        .as_ref()
                        .cloned()
                        .unwrap_or(thread.stack.base);

                    let offset = location.offset_of(base)?;
                    println!("THREADSTACK{}{}: {}", thread.id, offset, location);
                }
                Location::None => {
                    println!("{}", location);
                }
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
