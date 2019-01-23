use failure::Error;
use ptscan::{system_processes, system_threads, Location, ProcessHandle, Size};
use std::collections::HashMap;

fn try_main() -> Result<(), Error> {
    let opts = ptscan::opts::opts();

    let mut threads = HashMap::<_, Vec<_>>::new();

    for t in system_threads()? {
        threads.entry(t.process_id()).or_default().push(t);
    }

    let buffer_size = Size::new(0x100u64);
    let needle = 0xDEADBEEFu32;

    for pid in system_processes()? {
        let handle = match ProcessHandle::open(pid, &threads) {
            Ok(handle) => handle,
            Err(e) => {
                println!("failed to open: {}", e);
                continue;
            }
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

fn main() -> Result<(), Error> {
    if let Err(e) = try_main() {
        eprintln!("{}", e);
        eprintln!("{}", e.backtrace());
    }

    Ok(())
}
