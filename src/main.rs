use failure::Error;
use ptscan::{system_processes, system_threads, ProcessHandle};

use std::collections::HashMap;

fn try_main() -> Result<(), Error> {
    use ptscan::SystemThread;

    let opts = ptscan::opts::opts();

    let mut threads = HashMap::<_, Vec<SystemThread>>::new();

    for thread in system_threads()? {
        threads.entry(thread.process_id()).or_default().push(thread);
    }

    let buffer_size = 0x100u64;
    let needle = 0xDEADBEEFu32;

    for pid in system_processes()? {
        let handle = match ProcessHandle::open(pid) {
            Ok(handle) => handle,
            Err(_) => continue,
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

        for module in &handle.modules {
            println!("MODULE: {:?}", module);
        }

        for location in handle.scan_for_value(buffer_size, needle)? {
            println!("{:X}", location);
        }

        /*let mut segments = (start..(end / buffer_size))
        .into_par_iter()
        .map(|n| {
            let s = n * buffer_size;
            let e = u64::min((n + 1) * buffer_size, end);
            (s, e)
        })
        .for_each(|(s, e)| {
            println!("{:?}", (s, e));
        });*/

        /*for segment in segments {
            println!("{:?}", segment);
        }*/

        /*for t in threads.get(&pid).map(|d| d.as_slice()).unwrap_or(&[]) {
            let thread = Thread::builder().all_access().build(t.thread_id())?;

            if let Some(stack) = handle.thread_stack(&thread)? {
                println!("THREAD: {:?} (stack: {:X})", thread, stack);
            }
        }*/
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
