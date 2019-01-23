use crate::{
    address::{Address, Size},
    process_handle::ProcessHandle,
    scan, thread_buffers,
};
use std::{convert::TryFrom, sync::Arc};

/// A scanner responsible for finding results in memory.
pub struct Scanner<'a> {
    process_handle: &'a ProcessHandle,
    thread_pool: Arc<rayon::ThreadPool>,
    /// Current results in scanner.
    pub results: Vec<ScanResult>,
}

impl<'a> Scanner<'a> {
    pub(crate) fn new(
        process_handle: &'a ProcessHandle,
        thread_pool: Arc<rayon::ThreadPool>,
    ) -> Self {
        Self {
            results: Vec::new(),
            process_handle,
            thread_pool,
        }
    }

    /// rescan a set of results.
    pub fn rescan(
        &mut self,
        predicate: &(dyn scan::Predicate + Sync + Send),
    ) -> Result<(), failure::Error> {
        use rayon::prelude::*;

        let buffers = thread_buffers::ThreadBuffers::new();
        let process = &self.process_handle.process;
        let results = &mut self.results;

        self.thread_pool.install(|| {
            let count = results
                .par_iter_mut()
                .map(|result| {
                    let mut work = || {
                        let ty = result.value.ty();

                        let size = ty.size();
                        let mut buf = buffers.get_mut(size)?;
                        let buf = process.read_process_memory(result.address, &mut *buf)?;

                        if buf.len() != size {
                            failure::bail!("incomplete read");
                        }

                        let value = ty.decode(buf);
                        result.active = predicate.test(&value);
                        result.value = value;
                        Ok(())
                    };

                    match work() {
                        Ok(()) => 1,
                        Err(_) => 0,
                    }
                })
                .sum::<u64>();

            println!("successful rescans: {}", count);
            results.retain(|v| v.active);
        });

        Ok(())
    }

    /// Scan for the given value in memory.
    ///
    /// TODO: handle unaligned regions.
    pub fn scan_for_value(
        &mut self,
        scan_type: scan::Type,
        predicate: &(dyn scan::Predicate + Sync + Send),
    ) -> Result<(), failure::Error> {
        use crate::utils::IteratorExtension;
        use std::sync::mpsc;

        let size: usize = scan_type.size();
        let buffer_size = 0x1000;

        let buffers = thread_buffers::ThreadBuffers::new();
        let results = &mut self.results;
        let process = &self.process_handle.process;

        self.thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::sync_channel(1024);
                let mut total = 0;
                let mut bytes = 0;

                for region in process.virtual_memory_regions().only_relevant() {
                    println!("{:?}", region);
                    bytes += region
                        .as_ref()
                        .map_err(|_| ())
                        .and_then(|r| r.range.size.into_usize().map_err(|_| ()))
                        .unwrap_or(0);

                    total += 1;
                    let tx = tx.clone();

                    let buffers_ref = &buffers;

                    s.spawn(move |_| {
                        let work = || {
                            let region = region?;
                            let range = region.range;
                            let len = range.size.into_usize()?;

                            let mut start = 0;

                            while start < len {
                                let len = usize::min(buffer_size, len - start);
                                let loc = range.base.add(Size::try_from(start)?)?;

                                // length of the buffer we need to read process memory.
                                let mut buf = buffers_ref.get_mut(len)?;
                                let buf = process.read_process_memory(loc, &mut *buf)?;

                                for (n, w) in buf.chunks(size).enumerate() {
                                    let value = scan_type.decode(w);

                                    if predicate.test(&value) {
                                        let address = range.base.add(Size::try_from(size * n)?)?;
                                        let scan_result = ScanResult {
                                            address,
                                            value,
                                            active: true,
                                        };
                                        tx.send(TaskProgress::ScanResult(scan_result))?;
                                    }
                                }

                                start += buffer_size;
                            }

                            Ok(())
                        };

                        tx.send(TaskProgress::Result(work()))
                            .expect("channel send failed");
                    });
                }

                println!("TOTAL: {} bytes", bytes);

                let mut current = 0;
                let mut percentage = total / 100;

                if percentage <= 0 {
                    percentage = 1;
                }

                for m in rx {
                    match m {
                        // Scan result from a task.
                        TaskProgress::ScanResult(scan_result) => {
                            results.push(scan_result);
                        }
                        // Result from one task.
                        TaskProgress::Result(result) => {
                            match result {
                                Ok(()) => {}
                                Err(e) => {
                                    println!("error in scan: {}", e);
                                }
                            }

                            current += 1;

                            // print progress.
                            if current % percentage == 0 {
                                let p = (current * 100) / total;
                                println!("{}%", p);
                            }

                            if current >= total {
                                break;
                            }
                        }
                    }
                }
            })
        });

        return Ok(());

        enum TaskProgress {
            Result(Result<(), failure::Error>),
            ScanResult(ScanResult),
        }
    }
}

/// A single scan result.
#[derive(Debug)]
pub struct ScanResult {
    pub address: Address,
    pub value: scan::Value,
    pub active: bool,
}
