//! High-level interface to processes.

use crate::{
    process::Process, system_thread::SystemThread, thread::Thread, utils::AddressRange, ProcessId,
    VirtualAddress,
};
use failure::Error;
use std::collections::HashMap;

#[derive(Debug)]
pub struct ModuleInfo {
    pub name: String,
    pub range: AddressRange,
}

#[derive(Debug)]
pub struct ProcessHandle {
    /// Name of the process (if present).
    pub name: Option<String>,
    /// Handle to the process.
    pub process: Process,
    /// If the process is a 64-bit process (or not).
    is_64bit: bool,
    /// Information about all loaded modules.
    pub modules: Vec<ModuleInfo>,
    /// The range at which we have found kernel32.dll (if present).
    kernel32: Option<AddressRange>,
    /// Threads.
    pub threads: Vec<ProcessThread>,
}

impl ProcessHandle {
    /// Open the given process id and create a simplified handle to it.
    pub fn open(
        pid: ProcessId,
        threads: &HashMap<ProcessId, Vec<SystemThread>>,
    ) -> Result<ProcessHandle, Error> {
        use std::ffi::OsStr;

        let process = Process::open(pid)?;
        let is_64bit = process.is_64bit()?;

        let mut name = None;
        let mut kernel32 = None;

        let mut modules = Vec::new();

        for module in process.modules()? {
            let module_name = module.name()?;

            if name.is_none() {
                name = Some(module_name.to_string_lossy().to_string());
            }

            let info = module.info()?;

            if module_name.as_os_str() == OsStr::new("KERNEL32.DLL") {
                kernel32 = Some(AddressRange {
                    base: info.base_of_dll,
                    length: info.size_of_image,
                });
            }

            modules.push(ModuleInfo {
                name: module_name.to_string_lossy().to_string(),
                range: AddressRange {
                    base: info.base_of_dll,
                    length: info.size_of_image,
                },
            });
        }

        modules.sort_by_key(|m| m.range.base);

        let mut handle = ProcessHandle {
            name,
            process,
            is_64bit,
            modules,
            kernel32,
            threads: Vec::new(),
        };

        let threads = threads.get(&pid).map(|t| t.as_slice()).unwrap_or(&[]);

        for (id, t) in threads.into_iter().enumerate() {
            let thread = Thread::builder().all_access().build(t.thread_id())?;
            let stack = handle.thread_stack(&thread)?;
            let stack_exit = handle.scan_for_exit(stack)?;
            handle.threads.push(ProcessThread {
                id,
                stack,
                thread,
                stack_exit,
            });
        }

        handle.threads.sort_by_key(|t| t.stack.base);
        Ok(handle)
    }

    /// Find the module that the address is contained in.
    pub fn find_location<'a>(&'a self, address: VirtualAddress) -> Location<'a> {
        if let Some(module) = self.find_module(address) {
            return Location::Module(module);
        }

        // TODO: need exact stack for thread.
        if let Some(thread) = self.find_thread_by_stack(address) {
            return Location::Thread(thread);
        }

        Location::None
    }

    /// Find if address is contained in a module.
    pub fn find_module<'a>(&'a self, address: VirtualAddress) -> Option<&'a ModuleInfo> {
        let module = match self
            .modules
            .binary_search_by(|m| m.range.base.cmp(&address))
        {
            Ok(exact) => &self.modules[exact],
            Err(closest) => {
                if closest > 0 {
                    &self.modules[closest - 1]
                } else {
                    return None;
                }
            }
        };

        if module.range.contains(address) {
            return Some(module);
        }

        None
    }

    /// Find if address is contained in a module.
    pub fn find_thread_by_stack<'a>(
        &'a self,
        address: VirtualAddress,
    ) -> Option<&'a ProcessThread> {
        let thread = match self
            .threads
            .binary_search_by(|m| m.stack.base.cmp(&address))
        {
            Ok(exact) => &self.threads[exact],
            Err(closest) => {
                if closest > 0 {
                    &self.threads[closest - 1]
                } else {
                    return None;
                }
            }
        };

        if thread.stack.contains(address) {
            return Some(thread);
        }

        None
    }

    /// Access the address of the stack for the given thread.
    pub fn thread_stack(&self, thread: &Thread) -> Result<AddressRange, Error> {
        if self.is_64bit {
            thread.thread_stack(&self.process)
        } else {
            thread.get_context()?.thread_stack(&self.process)
        }
    }

    /// Scan for the base of the stack.
    ///
    /// TODO: look into using VirtualQueryEx to get the range of the stack.
    /// https://msdn.microsoft.com/en-us/library/windows/desktop/aa366907(v=vs.85).aspx
    ///
    /// Ported from:
    /// https://github.com/cheat-engine/cheat-engine/blob/0d9d35183d0dcd9eeed4b4d0004fa3a1981b018a/Cheat%20Engine/CEFuncProc.pas
    pub fn scan_for_exit(&self, stack: AddressRange) -> Result<Option<VirtualAddress>, Error> {
        use byteorder::{ByteOrder, LittleEndian};

        let ptr_width = if self.is_64bit { 8usize } else { 8usize };

        let mut buf = vec![0u8; stack.length as usize];
        let buf = self.process.read_process_memory(stack.base, &mut buf)?;

        if stack.base % (ptr_width as u64) != 0 {
            failure::bail!("for some reason, the stack is not aligned!");
        }

        let kernel32 = match self.kernel32.as_ref() {
            Some(kernel32) => kernel32,
            None => return Ok(None),
        };

        for (n, w) in buf.chunks(ptr_width).enumerate().rev() {
            let n = n as VirtualAddress;
            let ptr_width = ptr_width as VirtualAddress;

            // TODO: make independent of host architecture (use u64).
            let ptr = LittleEndian::read_u64(w) as VirtualAddress;

            if kernel32.contains(ptr) {
                return Ok(Some(stack.base + n * ptr_width));
            }
        }

        Ok(None)
    }

    pub fn scan_for_value<T>(
        &self,
        buffer_size: u64,
        value: T,
    ) -> Result<Vec<VirtualAddress>, Error>
    where
        T: Sync + PartialEq<T> + Decode,
    {
        use crate::utils::IteratorExtension;
        use rayon::prelude::*;
        use std::mem;

        let size: usize = mem::size_of::<T>();

        let segments = self
            .process
            .virtual_memory_regions()
            .only_relevant(false)
            .chunked(buffer_size)
            .collect::<Vec<_>>();

        let addrs = segments
            .into_par_iter()
            .map(|r| {
                let (_, r) = r?;
                let mut buf = vec![0u8; r.length as usize];

                // TODO: handle when value overlapps memory edges.
                let buf = match self.process.read_process_memory(r.base, &mut buf) {
                    Ok(buf) => buf,
                    Err(_) => {
                        // eprintln!("ERR: info: {:?}, range: {:?}: {}", info, r, e);
                        return Ok(Vec::new());
                    }
                };

                let mut results = Vec::new();

                for (n, w) in buf.chunks(size).enumerate() {
                    if T::decode(w)? == value {
                        results.push(r.base + (size as VirtualAddress * n as VirtualAddress));
                    }
                }

                Ok(results)
            })
            .collect::<Result<Vec<Vec<VirtualAddress>>, Error>>()?;

        Ok(addrs.into_iter().flat_map(|vec| vec).collect())
    }
}

#[derive(Debug)]
pub enum Location<'a> {
    /// Location found in module.
    Module(&'a ModuleInfo),
    /// Location found in thread stack.
    Thread(&'a ProcessThread),
    /// Location not found.
    None,
}

#[derive(Debug)]
pub struct ProcessThread {
    pub id: usize,
    pub stack: AddressRange,
    pub thread: Thread,
    pub stack_exit: Option<VirtualAddress>,
}

pub trait Decode: Sized {
    fn decode(buf: &[u8]) -> Result<Self, Error>;
}

impl Decode for u64 {
    fn decode(buf: &[u8]) -> Result<Self, Error> {
        use byteorder::{ByteOrder, LittleEndian};
        Ok(LittleEndian::read_u64(buf))
    }
}

impl Decode for u32 {
    fn decode(buf: &[u8]) -> Result<Self, Error> {
        use byteorder::{ByteOrder, LittleEndian};
        Ok(LittleEndian::read_u32(buf))
    }
}
