//! High-level interface to processes.

use crate::{
    process::Process, system_thread::SystemThread, thread::Thread, Address, AddressRange,
    ProcessId, Size,
};
use failure::Error;
use std::{collections::HashMap, convert::TryFrom};

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
    pub fn find_location<'a>(&'a self, address: Address) -> Result<Location<'a>, Error> {
        if let Some(module) = self.find_module(address)? {
            return Ok(Location::Module(module));
        }

        // TODO: need exact stack for thread.
        if let Some(thread) = self.find_thread_by_stack(address)? {
            return Ok(Location::Thread(thread));
        }

        Ok(Location::None)
    }

    /// Find if address is contained in a module.
    pub fn find_module<'a>(&'a self, address: Address) -> Result<Option<&'a ModuleInfo>, Error> {
        let module = match self
            .modules
            .binary_search_by(|m| m.range.base.cmp(&address))
        {
            Ok(exact) => &self.modules[exact],
            Err(closest) => {
                if closest > 0 {
                    &self.modules[closest - 1]
                } else {
                    return Ok(None);
                }
            }
        };

        if module.range.contains(address)? {
            return Ok(Some(module));
        }

        Ok(None)
    }

    /// Find if address is contained in a module.
    pub fn find_thread_by_stack<'a>(
        &'a self,
        address: Address,
    ) -> Result<Option<&'a ProcessThread>, Error> {
        let thread = match self
            .threads
            .binary_search_by(|m| m.stack.base.cmp(&address))
        {
            Ok(exact) => &self.threads[exact],
            Err(closest) => {
                if closest > 0 {
                    &self.threads[closest - 1]
                } else {
                    return Ok(None);
                }
            }
        };

        if thread.stack.contains(address)? {
            return Ok(Some(thread));
        }

        Ok(None)
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
    pub fn scan_for_exit(&self, stack: AddressRange) -> Result<Option<Address>, Error> {
        use byteorder::{ByteOrder, LittleEndian};

        let ptr_width = if self.is_64bit {
            Size::new(8)
        } else {
            Size::new(4)
        };

        let mut buf = vec![0u8; stack.length.into_usize()?];
        let buf = self.process.read_process_memory(stack.base, &mut buf)?;

        if !stack.base.is_aligned(ptr_width)? {
            failure::bail!(
                "for some reason, the stack {:?} is not aligned with {}",
                stack,
                ptr_width
            );
        }

        let kernel32 = match self.kernel32.as_ref() {
            Some(kernel32) => kernel32,
            None => return Ok(None),
        };

        for (n, w) in buf.chunks(ptr_width.into_usize()?).enumerate().rev() {
            // TODO: make independent of host architecture (use u64).
            let ptr = Address::try_from(LittleEndian::read_u64(w))?;

            if kernel32.contains(ptr)? {
                let address = Size::try_from(n * ptr_width.into_usize()?)?;
                return Ok(Some(stack.base.add(address)?));
            }
        }

        Ok(None)
    }

    /// Scan for the given value in memory.
    pub fn scan_for_value<T>(&self, buffer_size: Size, value: T) -> Result<Vec<Address>, Error>
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
                let mut buf = vec![0u8; r.length.into_usize()?];

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
                        results.push(r.base.add(Size::try_from(size * n)?)?);
                    }
                }

                Ok(results)
            })
            .collect::<Result<Vec<Vec<Address>>, Error>>()?;

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
    pub stack_exit: Option<Address>,
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
