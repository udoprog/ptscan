//! High-level interface to processes.

use failure::Error;
use std::fmt;

use crate::{utils::Hex, process::Process, thread, ProcessId, VirtualAddress};

pub struct AddressRange {
    pub base: VirtualAddress,
    pub length: VirtualAddress,
}

impl fmt::Debug for AddressRange {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("AddressRange")
            .field("base", &Hex(self.base))
            .field("length", &Hex(self.length))
            .finish()
    }
}

impl AddressRange {
    pub fn contains(&self, value: VirtualAddress) -> bool {
        self.base <= value && value <= (self.base + self.length)
    }
}

#[derive(Debug)]
pub struct ModuleInfo {
    name: String,
    base_address: VirtualAddress,
    region_size: VirtualAddress,
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
}

impl ProcessHandle {
    /// Open the given process id and create a simplified handle to it.
    pub fn open(pid: ProcessId) -> Result<ProcessHandle, Error> {
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
                base_address: info.base_of_dll,
                region_size: info.size_of_image,
            });
        }

        Ok(ProcessHandle {
            name,
            process,
            is_64bit,
            modules,
            kernel32,
        })
    }

    /// Access the address of the stack for the given thread.
    pub fn thread_stack(&self, thread: &thread::Thread) -> Result<Option<VirtualAddress>, Error> {
        let stack = if self.is_64bit {
            thread.thread_stack(&self.process)?
        } else {
            let ctx = thread.get_context()?;
            ctx.thread_stack(&self.process)?
        };

        // TODO: take variable stack sizes into account, don't just use 0x1000.
        self.scan_for_stack_base(stack, 0x1000)
    }

    /// Scan for the base of the stack.
    ///
    /// TODO: look into using VirtualQueryEx to get the range of the stack.
    /// https://msdn.microsoft.com/en-us/library/windows/desktop/aa366907(v=vs.85).aspx
    ///
    /// Ported from:
    /// https://github.com/cheat-engine/cheat-engine/blob/0d9d35183d0dcd9eeed4b4d0004fa3a1981b018a/Cheat%20Engine/CEFuncProc.pas
    fn scan_for_stack_base(
        &self,
        stack: VirtualAddress,
        depth: usize,
    ) -> Result<Option<VirtualAddress>, Error> {
        use byteorder::{ByteOrder, LittleEndian};

        let ptr_width = if self.is_64bit { 8usize } else { 8usize };

        let base = stack.saturating_sub(depth as VirtualAddress);
        let mut buf = vec![0u8; depth];
        let buf = self.process.read_process_memory(base, &mut buf)?;

        if base % (ptr_width as u64) != 0 {
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
                return Ok(Some(base + n * ptr_width));
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
                    Err(e) => {
                        // eprintln!("ERR: info: {:?}, range: {:?}: {}", info, r, e);
                        return Ok(Vec::new());
                    },
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
