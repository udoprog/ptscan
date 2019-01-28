//! High-level interface to processes.

use crate::{
    filter, process, scan, system, thread::Thread, Address, AddressRange, ProcessId, Size, ThreadId,
};
use failure::ResultExt;
use std::{convert::TryFrom, fmt};
use winapi::shared::winerror;

#[derive(Debug, failure::Fail)]
enum Error {
    #[fail(display = "failed to open process: {}", _0)]
    OpenProcess(ProcessId),
    #[fail(display = "failed to build thread interface: {}", _0)]
    BuildThread(ThreadId),
    #[fail(display = "failed to extract thread stack for thread: {}", _0)]
    ThreadStack(ThreadId),
    #[fail(display = "failed to scan for thread exit")]
    ScanForExit,
}

/// A handle for a process.
#[derive(Debug)]
pub struct ProcessHandle {
    /// Name of the process (if present).
    pub name: Option<String>,
    /// Handle to the process.
    pub process: process::Process,
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
    ///
    /// This returns `None` if:
    ///
    /// * We try to open a special PID like 0.
    /// * We don't have sufficient privileges to open the process.
    /// * The process no longer exists.
    pub fn open(pid: ProcessId) -> Result<Option<ProcessHandle>, failure::Error> {
        use std::ffi::OsStr;

        // IDLE process, cannot be opened.
        if pid == 0 {
            return Ok(None);
        }

        let process = match process::Process::builder().all_access().build(pid) {
            Ok(process) => process,
            Err(e) => match e.raw_os_error().map(|n| n as u32) {
                // NB: process no longer exists.
                Some(winerror::ERROR_INVALID_PARAMETER) => return Ok(None),
                // NB: we don't have access to the process.
                Some(winerror::ERROR_ACCESS_DENIED) => return Ok(None),
                _ => return Err(failure::Error::from(e)),
            },
        };

        let mut name = None;
        let mut modules = Vec::new();
        let mut kernel32 = None;

        for module in process.modules()? {
            let module_name = module.name()?;

            if name.is_none() {
                name = Some(module_name.to_string_lossy().to_string());
            }

            let info = module.info()?;

            let range = AddressRange {
                base: info.base_of_dll,
                size: info.size_of_image,
            };

            if module_name.as_os_str() == OsStr::new("KERNEL32.DLL") {
                kernel32 = Some(range.clone());
            }

            modules.push(ModuleInfo {
                name: module_name.to_string_lossy().to_string(),
                range,
            });
        }

        modules.sort_by_key(|m| m.range.base);
        let is_64bit = process.is_64bit()?;

        Ok(Some(ProcessHandle {
            name,
            process,
            is_64bit,
            modules,
            kernel32,
            threads: Vec::new(),
        }))
    }

    /// Find the first process matching the given name.
    pub fn open_by_name(name: &str) -> Result<Option<ProcessHandle>, failure::Error> {
        for pid in system::processes()? {
            let handle = match ProcessHandle::open(pid).with_context(|_| Error::OpenProcess(pid))? {
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

            return Ok(Some(handle));
        }

        Ok(None)
    }

    /// Build a filter that matches any pointers which points to valid memory.
    pub fn pointer_filter(&self) -> Result<PointerFilter, failure::Error> {
        use crate::utils::IteratorExtension;

        let mut memory_regions = self
            .process
            .virtual_memory_regions()
            .only_relevant()
            .collect::<Result<Vec<_>, _>>()?;

        memory_regions.sort_by_key(|m| m.range.base);
        Ok(PointerFilter { memory_regions })
    }

    /// Name of the process.
    pub fn name(&self) -> ProcessName {
        ProcessName {
            id: self.process.process_id(),
            name: self.name.clone(),
        }
    }

    /// Refresh information about known threads.
    pub fn refresh_threads(&mut self) -> Result<(), failure::Error> {
        self.threads.clear();

        for (id, t) in system::threads()?.enumerate() {
            if t.process_id() != self.process.process_id() {
                continue;
            }

            let thread = Thread::builder()
                .all_access()
                .query_information()
                .get_context()
                .build(t.thread_id())
                .with_context(|_| Error::BuildThread(t.thread_id()))?;

            let stack = self
                .thread_stack(&thread)
                .with_context(|_| Error::ThreadStack(thread.thread_id()))?;

            let stack_exit = self
                .scan_for_exit(stack)
                .with_context(|_| Error::ScanForExit)?;

            self.threads.push(ProcessThread {
                id,
                stack,
                thread,
                stack_exit,
            });
        }

        self.threads.sort_by_key(|t| t.stack.base);
        Ok(())
    }

    /// Find the module that the address is contained in.
    pub fn find_location<'a>(&'a self, address: Address) -> Result<Location<'a>, failure::Error> {
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
    pub fn find_module<'a>(
        &'a self,
        address: Address,
    ) -> Result<Option<&'a ModuleInfo>, failure::Error> {
        AddressRange::find_in_range(&self.modules, |m| &m.range, address)
    }

    /// Find if address is contained in a module.
    pub fn find_thread_by_stack<'a>(
        &'a self,
        address: Address,
    ) -> Result<Option<&'a ProcessThread>, failure::Error> {
        AddressRange::find_in_range(&self.threads, |m| &m.stack, address)
    }

    /// Access the address of the stack for the given thread.
    pub fn thread_stack(&self, thread: &Thread) -> Result<AddressRange, failure::Error> {
        // NB: if ptscan is running in 32-bit mode this might have to be different.
        thread.thread_stack(&self.process)
    }

    /// Scan for the base of the stack.
    ///
    /// TODO: look into using VirtualQueryEx to get the range of the stack.
    /// https://msdn.microsoft.com/en-us/library/windows/desktop/aa366907(v=vs.85).aspx
    ///
    /// Ported from:
    /// https://github.com/cheat-engine/cheat-engine/blob/0d9d35183d0dcd9eeed4b4d0004fa3a1981b018a/Cheat%20Engine/CEFuncProc.pas
    pub fn scan_for_exit(&self, stack: AddressRange) -> Result<Option<Address>, failure::Error> {
        use byteorder::{ByteOrder, LittleEndian};

        let ptr_width = if self.is_64bit {
            Size::new(8)
        } else {
            Size::new(4)
        };

        let mut buf = vec![0u8; stack.size.into_usize()?];
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
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub name: String,
    pub range: AddressRange,
}

/// A helper struct to give a name to a process.
#[derive(Debug)]
pub struct ProcessName {
    pub id: ProcessId,
    pub name: Option<String>,
}

impl fmt::Display for ProcessName {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = self.name.as_ref() {
            write!(fmt, "{} ({})", name, self.id)
        } else {
            write!(fmt, "{}", self.id)
        }
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

#[derive(Debug, Clone)]
pub struct ProcessThread {
    pub id: usize,
    pub stack: AddressRange,
    pub thread: Thread,
    pub stack_exit: Option<Address>,
}

pub trait Decode: Sized {
    fn decode(buf: &[u8]) -> Result<Self, failure::Error>;
}

impl Decode for u64 {
    fn decode(buf: &[u8]) -> Result<Self, failure::Error> {
        use byteorder::{ByteOrder, LittleEndian};
        Ok(LittleEndian::read_u64(buf))
    }
}

impl Decode for u32 {
    fn decode(buf: &[u8]) -> Result<Self, failure::Error> {
        use byteorder::{ByteOrder, LittleEndian};
        Ok(LittleEndian::read_u32(buf))
    }
}

#[derive(Debug)]
pub struct PointerFilter {
    /// Sorted memory regions.
    memory_regions: Vec<process::MemoryInformation>,
}

impl filter::Filter for PointerFilter {
    fn special(&self) -> Option<scan::Special> {
        Some(scan::Special::NotZero)
    }

    fn ty(&self) -> Option<scan::Type> {
        Some(scan::Type::U64)
    }

    fn test(&self, _: Option<&scan::ScanResult>, value: &scan::Value) -> bool {
        let address = match value.as_address() {
            Ok(address) => address,
            Err(_) => return false,
        };

        match AddressRange::find_in_range(&self.memory_regions, |m| &m.range, address) {
            Ok(Some(_)) => return true,
            _ => return false,
        }
    }
}

impl fmt::Display for PointerFilter {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "pointer")
    }
}
