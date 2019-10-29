//! High-level interface to processes.

use crate::{
    error::Error,
    pointer::{Base, Pointer},
    process, system,
    thread::Thread,
    Address, AddressRange, ProcessId, Size, Type, Value,
};
use anyhow::{bail, Context as _};
use hashbrown::HashMap;
use std::{convert::TryFrom, fmt};
use winapi::shared::winerror;

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
    /// Module address by name.
    pub modules_address: HashMap<String, Address>,
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
    pub fn open(pid: ProcessId) -> Result<Option<ProcessHandle>, Error> {
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
                _ => return Err(e),
            },
        };

        let name = process.module_base_name()?.to_string_lossy().to_string();
        let is_64bit = process.is_64bit()?;

        Ok(Some(ProcessHandle {
            name: Some(name),
            process,
            is_64bit,
            modules: Vec::new(),
            modules_address: HashMap::new(),
            kernel32: None,
            threads: Vec::new(),
        }))
    }

    /// Find the first process matching the given name.
    pub fn open_by_name(name: &str) -> anyhow::Result<Option<ProcessHandle>> {
        for pid in system::processes()? {
            let handle = match ProcessHandle::open(pid).with_context(|| Error::OpenProcess(pid))? {
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

    /// Build a matcher that matches any pointers which points to valid memory.
    pub fn pointer_matcher(&self) -> Result<PointerMatcher, Error> {
        use crate::utils::IteratorExtension;

        let mut memory_regions = self
            .process
            .virtual_memory_regions()
            .only_relevant()
            .collect::<Result<Vec<_>, _>>()?;

        memory_regions.sort_by_key(|m| m.range.base);
        Ok(PointerMatcher { memory_regions })
    }

    /// Name of the process.
    pub fn name(&self) -> ProcessName {
        ProcessName {
            id: self.process.process_id(),
            name: self.name.clone(),
        }
    }

    /// Refresh all available modules.
    ///
    /// If kernel32 is found, extract its range separately.
    pub fn refresh_modules(&mut self) -> Result<(), Error> {
        use std::ffi::OsStr;

        let mut modules = Vec::with_capacity(self.modules.len());
        let mut modules_address = HashMap::new();

        for module in self.process.modules()? {
            let module_name = module.name()?;

            if self.name.is_none() {
                self.name = Some(module_name.to_string_lossy().to_string());
            }

            let info = module.info()?;

            let range = AddressRange {
                base: info.base_of_dll,
                size: info.size_of_image,
            };

            if module_name.as_os_str() == OsStr::new("KERNEL32.DLL") {
                self.kernel32 = Some(range.clone());
            }

            let name = module_name.to_string_lossy().to_string();

            modules_address.insert(name.to_string(), range.base);

            modules.push(ModuleInfo { name, range });
        }

        modules.sort_by_key(|m| m.range.base);
        self.modules = modules;
        self.modules_address = modules_address;
        Ok(())
    }

    /// Refresh information about known threads.
    pub fn refresh_threads(&mut self) -> anyhow::Result<()> {
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
                .with_context(|| Error::BuildThread(t.thread_id()))?;

            let stack = self
                .thread_stack(&thread)
                .with_context(|| Error::ThreadStack(thread.thread_id()))?;

            let stack_exit = self
                .scan_for_exit(stack)
                .with_context(|| Error::ScanForExit)?;

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
        AddressRange::find_in_range(&self.modules, |m| &m.range, address)
    }

    /// Find if address is contained in a module.
    pub fn find_thread_by_stack<'a>(
        &'a self,
        address: Address,
    ) -> Result<Option<&'a ProcessThread>, Error> {
        AddressRange::find_in_range(&self.threads, |m| &m.stack, address)
    }

    /// Access the address of the stack for the given thread.
    pub fn thread_stack(&self, thread: &Thread) -> Result<AddressRange, Error> {
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
    pub fn scan_for_exit(&self, stack: AddressRange) -> anyhow::Result<Option<Address>> {
        use byteorder::{ByteOrder, LittleEndian};

        let ptr_width = if self.is_64bit {
            Size::new(8)
        } else {
            Size::new(4)
        };

        let mut buf = vec![0u8; stack.size.as_usize()];
        self.process.read_process_memory(stack.base, &mut buf)?;

        if !stack.base.is_aligned(ptr_width)? {
            bail!(
                "for some reason, the stack {:?} is not aligned with {}",
                stack,
                ptr_width
            );
        }

        let kernel32 = match self.kernel32.as_ref() {
            Some(kernel32) => kernel32,
            None => return Ok(None),
        };

        for (n, w) in buf.chunks(ptr_width.as_usize()).enumerate().rev() {
            // TODO: make independent of host architecture (use u64).
            let ptr = Address::try_from(LittleEndian::read_u64(w))?;

            if kernel32.contains(ptr)? {
                let address = Size::try_from(n * ptr_width.as_usize())?;
                return Ok(Some(stack.base.add(address)?));
            }
        }

        Ok(None)
    }

    /// Read an address. Returns None if the address resolves to NULL.
    pub fn read_address(&self, address: Address) -> Option<Address> {
        let mut buf = [0u8; 8];

        // TODO: use dynamic address type.
        let n = match self
            .process
            .read_memory_of_type(address, Type::U64, &mut buf)
            .ok()?
        {
            Value::U64(0) => return None,
            Value::U64(n) => n,
            _ => return None,
        };

        Some(Address::new(n))
    }

    /// Read a pointer.
    pub fn read_pointer(&self, pointer: &Pointer) -> Option<Address> {
        let mut address = match pointer.base {
            Base::Module(ref module, offset) => {
                let address = self.modules_address.get(module)?;
                address.checked_offset(offset)?
            }
            Base::Fixed(address) => address,
        };

        for o in &pointer.offsets {
            address = self.read_address(address)?;
            address = address.checked_offset(*o)?;
        }

        Some(address)
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
    fn decode(buf: &[u8]) -> Result<Self, anyhow::Error>;
}

impl Decode for u64 {
    fn decode(buf: &[u8]) -> Result<Self, anyhow::Error> {
        use byteorder::{ByteOrder, LittleEndian};
        Ok(LittleEndian::read_u64(buf))
    }
}

impl Decode for u32 {
    fn decode(buf: &[u8]) -> Result<Self, anyhow::Error> {
        use byteorder::{ByteOrder, LittleEndian};
        Ok(LittleEndian::read_u32(buf))
    }
}

#[derive(Debug, Clone)]
pub struct PointerMatcher {
    /// Sorted memory regions.
    memory_regions: Vec<process::MemoryInformation>,
}

impl PointerMatcher {
    pub fn test(
        &self,
        _: Type,
        _: &Value,
        proxy: process::AddressProxy<'_>,
    ) -> anyhow::Result<bool> {
        let value = proxy.eval(Type::U64)?;

        let address = match value {
            Value::U64(value) => Address::new(value),
            _ => anyhow::bail!("bad value when decoding address"),
        };

        Ok(
            match AddressRange::find_in_range(&self.memory_regions, |m| &m.range, address) {
                Ok(Some(_)) => true,
                _ => false,
            },
        )
    }
}

impl fmt::Display for PointerMatcher {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "pointer")
    }
}
