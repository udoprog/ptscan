use std::{convert::TryFrom, ffi::OsString, fmt, io, slice, sync::Arc};

use crate::{
    error::Error, module, platform::RawProcess, Address, AddressRange, Endianness, PointerInfo,
    ProcessId, Size, Type,
};

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PointerWidth {
    Pointer32,
    Pointer64,
}

impl PointerWidth {
    /// The size of the pointer in bytes.
    pub fn size(self) -> usize {
        match self {
            Self::Pointer32 => 4,
            Self::Pointer64 => 8,
        }
    }

    /// Convert into a pointer type.
    pub fn into_type(self) -> Type {
        Type::Pointer(self)
    }

    /// Decode the given buffer as a pointer.
    pub fn decode_pointer<B>(&self, buf: &[u8]) -> Address
    where
        B: byteorder::ByteOrder,
    {
        assert!(
            buf.len() == self.size(),
            "{} (buffer length) != {} (pointer width)",
            buf.len(),
            self.size()
        );

        match self {
            Self::Pointer64 => {
                let address = B::read_u64(buf);
                Address(address)
            }
            Self::Pointer32 => {
                let address = B::read_u32(buf);
                Address(address as u64)
            }
        }
    }

    /// Encode a pointer into the specified buffer.
    ///
    /// Returns a boolean indicating if the pointer could be encoded.
    pub fn encode_pointer<B>(&self, buf: &mut [u8], value: u64) -> bool
    where
        B: byteorder::ByteOrder,
    {
        match self {
            Self::Pointer64 => B::write_u64(buf, value),
            Self::Pointer32 => {
                let value = match u32::try_from(value) {
                    Ok(value) => value,
                    Err(..) => return false,
                };

                B::write_u32(buf, value);
            }
        }

        true
    }

    /// # Safety
    ///
    /// `ptr` must be valid for writes of a pointer of this width.
    pub unsafe fn write_unchecked(self, ptr: *mut u8, value: Address) {
        use std::ptr::write;

        match self {
            Self::Pointer32 => write(ptr as *mut _, u64::from(value) as u32),
            Self::Pointer64 => write(ptr as *mut _, value),
        }
    }

    /// # Safety
    ///
    /// `ptr` must be valid for reads of a pointer of this width.
    pub unsafe fn read_unchecked(self, ptr: *const u8) -> Address {
        use std::ptr::read;

        Address(match self {
            Self::Pointer32 => read(ptr as *const u32) as u64,
            Self::Pointer64 => read(ptr as *const _),
        })
    }
}

impl fmt::Display for PointerWidth {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Pointer32 => write!(fmt, "pointer32"),
            Self::Pointer64 => write!(fmt, "pointer64"),
        }
    }
}

pub trait MemoryReader {
    type ByteOrder: byteorder::ByteOrder;

    fn read_memory(&self, address: Address, buf: &mut [u8]) -> anyhow::Result<usize>;
}

impl MemoryReader for Process {
    type ByteOrder = byteorder::NativeEndian;

    fn read_memory(&self, address: Address, buf: &mut [u8]) -> anyhow::Result<usize> {
        Ok(self.read_process_memory(address, buf)?)
    }
}

pub trait ProcessInfo {
    type Process: ProcessInfo;
    type ByteOrder: 'static + Send + Sync + byteorder::ByteOrder;
    type PointerInfo: PointerInfo;

    fn process(&self) -> &Self::Process;

    fn pointer_info(&self) -> &Self::PointerInfo;

    /// Access information on virtual memory for the given address.
    fn virtual_query(&self, address: Address) -> anyhow::Result<Option<MemoryInformation>>;

    /// Every virtual memory region in address order, when the platform lists
    /// them more cheaply than walking with [`virtual_query`](Self::virtual_query).
    fn virtual_memory_snapshot(&self) -> anyhow::Result<Option<Vec<MemoryInformation>>> {
        Ok(None)
    }

    /// Access virtual memory regions of the process.
    fn virtual_memory_regions(&self) -> VirtualMemoryRegions<'_, Self::Process> {
        VirtualMemoryRegions {
            process: self.process(),
            walk: Walk::Start,
        }
    }
}

/// A process handle.
#[derive(Clone)]
pub struct Process {
    pub process_id: ProcessId,
    pub is_64bit: bool,
    pub pointer_width: PointerWidth,
    pub endianness: Endianness,
    pub(crate) handle: Arc<RawProcess>,
}

impl Process {
    pub fn builder() -> OpenProcessBuilder {
        OpenProcessBuilder::default()
    }

    /// Get the name of the process.
    pub fn module_base_name(&self) -> io::Result<OsString> {
        self.handle.module_base_name()
    }

    /// Get the process ID.
    pub fn process_id(&self) -> ProcessId {
        self.process_id
    }

    /// Enumerate all modules.
    pub fn modules(&self) -> Result<Vec<module::Module>, Error> {
        self.handle.modules()
    }

    /// Read the given structure from the given address.
    ///
    /// This is unsafe since it reinteprets the given memory region as a type, which might not be correct.
    ///
    /// # Safety
    ///
    /// The type `T` must be zeroable and support being punted from process memory.
    pub unsafe fn read<T>(&self, address: Address) -> Result<T, Error> {
        use std::mem;

        let mut out: T = mem::zeroed();
        let buf = slice::from_raw_parts_mut(&mut out as *mut T as *mut u8, mem::size_of::<T>());

        let len = self.read_process_memory(address, buf)?;

        if len != buf.len() {
            return Err(Error::ReadUnderflow);
        }

        Ok(out)
    }

    /// Write to process memory at the specified address.
    pub fn write_process_memory(&self, address: Address, buffer: &[u8]) -> Result<bool, Error> {
        self.handle.write_memory(address, buffer)
    }

    /// Read process memory at the specified address.
    ///
    /// Returns `true` if the region was successfully read.
    /// Returns `false` if there was not enough space in the region that we attempted to read.
    pub fn read_process_memory(&self, address: Address, buffer: &mut [u8]) -> Result<usize, Error> {
        self.handle.read_memory(address, buffer)
    }

    /// Retrieve information about the memory page that corresponds to the given virtual `address`.
    pub fn virtual_query(&self, address: Address) -> anyhow::Result<Option<MemoryInformation>> {
        self.handle.virtual_query(address)
    }

    /// Suspend the process.
    pub fn suspend(&self) -> Result<(), Error> {
        self.handle.suspend()
    }

    /// Resume the process.
    pub fn resume(&self) -> Result<(), Error> {
        self.handle.resume()
    }
}

impl PointerInfo for Process {
    type ByteOrder = <Process as ProcessInfo>::ByteOrder;

    fn width(&self) -> PointerWidth {
        self.pointer_width
    }
}

impl ProcessInfo for Process {
    type Process = Process;
    type PointerInfo = Process;
    type ByteOrder = byteorder::NativeEndian;

    fn process(&self) -> &Self::Process {
        self
    }

    fn pointer_info(&self) -> &Self::PointerInfo {
        self
    }

    fn virtual_query(&self, address: Address) -> anyhow::Result<Option<MemoryInformation>> {
        Process::virtual_query(self, address)
    }

    fn virtual_memory_snapshot(&self) -> anyhow::Result<Option<Vec<MemoryInformation>>> {
        self.handle.memory_regions()
    }
}

impl fmt::Debug for Process {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Process")
            .field("process_id", &self.process_id)
            .finish()
    }
}

/// Builder to open a process.
#[derive(Default)]
#[cfg_attr(not(windows), allow(dead_code))]
pub struct OpenProcessBuilder {
    pub(crate) all_access: bool,
    pub(crate) query_information: bool,
    pub(crate) vm_read: bool,
    pub(crate) inherit_handles: bool,
}

impl OpenProcessBuilder {
    /// Open process with all access.
    pub fn all_access(self) -> Self {
        OpenProcessBuilder {
            all_access: true,
            ..self
        }
    }

    /// Set the query information flag.
    pub fn query_information(self) -> Self {
        OpenProcessBuilder {
            query_information: true,
            ..self
        }
    }

    /// Set the VM Read flag.
    pub fn vm_read(self) -> Self {
        OpenProcessBuilder {
            vm_read: true,
            ..self
        }
    }

    /// Indicate that the process should inherit handles.
    pub fn inherit_handles(self) -> Self {
        OpenProcessBuilder {
            inherit_handles: true,
            ..self
        }
    }

    /// Build the process handle.
    pub fn build(self, process_id: ProcessId) -> Result<Process, Error> {
        let handle = RawProcess::open(&self, process_id)?;
        let is_64bit = handle.is_64bit()?;
        let handle = Arc::new(handle);

        let pointer_width = if is_64bit {
            PointerWidth::Pointer64
        } else {
            PointerWidth::Pointer32
        };
        let endianness = Endianness::LittleEndian;

        Ok(Process {
            process_id,
            is_64bit,
            pointer_width,
            endianness,
            handle,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MemoryState {
    Commit,
    Free,
    Reserve,
}

impl MemoryState {
    /// Test if the memory state is free (i.e. unused).
    pub fn is_free(self) -> bool {
        matches!(self, MemoryState::Free)
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::Commit => "MEM_COMMIT",
            Self::Free => "MEM_FREE",
            Self::Reserve => "MEM_RESERVE",
        }
    }
}

impl fmt::Display for MemoryState {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(fmt)
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MemoryType {
    Image,
    Mapped,
    Private,
}

impl MemoryType {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Image => "MEM_IMAGE",
            Self::Mapped => "MEM_MAPPED",
            Self::Private => "MEM_PRIVATE",
        }
    }
}

impl fmt::Display for MemoryType {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(fmt)
    }
}

#[derive(fixed_map::Key, Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MemoryProtect {
    Execute,
    ExecuteRead,
    ExecuteReadWrite,
    ExecuteWriteCopy,
    NoAccess,
    ReadOnly,
    ReadWrite,
    WriteCopy,
    TargetsInvalid,
    TargetsNoUpdate,
    Guard,
    NoCache,
    WriteCombine,
}

macro_rules! as_str {
    ($slf:expr, {$($flag_from:ident => $flag_to:ident,)*}) => {
        match $slf {
            $(Self::$flag_from => stringify!($flag_to),)*
        }
    }
}

impl MemoryProtect {
    /// Get the string representation of this proection mode.
    pub fn as_str(self) -> &'static str {
        as_str!(self, {
            Execute => PAGE_EXECUTE,
            ExecuteRead => PAGE_EXECUTE_READ,
            ExecuteReadWrite => PAGE_EXECUTE_READWRITE,
            ExecuteWriteCopy => PAGE_EXECUTE_WRITECOPY,
            NoAccess => PAGE_NOACCESS,
            ReadOnly => PAGE_READONLY,
            ReadWrite => PAGE_READWRITE,
            WriteCopy => PAGE_WRITECOPY,
            TargetsInvalid => PAGE_TARGETS_INVALID,
            TargetsNoUpdate => PAGE_TARGETS_NO_UPDATE,
            Guard => PAGE_GUARD,
            NoCache => PAGE_NOCACHE,
            WriteCombine => PAGE_WRITECOMBINE,
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryInformation {
    pub range: AddressRange,
    pub state: MemoryState,
    #[serde(rename = "type", default, skip_serializing_if = "Option::is_none")]
    pub ty: Option<MemoryType>,
    pub protect: fixed_map::Set<MemoryProtect>,
}

impl MemoryInformation {
    /// Test if region is writable.
    pub fn is_readable(&self) -> bool {
        use self::MemoryProtect::*;

        self.state == MemoryState::Commit
            && !self.is_protect_any(&[NoAccess, TargetsInvalid, TargetsNoUpdate, Guard, NoCache])
    }

    /// Test if region is writable.
    pub fn is_writable(&self) -> bool {
        use self::MemoryProtect::*;
        self.is_protect_any(&[ReadWrite, WriteCopy, ExecuteReadWrite, ExecuteWriteCopy])
    }

    /// Test if region is executable.
    pub fn is_executable(&self) -> bool {
        use self::MemoryProtect::*;
        self.is_protect_any(&[Execute, ExecuteRead, ExecuteReadWrite, ExecuteWriteCopy])
    }

    /// Test if region is executable.
    pub fn is_copy_on_write(&self) -> bool {
        use self::MemoryProtect::*;
        self.state == MemoryState::Commit && self.is_protect_any(&[WriteCopy, ExecuteWriteCopy])
    }

    /// Test if protection contains any of the specified elements.
    fn is_protect_any<'a>(&self, it: impl IntoIterator<Item = &'a MemoryProtect>) -> bool {
        it.into_iter().any(|p| self.protect.contains(*p))
    }
}

impl fmt::Display for MemoryInformation {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{} {}", self.range, self.state)?;

        if let Some(ty) = self.ty {
            write!(fmt, " {}", ty)?;
        }

        let protect = self.protect.iter().map(|p| p.as_str()).collect::<Vec<_>>();

        if !protect.is_empty() {
            write!(fmt, "{}", protect.join(","))?;
        }

        Ok(())
    }
}

/// Iterator over virtual memory regions.
pub struct VirtualMemoryRegions<'a, P> {
    process: &'a P,
    walk: Walk,
}

enum Walk {
    Start,
    Snapshot(std::vec::IntoIter<MemoryInformation>),
    Query(Address),
}

impl<'a, P> Iterator for VirtualMemoryRegions<'a, P>
where
    P: ProcessInfo,
{
    type Item = anyhow::Result<MemoryInformation>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match &mut self.walk {
                Walk::Start => {
                    self.walk = match try_iter!(self.process.virtual_memory_snapshot()) {
                        Some(regions) => Walk::Snapshot(regions.into_iter()),
                        None => Walk::Query(Address::null()),
                    };
                }
                Walk::Snapshot(regions) => return regions.next().map(Ok),
                Walk::Query(current) => {
                    let m = try_iter!(self.process.virtual_query(*current))?;
                    *current = current
                        .checked_size(m.range.size)?
                        .checked_size(Size::new(1))?;
                    return Some(Ok(m));
                }
            }
        }
    }
}
