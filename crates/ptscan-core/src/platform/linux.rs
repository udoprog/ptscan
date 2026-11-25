//! Linux support through `/proc` and `process_vm_readv`/`process_vm_writev`.

use std::{
    collections::HashSet,
    convert::{TryFrom, TryInto},
    ffi::{OsStr, OsString},
    fs, io,
    os::unix::{ffi::OsStrExt, fs::FileExt},
    path::{Path, PathBuf},
};

use crate::{
    error::Error,
    module::ModuleInfo,
    process::{MemoryInformation, MemoryProtect, MemoryState, MemoryType, OpenProcessBuilder},
    system, Address, AddressRange, Process, ProcessId, Size, ThreadId,
};

fn invalid_data(message: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message.to_owned())
}

/// A process identified by its pid, accessed through `/proc/<pid>`.
pub(crate) struct RawProcess {
    process_id: ProcessId,
}

impl RawProcess {
    pub(crate) fn open(_: &OpenProcessBuilder, process_id: ProcessId) -> Result<Self, Error> {
        let raw = Self { process_id };

        if !fs::metadata(raw.proc_path(""))?.is_dir() {
            return Err(io::Error::from(io::ErrorKind::NotFound).into());
        }

        Ok(raw)
    }

    fn proc_path(&self, file: &str) -> PathBuf {
        Path::new("/proc")
            .join(self.process_id.to_string())
            .join(file)
    }

    pub(crate) fn is_64bit(&self) -> Result<bool, Error> {
        match ElfHeader::read(&self.proc_path("exe"))?.class {
            1 => Ok(false),
            2 => Ok(true),
            _ => Err(invalid_data("unknown ELF class").into()),
        }
    }

    pub(crate) fn module_base_name(&self) -> io::Result<OsString> {
        if let Ok(exe) = fs::read_link(self.proc_path("exe")) {
            if let Some(name) = exe.file_name() {
                return Ok(name.to_owned());
            }
        }

        let comm = fs::read(self.proc_path("comm"))?;
        let comm = comm.strip_suffix(b"\n").unwrap_or(&comm);
        Ok(OsStr::from_bytes(comm).to_owned())
    }

    pub(crate) fn modules(&self) -> Result<Vec<Module>, Error> {
        let maps = self.maps()?;
        let mut modules = Vec::<Module>::new();

        for map in &maps {
            let path = match &map.path {
                Some(path) if map.execute => path,
                _ => continue,
            };

            if modules.iter().any(|m| m.path == *path) {
                continue;
            }

            let (base, end) = maps
                .iter()
                .filter(|m| m.path.as_ref() == Some(path))
                .fold((map.start, map.end), |(base, end), m| {
                    (base.min(m.start), end.max(m.end))
                });

            modules.push(Module {
                path: path.clone(),
                base,
                end,
            });
        }

        modules.sort_by_key(|m| m.base);
        Ok(modules)
    }

    /// Write all of `buffer` at `address`, returning `false` if only part of
    /// it (or none) is mapped writable.
    pub(crate) fn write_memory(&self, address: Address, buffer: &[u8]) -> Result<bool, Error> {
        if buffer.is_empty() {
            return Ok(true);
        }

        let remote = usize::try_from(address.0).map_err(|_| Error::PointerConversionError)?;

        let local = libc::iovec {
            iov_base: buffer.as_ptr() as *mut libc::c_void,
            iov_len: buffer.len(),
        };

        let remote = libc::iovec {
            iov_base: remote as *mut libc::c_void,
            iov_len: buffer.len(),
        };

        // SAFETY: `local` covers exactly `buffer`, which is only read from.
        let n = unsafe {
            libc::process_vm_writev(self.process_id as libc::pid_t, &local, 1, &remote, 1, 0)
        };

        if n >= 0 && n as usize == buffer.len() {
            return Ok(true);
        }

        if n < 0 {
            let e = io::Error::last_os_error();

            if !matches!(e.raw_os_error(), Some(libc::EFAULT) | Some(libc::ENOSYS)) {
                return Err(e.into());
            }
        }

        // process_vm_writev stops at read-only pages, which `/proc/<pid>/mem`
        // writes through like WriteProcessMemory does.
        self.write_proc_mem(address.0, buffer)
    }

    fn write_proc_mem(&self, address: u64, buffer: &[u8]) -> Result<bool, Error> {
        let file = fs::OpenOptions::new()
            .write(true)
            .open(self.proc_path("mem"))?;
        let mut written = 0;

        while written < buffer.len() {
            match file.write_at(&buffer[written..], address + written as u64) {
                Ok(0) => break,
                Ok(n) => written += n,
                Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
                Err(e) if matches!(e.raw_os_error(), Some(libc::EIO) | Some(libc::EFAULT)) => break,
                Err(e) => return Err(e.into()),
            }
        }

        Ok(written == buffer.len())
    }

    /// Read as much of `buffer` as is mapped from `address` onwards, returning
    /// the number of bytes read; `0` if `address` itself is unmapped.
    pub(crate) fn read_memory(&self, address: Address, buffer: &mut [u8]) -> Result<usize, Error> {
        if buffer.is_empty() {
            return Ok(0);
        }

        let remote = usize::try_from(address.0).map_err(|_| Error::PointerConversionError)?;

        let local = libc::iovec {
            iov_base: buffer.as_mut_ptr().cast(),
            iov_len: buffer.len(),
        };

        let remote = libc::iovec {
            iov_base: remote as *mut libc::c_void,
            iov_len: buffer.len(),
        };

        // SAFETY: `local` covers exactly `buffer`, which we hold mutably.
        let n = unsafe {
            libc::process_vm_readv(self.process_id as libc::pid_t, &local, 1, &remote, 1, 0)
        };

        if n >= 0 {
            return Ok(n as usize);
        }

        let e = io::Error::last_os_error();

        match e.raw_os_error() {
            Some(libc::EFAULT) => Ok(0),
            Some(libc::ENOSYS) => self.read_proc_mem(address.0, buffer),
            _ => Err(e.into()),
        }
    }

    fn read_proc_mem(&self, address: u64, buffer: &mut [u8]) -> Result<usize, Error> {
        let file = fs::File::open(self.proc_path("mem"))?;
        let mut read = 0;

        while read < buffer.len() {
            match file.read_at(&mut buffer[read..], address + read as u64) {
                Ok(0) => break,
                Ok(n) => read += n,
                Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
                Err(e) if matches!(e.raw_os_error(), Some(libc::EIO) | Some(libc::EFAULT)) => break,
                Err(e) => return Err(e.into()),
            }
        }

        Ok(read)
    }

    /// Query the region containing `address`, which is a free region when it
    /// lies between mappings, or `None` past the last mapping.
    pub(crate) fn virtual_query(
        &self,
        address: Address,
    ) -> anyhow::Result<Option<MemoryInformation>> {
        let maps = self.maps()?;
        let address = address.0;

        let index = match maps.iter().position(|m| address < m.end) {
            Some(index) => index,
            None => return Ok(None),
        };

        let map = &maps[index];

        if address < map.start {
            let start = index.checked_sub(1).map(|i| maps[i].end).unwrap_or(0);
            return Ok(Some(free(start, map.start)));
        }

        Ok(Some(map.information(&image_paths(&maps))))
    }

    /// Every region from address zero in order, the same as walking with
    /// [`virtual_query`](Self::virtual_query), from a single read of the maps.
    pub(crate) fn memory_regions(&self) -> anyhow::Result<Option<Vec<MemoryInformation>>> {
        let maps = self.maps()?;
        let images = image_paths(&maps);
        let mut regions = Vec::with_capacity(maps.len() * 2);
        let mut end = 0;

        for map in &maps {
            if end < map.start {
                regions.push(free(end, map.start));
            }

            regions.push(map.information(&images));
            end = map.end;
        }

        Ok(Some(regions))
    }

    fn maps(&self) -> io::Result<Vec<Map>> {
        let maps = fs::read(self.proc_path("maps"))?;

        maps.split(|&b| b == b'\n')
            .filter(|line| !line.is_empty())
            .map(Map::parse)
            .collect()
    }

    pub(crate) fn suspend(&self) -> Result<(), Error> {
        self.signal(libc::SIGSTOP)
    }

    pub(crate) fn resume(&self) -> Result<(), Error> {
        self.signal(libc::SIGCONT)
    }

    fn signal(&self, signal: libc::c_int) -> Result<(), Error> {
        // SAFETY: kill only takes plain integers.
        if unsafe { libc::kill(self.process_id as libc::pid_t, signal) } != 0 {
            return Err(Error::last_system_error());
        }

        Ok(())
    }

    /// The mapping holding the stack of thread `tid`, found from the stack
    /// pointer it is blocked with, or the `[stack]` mapping for the main
    /// thread when it is running.
    fn thread_stack(&self, tid: ThreadId) -> Result<AddressRange, Error> {
        let path = self.proc_path("task").join(tid.to_string()).join("syscall");
        let maps = fs::read(self.proc_path("maps"))?;
        let mut lines = maps.split(|&b| b == b'\n').filter(|l| !l.is_empty());

        let map = match stack_pointer(&fs::read(path)?)? {
            Some(sp) => lines
                .map(Map::parse)
                .find(|m| m.as_ref().map_or(true, |m| m.start <= sp && sp < m.end)),
            None if tid == self.process_id => {
                lines.find(|l| l.ends_with(b" [stack]")).map(Map::parse)
            }
            None => None,
        };

        let map = map.ok_or(Error::Unsupported)??;
        Ok(range(map.start, map.end))
    }
}

/// The stack pointer in `/proc/<pid>/task/<tid>/syscall`, which is the
/// second to last field unless the thread is running.
fn stack_pointer(syscall: &[u8]) -> io::Result<Option<u64>> {
    let bad = || invalid_data("malformed /proc/<pid>/task/<tid>/syscall");
    let syscall = std::str::from_utf8(syscall).map_err(|_| bad())?.trim();

    if syscall == "running" {
        return Ok(None);
    }

    let fields = syscall.split(' ').collect::<Vec<_>>();

    let sp = match fields.len().checked_sub(2) {
        Some(index) if index > 0 => fields[index],
        _ => return Err(bad()),
    };

    let sp = sp.strip_prefix("0x").ok_or_else(bad)?;
    Ok(Some(u64::from_str_radix(sp, 16).map_err(|_| bad())?))
}

fn free(start: u64, end: u64) -> MemoryInformation {
    MemoryInformation {
        range: range(start, end),
        state: MemoryState::Free,
        ty: None,
        protect: fixed_map::Set::new(),
    }
}

/// Files with an executable mapping, whose every mapping is part of an image.
fn image_paths(maps: &[Map]) -> HashSet<&Path> {
    maps.iter()
        .filter(|m| m.execute)
        .filter_map(|m| m.path.as_deref())
        .collect()
}

fn range(start: u64, end: u64) -> AddressRange {
    AddressRange {
        base: Address::new(start),
        size: Size::new(end - start),
    }
}

/// A line of `/proc/<pid>/maps`.
#[derive(Debug, PartialEq, Eq)]
struct Map {
    start: u64,
    end: u64,
    read: bool,
    write: bool,
    execute: bool,
    shared: bool,
    /// Backing file, for mappings with an inode.
    path: Option<PathBuf>,
}

impl Map {
    /// Parse `start-end perms offset dev inode [path]`, where the path may
    /// contain spaces.
    fn parse(line: &[u8]) -> io::Result<Self> {
        let bad = || invalid_data("malformed /proc/<pid>/maps line");

        let mut rest = line;
        let mut field = || {
            let s = rest.iter().position(|b| *b != b' ').unwrap_or(rest.len());
            rest = &rest[s..];
            let e = rest.iter().position(|b| *b == b' ').unwrap_or(rest.len());
            let (field, tail) = rest.split_at(e);
            rest = tail;
            std::str::from_utf8(field).map_err(|_| bad())
        };

        let (start, end) = field()?.split_once('-').ok_or_else(bad)?;
        let start = u64::from_str_radix(start, 16).map_err(|_| bad())?;
        let end = u64::from_str_radix(end, 16).map_err(|_| bad())?;
        let perms = field()?.as_bytes();
        let _offset = field()?;
        let _dev = field()?;
        let inode = field()?.parse::<u64>().map_err(|_| bad())?;

        if perms.len() != 4 || start > end {
            return Err(bad());
        }

        let path = rest.trim_ascii_start();
        let path = (inode != 0 && !path.is_empty()).then(|| PathBuf::from(OsStr::from_bytes(path)));

        Ok(Self {
            start,
            end,
            read: perms[0] == b'r',
            write: perms[1] == b'w',
            execute: perms[2] == b'x',
            shared: perms[3] == b's',
            path,
        })
    }

    fn information(&self, images: &HashSet<&Path>) -> MemoryInformation {
        let ty = match &self.path {
            Some(path) if images.contains(path.as_path()) => MemoryType::Image,
            _ if self.path.is_some() || self.shared => MemoryType::Mapped,
            _ => MemoryType::Private,
        };

        let mut protect = fixed_map::Set::new();
        protect.insert(self.protect());

        MemoryInformation {
            range: range(self.start, self.end),
            state: MemoryState::Commit,
            ty: Some(ty),
            protect,
        }
    }

    fn protect(&self) -> MemoryProtect {
        match (self.read || self.write, self.write, self.execute) {
            (_, true, true) => MemoryProtect::ExecuteReadWrite,
            (true, false, true) => MemoryProtect::ExecuteRead,
            (false, false, true) => MemoryProtect::Execute,
            (_, true, false) => MemoryProtect::ReadWrite,
            (true, false, false) => MemoryProtect::ReadOnly,
            (false, false, false) => MemoryProtect::NoAccess,
        }
    }
}

/// The fields of an ELF header that ptscan needs.
struct ElfHeader {
    class: u8,
    /// `ET_DYN` objects have an entry point relative to their load address.
    relocatable: bool,
    entry: u64,
}

impl ElfHeader {
    fn read(path: &Path) -> io::Result<Self> {
        use std::io::Read as _;

        let mut header = [0u8; 32];
        fs::File::open(path)?.read_exact(&mut header)?;

        if &header[..4] != b"\x7fELF" {
            return Err(invalid_data("not an ELF file"));
        }

        let class = header[4];
        let little = match header[5] {
            1 => true,
            2 => false,
            _ => return Err(invalid_data("unknown ELF data encoding")),
        };

        let u16_at = |at: usize| {
            let b = [header[at], header[at + 1]];
            if little {
                u16::from_le_bytes(b)
            } else {
                u16::from_be_bytes(b)
            }
        };

        let entry = match class {
            1 => {
                let b = header[24..28].try_into().expect("4 bytes");
                u64::from(if little {
                    u32::from_le_bytes(b)
                } else {
                    u32::from_be_bytes(b)
                })
            }
            _ => {
                let b = header[24..32].try_into().expect("8 bytes");
                if little {
                    u64::from_le_bytes(b)
                } else {
                    u64::from_be_bytes(b)
                }
            }
        };

        Ok(Self {
            class,
            relocatable: u16_at(16) == 3,
            entry,
        })
    }
}

pub(crate) fn process_unavailable(error: &Error) -> bool {
    match error {
        Error::System(e) => matches!(
            e.kind(),
            io::ErrorKind::NotFound | io::ErrorKind::PermissionDenied
        ),
        _ => false,
    }
}

pub fn processes() -> Result<Vec<ProcessId>, Error> {
    let mut processes = Vec::new();

    for entry in fs::read_dir("/proc")? {
        if let Some(pid) = entry?.file_name().to_str().and_then(|s| s.parse().ok()) {
            processes.push(pid);
        }
    }

    processes.sort_unstable();
    Ok(processes)
}

/// Threads of `process_id`, from `/proc/<pid>/task`.
pub(crate) fn threads(process_id: ProcessId) -> Result<Vec<system::Thread>, Error> {
    let mut threads = Vec::new();

    for entry in fs::read_dir(Path::new("/proc").join(process_id.to_string()).join("task"))? {
        if let Some(tid) = entry?.file_name().to_str().and_then(|s| s.parse().ok()) {
            threads.push(system::Thread::new(process_id, tid));
        }
    }

    threads.sort_unstable_by_key(|t| t.thread_id());
    Ok(threads)
}

/// A file mapped into a process with at least one executable mapping.
#[derive(Debug)]
pub struct Module {
    path: PathBuf,
    base: u64,
    end: u64,
}

impl Module {
    /// Get the name of the module.
    pub fn name(&self, _: &Process) -> io::Result<OsString> {
        self.path
            .file_name()
            .map(OsStr::to_owned)
            .ok_or_else(|| invalid_data("module path has no file name"))
    }

    /// Get the file name corresponding to the module.
    pub fn file_name(&self) -> io::Result<OsString> {
        Ok(self.path.clone().into_os_string())
    }

    /// Get the information about the module. The entry point is the base when
    /// the backing file cannot be read.
    pub fn info(&self, _: &Process) -> io::Result<ModuleInfo> {
        let entry_point = match ElfHeader::read(&self.path) {
            Ok(h) if h.relocatable => self.base.wrapping_add(h.entry),
            Ok(h) => h.entry,
            Err(_) => self.base,
        };

        Ok(ModuleInfo {
            base_of_dll: Address::new(self.base),
            size_of_image: Size::new(self.end - self.base),
            entry_point: Address::new(entry_point),
        })
    }
}

#[derive(Clone)]
pub struct Thread {
    thread_id: ThreadId,
}

impl Thread {
    pub fn builder() -> OpenThreadBuilder {
        OpenThreadBuilder::default()
    }

    pub fn open(thread_id: ThreadId) -> Result<Thread, Error> {
        Self::builder().build(thread_id)
    }

    pub fn thread_id(&self) -> ThreadId {
        self.thread_id
    }

    /// The mapping holding the thread's stack. [`Error::Unsupported`] if it
    /// cannot be found, which is when a thread other than the main thread is
    /// running rather than blocked.
    pub fn thread_stack(&self, process: &Process) -> anyhow::Result<AddressRange> {
        Ok(process.handle.thread_stack(self.thread_id)?)
    }
}

#[derive(Default)]
pub struct OpenThreadBuilder {
    _private: (),
}

impl OpenThreadBuilder {
    pub fn all_access(self) -> Self {
        self
    }

    pub fn get_context(self) -> Self {
        self
    }

    pub fn query_information(self) -> Self {
        self
    }

    pub fn inherit_handles(self) -> Self {
        self
    }

    pub fn build(self, thread_id: ThreadId) -> Result<Thread, Error> {
        // Threads are hidden from listing `/proc` but reachable by their id.
        if !fs::metadata(Path::new("/proc").join(thread_id.to_string()))?.is_dir() {
            return Err(io::Error::from(io::ErrorKind::NotFound).into());
        }

        Ok(Thread { thread_id })
    }
}

#[cfg(test)]
mod tests {
    use std::{
        io::{BufRead, BufReader, Write},
        process::{Child, Command, Stdio},
        sync::atomic::AtomicU64,
        time::Duration,
    };

    use super::*;
    use crate::{
        process::ProcessInfo, Addresses, BlockAddresses, FilterExpr, Offset, Pointer, PointerScan,
        Process, ProcessHandle, ScanContext, ScanProgress, ScanResults, Sign, Token, Type, Value,
        ValueExpr, Values,
    };

    /// Kills the scratch child when dropped, so a failing test cleans up.
    struct Scratch(Child);

    impl Drop for Scratch {
        fn drop(&mut self) {
            let _ = self.0.kill();
            let _ = self.0.wait();
        }
    }

    /// A `sleep` child, returned once the loader has mapped libc.
    fn scratch() -> Scratch {
        let child = Scratch(
            Command::new("sleep")
                .arg("30")
                .spawn()
                .expect("spawn sleep"),
        );

        let maps = Path::new("/proc")
            .join(child.0.id().to_string())
            .join("maps");

        for _ in 0..500 {
            if fs::read_to_string(&maps).is_ok_and(|m| m.contains("/libc")) {
                return child;
            }

            std::thread::sleep(Duration::from_millis(10));
        }

        panic!("sleep never mapped libc");
    }

    #[test]
    fn parse_maps_lines() {
        let map = Map::parse(
            b"7f12a000-7f12b000 r-xp 00001000 fd:01 1234   /usr/lib/with space.so (deleted)",
        )
        .unwrap();

        assert_eq!(
            map,
            Map {
                start: 0x7f12a000,
                end: 0x7f12b000,
                read: true,
                write: false,
                execute: true,
                shared: false,
                path: Some(PathBuf::from("/usr/lib/with space.so (deleted)")),
            }
        );
        assert!(matches!(map.protect(), MemoryProtect::ExecuteRead));

        let heap = Map::parse(b"55d0e000-55d2f000 rw-p 00000000 00:00 0      [heap]").unwrap();
        assert_eq!(heap.path, None);
        assert!(matches!(heap.protect(), MemoryProtect::ReadWrite));

        let anon = Map::parse(b"7f000000-7f001000 ---p 00000000 00:00 0").unwrap();
        assert_eq!(anon.path, None);
        assert!(matches!(anon.protect(), MemoryProtect::NoAccess));

        assert!(Map::parse(b"zz-10 r--p 0 0 0").is_err());
    }

    #[test]
    fn parse_syscall_stack_pointer() {
        let blocked = b"0 0x3 0x7ffd1000 0x1 0x0 0x0 0x0 0x7ffc2a10 0x7f0011223344\n";
        assert_eq!(stack_pointer(blocked).unwrap(), Some(0x7ffc2a10));
        assert_eq!(
            stack_pointer(b"-1 0x7ffc2a10 0x7f00\n").unwrap(),
            Some(0x7ffc2a10)
        );
        assert_eq!(stack_pointer(b"running\n").unwrap(), None);
        assert!(stack_pointer(b"0x7ffc2a10\n").is_err());
    }

    #[test]
    fn processes_include_self_and_child() {
        let child = scratch();
        let processes = processes().unwrap();
        assert!(processes.contains(&std::process::id()));
        assert!(processes.contains(&child.0.id()));
    }

    #[test]
    fn open_missing_process_is_unavailable() {
        // Larger than any pid_max the kernel allows.
        let e = Process::builder().build(u32::MAX).err().unwrap();
        assert!(process_unavailable(&e));
    }

    #[test]
    fn scratch_child_regions_and_modules() {
        let child = scratch();
        let process = Process::builder().build(child.0.id()).unwrap();

        assert_eq!(process.is_64bit, cfg!(target_pointer_width = "64"));
        assert_eq!(process.module_base_name().unwrap(), "sleep");

        let regions = process
            .virtual_memory_regions()
            .collect::<anyhow::Result<Vec<_>>>()
            .unwrap();

        let mut previous_end = 0;

        for region in &regions {
            assert_eq!(region.range.base.0, previous_end, "regions are contiguous");
            previous_end = region.range.base.0 + region.range.size.into_inner();
        }

        assert!(regions.iter().any(|r| r.state == MemoryState::Commit
            && matches!(r.ty, Some(MemoryType::Image))
            && r.is_executable()));
        assert!(regions
            .iter()
            .any(|r| matches!(r.ty, Some(MemoryType::Private)) && r.is_writable()));

        let modules = process.modules().unwrap();
        let names = modules
            .iter()
            .map(|m| m.name(&process).unwrap())
            .collect::<Vec<_>>();
        assert!(names.iter().any(|n| n == "sleep"), "{:?}", names);
        assert!(
            names
                .iter()
                .any(|n| n.to_string_lossy().starts_with("libc")),
            "{:?}",
            names
        );

        for module in &modules {
            let info = module.info(&process).unwrap();
            let region = process.virtual_query(info.base_of_dll).unwrap().unwrap();
            assert_eq!(region.range.base, info.base_of_dll);
            assert!(matches!(region.ty, Some(MemoryType::Image)));
            assert!(info.entry_point.0 >= info.base_of_dll.0);
            assert!(info.entry_point.0 < info.base_of_dll.0 + info.size_of_image.into_inner());
        }
    }

    #[test]
    fn own_process_modules() {
        let process = Process::builder().build(std::process::id()).unwrap();
        let exe = std::env::current_exe().unwrap();
        let modules = process.modules().unwrap();
        assert!(modules
            .iter()
            .any(|m| m.file_name().unwrap() == exe.as_os_str()));
    }

    const HOLD_ENV: &str = "PTSCAN_TEST_HOLD_VALUE";
    const HELD: u64 = 0x5eed_c0de_f00d_7a11;
    static HELD_PAIR: [AtomicU64; 2] = [AtomicU64::new(HELD), AtomicU64::new(HELD)];
    /// `HELD` in the image's read-only data, which value scans skip.
    static HELD_READ_ONLY: u64 = HELD;
    /// Words in the image's writable data and on the heap that tests write
    /// pointer graphs into.
    const GRAPH_WORDS: usize = 32;
    static HELD_GRAPH: [AtomicU64; GRAPH_WORDS] = [const { AtomicU64::new(0) }; GRAPH_WORDS];

    /// Run in a re-executed test binary by [`Holder::spawn`]: keeps `HELD` and
    /// a pointer to it on the heap, prints their addresses, that of
    /// `HELD_PAIR` in the image's writable data and that of `HELD_READ_ONLY`,
    /// then answers each line on stdin with the value it now holds.
    #[test]
    fn hold_value_child() {
        if std::env::var_os(HOLD_ENV).is_none() {
            return;
        }

        let value = Box::new(std::hint::black_box(HELD));
        let pointer = Box::new(&*value as *const u64 as usize);
        let heap_graph = Box::new([0u64; GRAPH_WORDS]);
        println!(
            "held {:x} {:x} {:x} {:x} {:x} {:x}",
            &*value as *const u64 as usize,
            &*pointer as *const usize as usize,
            HELD_PAIR.as_ptr() as usize,
            std::hint::black_box(&HELD_READ_ONLY) as *const u64 as usize,
            HELD_GRAPH.as_ptr() as usize,
            heap_graph.as_ptr() as usize,
        );
        io::stdout().flush().unwrap();

        for line in io::stdin().lines() {
            line.unwrap();
            // SAFETY: the box is live; the read is volatile since the parent
            // writes it behind our back.
            let now = unsafe { std::ptr::read_volatile(&*value) };
            println!("now {:x}", now);
            io::stdout().flush().unwrap();
        }

        std::hint::black_box((value, pointer, heap_graph, &HELD_PAIR, &HELD_GRAPH));
    }

    struct Holder {
        child: Scratch,
        stdout: io::Lines<BufReader<std::process::ChildStdout>>,
        value: u64,
        pointer: u64,
        pair: u64,
        read_only: u64,
        /// [`GRAPH_WORDS`] words in the image.
        image_graph: u64,
        /// [`GRAPH_WORDS`] words on the heap.
        heap_graph: u64,
    }

    impl Holder {
        fn spawn() -> Self {
            let mut child = Scratch(
                Command::new(std::env::current_exe().unwrap())
                    .args([
                        "--exact",
                        "platform::linux::tests::hold_value_child",
                        "--nocapture",
                        "--test-threads=1",
                    ])
                    .env(HOLD_ENV, "1")
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                    .expect("spawn holder"),
            );

            let mut stdout = BufReader::new(child.0.stdout.take().unwrap()).lines();

            while let Some(line) = stdout.next() {
                let line = line.unwrap();

                if let Some((_, rest)) = line.split_once("held ") {
                    let mut it = rest.split(' ').map(|a| u64::from_str_radix(a, 16).unwrap());
                    let value = it.next().unwrap();
                    let pointer = it.next().unwrap();
                    let pair = it.next().unwrap();
                    let read_only = it.next().unwrap();
                    let image_graph = it.next().unwrap();
                    let heap_graph = it.next().unwrap();
                    return Self {
                        child,
                        stdout,
                        value,
                        pointer,
                        pair,
                        read_only,
                        image_graph,
                        heap_graph,
                    };
                }
            }

            panic!("holder exited without printing its addresses");
        }

        fn handle(&self) -> ProcessHandle {
            ProcessHandle::open(self.child.0.id()).unwrap().unwrap()
        }

        /// The value the child itself now reads.
        fn held(&mut self) -> u64 {
            let stdin = self.child.0.stdin.as_mut().unwrap();
            stdin.write_all(b"\n").unwrap();
            stdin.flush().unwrap();

            for line in &mut self.stdout {
                if let Some(now) = line.unwrap().strip_prefix("now ") {
                    return u64::from_str_radix(now, 16).unwrap();
                }
            }

            panic!("holder exited without answering");
        }

        /// Wait until no thread of the child is running, so each reports the
        /// stack pointer it is blocked with. The child prints its addresses
        /// before it blocks reading stdin.
        fn wait_blocked(&self) {
            let task = format!("/proc/{}/task", self.child.0.id());

            for _ in 0..500 {
                let running = fs::read_dir(&task).unwrap().any(|e| {
                    let path = e.unwrap().path().join("syscall");
                    fs::read_to_string(path).is_ok_and(|s| s.trim() == "running")
                });

                if !running {
                    return;
                }

                std::thread::sleep(Duration::from_millis(10));
            }

            panic!("child threads did not block");
        }

        fn state(&self) -> char {
            let stat = fs::read_to_string(format!("/proc/{}/stat", self.child.0.id())).unwrap();
            let (_, rest) = stat.rsplit_once(") ").unwrap();
            rest.chars().next().unwrap()
        }
    }

    struct NoProgress;

    impl ScanProgress for NoProgress {
        fn report_bytes(&mut self, _: Size) -> anyhow::Result<()> {
            Ok(())
        }

        fn report(&mut self, _: usize, _: u64) -> anyhow::Result<()> {
            Ok(())
        }
    }

    fn scan(handle: &ProcessHandle, filter: &FilterExpr, ty: Type) -> Vec<Address> {
        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(2)
            .build()
            .unwrap();
        let mut addresses = BlockAddresses::new(1).unwrap();
        let mut values = Values::new(ty);
        handle
            .initial_scan(
                ScanContext::new(&pool, None, NoProgress),
                filter,
                &mut addresses,
                &mut values,
                Default::default(),
            )
            .unwrap();
        addresses.iter().collect()
    }

    #[test]
    fn read_child_memory() {
        let holder = Holder::spawn();
        let raw = &holder.handle().process.handle;

        let mut buf = [0u8; 8];
        assert_eq!(
            raw.read_memory(Address::new(holder.value), &mut buf)
                .unwrap(),
            8
        );
        assert_eq!(u64::from_ne_bytes(buf), HELD);

        let mut buf = [0u8; 8];
        assert_eq!(raw.read_proc_mem(holder.value, &mut buf).unwrap(), 8);
        assert_eq!(u64::from_ne_bytes(buf), HELD);

        let mut buf = [0u8; 8];
        assert_eq!(raw.read_memory(Address::new(0), &mut buf).unwrap(), 0);
        assert_eq!(raw.read_proc_mem(0, &mut buf).unwrap(), 0);

        // A read running off the end of a mapping stops at its end.
        let regions = raw.memory_regions().unwrap().unwrap();
        let region = regions
            .windows(2)
            .find(|w| w[0].is_writable() && w[1].state == MemoryState::Free)
            .map(|w| &w[0])
            .unwrap();
        let end = region.range.base.0 + region.range.size.into_inner();
        let mut buf = [0u8; 16];
        assert_eq!(raw.read_memory(Address::new(end - 8), &mut buf).unwrap(), 8);
        assert_eq!(raw.read_proc_mem(end - 8, &mut buf).unwrap(), 8);
    }

    #[test]
    fn snapshot_matches_query_walk() {
        let holder = Holder::spawn();
        let process = holder.handle().process;

        let mut walked = Vec::new();
        let mut address = 0;

        while let Some(region) = process.virtual_query(Address::new(address)).unwrap() {
            address = region.range.base.0 + region.range.size.into_inner();
            walked.push(region);
        }

        let snapshot = process.handle.memory_regions().unwrap().unwrap();
        assert_eq!(format!("{:?}", snapshot), format!("{:?}", walked));
    }

    #[test]
    fn value_scan_finds_child_value() {
        let holder = Holder::spawn();
        let handle = holder.handle();
        let filter = FilterExpr::parse(&format!("value == {}u64", HELD), &handle).unwrap();
        let found = scan(&handle, &filter, Type::U64);
        assert!(found.contains(&Address::new(holder.value)), "{:?}", found);
    }

    #[test]
    fn value_scan_reports_each_address_once() {
        let holder = Holder::spawn();
        let mut handle = holder.handle();
        handle.refresh_modules().unwrap();
        let filter = FilterExpr::parse(&format!("value == {}u64", HELD), &handle).unwrap();
        let found = scan(&handle, &filter, Type::U64);

        for address in [holder.pair, holder.pair + 8] {
            assert!(found.contains(&Address::new(address)), "{:?}", found);
        }

        let unique = found.iter().collect::<HashSet<_>>();
        assert_eq!(unique.len(), found.len(), "{:?}", found);
    }

    /// Only the writable parts of module images are scanned: a value in an
    /// image's writable data is found, one in its read-only data is not, and
    /// no scanned range touches code or read-only data.
    #[test]
    fn value_scan_skips_read_only_module_parts() {
        let holder = Holder::spawn();
        let mut handle = holder.handle();
        handle.refresh_modules().unwrap();

        let read_only = handle
            .process
            .virtual_query(Address::new(holder.read_only))
            .unwrap()
            .unwrap();
        assert!(matches!(read_only.ty, Some(MemoryType::Image)));
        assert!(!read_only.is_writable(), "{}", read_only);

        let filter = FilterExpr::parse(&format!("value == {}u64", HELD), &handle).unwrap();

        for modules_only in [false, true] {
            let pool = rayon::ThreadPoolBuilder::new()
                .num_threads(2)
                .build()
                .unwrap();
            let mut addresses = BlockAddresses::new(1).unwrap();
            let mut values = Values::new(Type::U64);
            handle
                .initial_scan(
                    ScanContext::new(&pool, None, NoProgress),
                    &filter,
                    &mut addresses,
                    &mut values,
                    crate::InitialScanConfig {
                        modules_only,
                        ..Default::default()
                    },
                )
                .unwrap();
            let found = addresses.iter().collect::<Vec<_>>();

            for address in [holder.pair, holder.pair + 8] {
                assert!(found.contains(&Address::new(address)), "{:?}", found);
            }

            assert!(
                !found.contains(&Address::new(holder.read_only)),
                "{:?}",
                found
            );
            assert_eq!(
                found.contains(&Address::new(holder.value)),
                !modules_only,
                "{:?}",
                found
            );
        }

        let regions = handle.process.handle.memory_regions().unwrap().unwrap();
        let skipped = regions
            .iter()
            .filter(|r| matches!(r.ty, Some(MemoryType::Image)) && !r.is_writable())
            .collect::<Vec<_>>();
        assert!(skipped.iter().any(|r| r.is_executable()));

        for modules_only in [false, true] {
            let ranges = handle
                .scan_ranges(modules_only, crate::process_handle::ModuleParts::Writable)
                .unwrap();

            for range in &ranges {
                let end = range.base.0 + range.size.into_inner();

                for r in &skipped {
                    let r_end = r.range.base.0 + r.range.size.into_inner();
                    assert!(
                        end <= r.range.base.0 || r_end <= range.base.0,
                        "{:?} overlaps {}",
                        range,
                        r
                    );
                }
            }
        }
    }

    /// A value straddling the end of a scan buffer is still found: the
    /// default and compare scanners read what runs past their chunk from the
    /// process.
    #[test]
    fn scan_finds_value_straddling_chunks() {
        let holder = Holder::spawn();
        let handle = holder.handle();
        let region = handle
            .process
            .virtual_query(Address::new(holder.value))
            .unwrap()
            .unwrap();

        // End the first chunk of the value's region halfway through the value.
        let buffer_size = usize::try_from(holder.value + 4 - region.range.base.0).unwrap();

        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(2)
            .build()
            .unwrap();

        // Ranges rather than `==`, which has a specialized scanner. Arithmetic
        // on `value` takes the default scanner, a bare `value` the compare
        // scanner.
        for (filter, specialized) in [
            (
                format!(
                    "value + 0 > {}u64 and value + 0 < {}u64",
                    HELD - 1,
                    HELD + 1
                ),
                false,
            ),
            (
                format!("value > {}u64 and value < {}u64", HELD - 1, HELD + 1),
                true,
            ),
        ] {
            let filter = FilterExpr::parse(&filter, &handle).unwrap();
            assert_eq!(
                filter.scanner(None, &handle, Type::U64).unwrap().is_some(),
                specialized,
                "{}",
                filter
            );
            let mut addresses = BlockAddresses::new(1).unwrap();
            let mut values = Values::new(Type::U64);
            handle
                .initial_scan(
                    ScanContext::new(&pool, None, NoProgress),
                    &filter,
                    &mut addresses,
                    &mut values,
                    crate::InitialScanConfig {
                        buffer_size: Some(buffer_size),
                        aligned: Some(false),
                        ..Default::default()
                    },
                )
                .unwrap();

            let found = addresses.iter().collect::<Vec<_>>();
            let held = Address::new(holder.value);
            assert_eq!(
                found.iter().filter(|a| **a == held).count(),
                1,
                "{}: {:?}",
                filter,
                found
            );
            let index = found.iter().position(|a| *a == held).unwrap();
            assert_eq!(values.get(index), Some(crate::Value::U64(HELD)));
        }
    }

    /// With many tasks and small buffers, workers finish chunks out of order;
    /// `initial_scan` still emits results in strictly increasing address
    /// order, the same results a single task scanning big buffers finds.
    #[test]
    fn initial_scan_results_in_address_order() {
        let holder = Holder::spawn();
        let mut handle = holder.handle();
        handle.refresh_modules().unwrap();
        handle.refresh_threads().unwrap();

        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(8)
            .build()
            .unwrap();
        let filter = FilterExpr::parse("value != 0", &handle).unwrap();

        let run = |tasks: usize, buffer_size: usize| {
            let mut addresses = BlockAddresses::new(1).unwrap();
            let mut values = Values::new(Type::U64);
            handle
                .initial_scan(
                    ScanContext::new(&pool, None, NoProgress),
                    &filter,
                    &mut addresses,
                    &mut values,
                    crate::InitialScanConfig {
                        tasks: Some(tasks),
                        buffer_size: Some(buffer_size),
                        ..Default::default()
                    },
                )
                .unwrap();
            (
                addresses.iter().collect::<Vec<_>>(),
                values.iter().map(|v| v.read()).collect::<Vec<_>>(),
            )
        };

        assert!(handle.modules.modules.len() > 1);

        let (expected, expected_values) = run(1, 0x100_000);
        assert!(expected.len() > 1000, "{}", expected.len());

        let (found, found_values) = run(16, 0x1000);
        assert!(found.windows(2).all(|w| w[0] < w[1]));
        assert_eq!(found, expected);
        assert_eq!(found_values, expected_values);
    }

    /// `rescan_large` keeps results inside module images, as `rescan_small`
    /// does, rather than depending on the number of results.
    #[test]
    fn rescan_large_keeps_module_results() {
        let holder = Holder::spawn();
        let mut handle = holder.handle();
        handle.refresh_modules().unwrap();
        handle.refresh_threads().unwrap();

        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(2)
            .build()
            .unwrap();
        let filter = FilterExpr::parse(&format!("value == {}u64", HELD), &handle).unwrap();
        let mut addresses = BlockAddresses::new(1).unwrap();
        let mut values = Values::new(Type::U64);
        handle
            .initial_scan(
                ScanContext::new(&pool, None, NoProgress),
                &filter,
                &mut addresses,
                &mut values,
                Default::default(),
            )
            .unwrap();

        let in_image = |a: u64| {
            handle
                .modules
                .modules
                .iter()
                .any(|m| m.range.contains(Address::new(a)))
        };
        assert!(in_image(holder.pair), "HELD_PAIR is not in a module image");

        let found = addresses.iter().collect::<Vec<_>>();
        assert!(found.contains(&Address::new(holder.pair)), "{:?}", found);
        assert!(found.contains(&Address::new(holder.value)), "{:?}", found);

        let mut results = ScanResults::unchanged(addresses, values);
        handle
            .rescan_large(
                ScanContext::new(&pool, None, NoProgress),
                &mut results,
                &filter,
            )
            .unwrap();

        let kept = results.addresses().iter().collect::<HashSet<_>>();
        assert_eq!(kept, found.into_iter().collect::<HashSet<_>>());
    }

    /// Windowed rescans, whatever the window size, keep exactly what
    /// `rescan_small` keeps, with the same values and in the original order,
    /// and drop results outside the scanned memory.
    #[test]
    fn rescan_windows_match_rescan_small() {
        let holder = Holder::spawn();
        let mut handle = holder.handle();
        handle.refresh_modules().unwrap();
        handle.refresh_threads().unwrap();

        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(4)
            .build()
            .unwrap();
        let filter = FilterExpr::parse("value != 0", &handle).unwrap();
        let mut addresses = BlockAddresses::new(1).unwrap();
        let mut values = Values::new(Type::U64);
        handle
            .initial_scan(
                ScanContext::new(&pool, None, NoProgress),
                &filter,
                &mut addresses,
                &mut values,
                Default::default(),
            )
            .unwrap();

        // Unmapped, so every rescan has to drop it. Put first to keep the
        // addresses sorted.
        let mut scanned = (addresses, values);
        let addresses = BlockAddresses::from_sorted(
            scanned.0.step(),
            std::iter::once(Address::new(8)).chain(scanned.0.iter()),
        )
        .unwrap();
        let mut values = Values::new(Type::U64);
        values.push(crate::Value::U64(1));
        values.append(&mut scanned.1);

        let original = addresses.iter().collect::<Vec<_>>();
        let results = ScanResults::unchanged(addresses, values);
        let filter = FilterExpr::parse("value < 4294967296", &handle).unwrap();

        let read = |r: &ScanResults| {
            (
                r.addresses().iter().collect::<Vec<_>>(),
                r.initial().iter().map(|v| v.read()).collect::<Vec<_>>(),
                r.last().iter().map(|v| v.read()).collect::<Vec<_>>(),
            )
        };

        let mut small = results.clone();
        handle
            .rescan_small(
                ScanContext::new(&pool, None, NoProgress),
                &mut small,
                &filter,
            )
            .unwrap();
        let expected = read(&small);

        assert!(!expected.0.is_empty() && expected.0.len() < original.len() - 1);
        assert!(!expected.0.contains(&Address::new(8)));
        let kept = expected.0.iter().collect::<HashSet<_>>();
        let in_order = original
            .iter()
            .filter(|a| kept.contains(a))
            .copied()
            .collect::<Vec<_>>();
        assert_eq!(expected.0, in_order);

        // 1001 puts window ends inside values, 0x100_000 is the default.
        for window in [1001, 0x10_000, 0x100_000] {
            let mut windowed = results.clone();
            handle
                .rescan_windows(
                    ScanContext::new(&pool, None, NoProgress),
                    &mut windowed,
                    &filter,
                    window,
                )
                .unwrap();
            assert!(read(&windowed) == expected, "window {}", window);
        }
    }

    #[test]
    fn pointer_scan_finds_child_pointer() {
        let holder = Holder::spawn();
        let handle = holder.handle();
        let filter = FilterExpr::pointer(ValueExpr::Value, &handle.process).unwrap();
        let found = scan(&handle, &filter, handle.process.pointer_width.into_type());
        assert!(found.contains(&Address::new(holder.pointer)));
    }

    struct NoPathProgress;

    impl crate::PointerScanInitialProgress for NoPathProgress {
        fn report(&mut self, _: usize, _: usize) -> anyhow::Result<()> {
            Ok(())
        }
    }

    impl crate::PointerScanBackreferenceProgress for NoPathProgress {
        fn report(&mut self, _: usize, _: usize) -> anyhow::Result<()> {
            Ok(())
        }
    }

    /// Each path found as its base address and offsets, before and after the
    /// backreference scan, and whether each stage was truncated.
    struct FoundPaths {
        scanned: Vec<(u64, Vec<Offset>)>,
        scanned_truncated: bool,
        all: Vec<(u64, Vec<Offset>)>,
        all_truncated: bool,
    }

    /// Writes each `(from, to)` pointer into the child and pointer-scans
    /// those from `needle`.
    ///
    /// The pointers are handed to the scan as written rather than found by an
    /// initial scan, so the test does not depend on where the child's
    /// allocations land.
    fn pointer_paths(
        handle: &ProcessHandle,
        pointers: &[(u64, u64)],
        needle: u64,
        max_depth: usize,
        max_offset: u64,
        max_paths: usize,
    ) -> FoundPaths {
        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(2)
            .build()
            .unwrap();
        let mut addresses = Addresses::new(handle.process.pointer_width);
        let mut values = Values::new(handle.process.pointer_width.into_type());

        for &(from, to) in pointers {
            assert!(handle
                .process
                .handle
                .write_memory(Address::new(from), &to.to_ne_bytes())
                .unwrap());
            addresses.push(Address::new(from));
            values.push(Value::Pointer(Address::new(to)));
        }

        let token = Token::new();
        let mut scan = PointerScan::new(&pool, handle, &token);
        scan.max_depth = max_depth;
        scan.max_offset = Size::new(max_offset);
        scan.max_paths = max_paths;
        scan.build_references(addresses.iter_copied(), values.iter())
            .unwrap();

        let paths = |pointers: &[Pointer]| {
            pointers
                .iter()
                .map(|p| (p.base.eval(handle).unwrap().unwrap().0, p.offsets.clone()))
                .collect::<Vec<_>>()
        };

        let mut pointers = Vec::new();
        scan.scan(Address::new(needle), &mut pointers, &mut NoPathProgress)
            .unwrap();
        let scanned = paths(&pointers);
        let scanned_truncated = scan.truncated();
        scan.backreference_scan(&mut pointers, &mut NoPathProgress)
            .unwrap();

        FoundPaths {
            scanned,
            scanned_truncated,
            all: paths(&pointers),
            all_truncated: scan.truncated(),
        }
    }

    #[test]
    fn pointer_scan_keeps_every_pointer_to_a_shared_target() {
        let holder = Holder::spawn();
        let mut handle = holder.handle();
        handle.refresh_modules().unwrap();

        // Both words of the pair, in the image, point at the heap value.
        let pointers = [(holder.pair, holder.value), (holder.pair + 8, holder.value)];
        let found = pointer_paths(&handle, &pointers, holder.value, 1, 0, usize::MAX);
        let scanned = found.scanned;
        assert!(!found.scanned_truncated);

        for base in [holder.pair, holder.pair + 8] {
            assert!(
                scanned.contains(&(base, vec![Offset::zero()])),
                "{:x}: {:x?}",
                base,
                scanned
            );
        }
    }

    #[test]
    fn backreference_scan_adds_alternative_paths() {
        let holder = Holder::spawn();
        let mut handle = holder.handle();
        handle.refresh_modules().unwrap();

        // The first word points at itself, so the needle (the second word)
        // is reached from it both directly and through the first word again.
        let pointers = [(holder.pair, holder.pair)];
        let eight = Offset::new(Sign::Plus, Size::new(8));
        let direct = (holder.pair, vec![eight]);
        let through = (holder.pair, vec![Offset::zero(), eight]);

        let found = pointer_paths(&handle, &pointers, holder.pair + 8, 2, 8, usize::MAX);
        let (scanned, all) = (found.scanned, found.all);
        assert!(scanned.contains(&direct), "{:x?}", scanned);
        assert!(!scanned.contains(&through), "{:x?}", scanned);
        assert!(all.contains(&direct), "{:x?}", all);
        assert!(all.contains(&through), "{:x?}", all);
        assert!(!found.all_truncated);
    }

    #[test]
    fn pointer_scan_stops_at_max_paths() {
        let holder = Holder::spawn();
        let mut handle = holder.handle();
        handle.refresh_modules().unwrap();

        let pointers = [(holder.pair, holder.value), (holder.pair + 8, holder.value)];

        let found = pointer_paths(&handle, &pointers, holder.value, 1, 0, 2);
        assert_eq!(found.scanned.len(), 2, "{:x?}", found.scanned);
        assert!(!found.scanned_truncated);

        let found = pointer_paths(&handle, &pointers, holder.value, 1, 0, 1);
        assert_eq!(found.scanned.len(), 1, "{:x?}", found.scanned);
        assert!(found.scanned_truncated);
        assert_eq!(found.all, found.scanned);
        assert!(found.all_truncated);

        let found = pointer_paths(&handle, &pointers, holder.value, 1, 0, 0);
        assert!(found.scanned.is_empty());
        assert!(found.all_truncated);
    }

    #[test]
    fn backreference_scan_stops_at_max_paths() {
        let holder = Holder::spawn();
        let mut handle = holder.handle();
        handle.refresh_modules().unwrap();

        let pointers = [(holder.pair, holder.pair)];
        let eight = Offset::new(Sign::Plus, Size::new(8));

        let found = pointer_paths(&handle, &pointers, holder.pair + 8, 2, 8, 1);
        assert_eq!(found.scanned, vec![(holder.pair, vec![eight])]);
        assert!(!found.scanned_truncated);
        assert_eq!(found.all, found.scanned);
        assert!(found.all_truncated);
    }

    /// A fixed graph of pointers between [`GRAPH_WORDS`] words in the image
    /// and as many on the heap, each pointing up to three words below a
    /// word, with many ways to reach each word.
    fn graph_pointers(holder: &Holder) -> Vec<(u64, u64)> {
        let word = |i: u64| {
            let (base, i) = if i < GRAPH_WORDS as u64 {
                (holder.image_graph, i)
            } else {
                (holder.heap_graph, i - GRAPH_WORDS as u64)
            };
            base + i * 8
        };

        let words = 2 * GRAPH_WORDS as u64;
        let mut state = 0x2545_f491_4f6c_dd1du64;
        let mut next = move || {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            state
        };

        (0..words)
            .map(|from| {
                let to = next() % words;
                let below = (next() % 4).min(to % GRAPH_WORDS as u64);
                (word(from), word(to) - below * 8)
            })
            .collect()
    }

    #[test]
    fn backreference_scan_matches_eager_alternatives() {
        let holder = Holder::spawn();
        let mut handle = holder.handle();
        handle.refresh_modules().unwrap();

        let pointers = graph_pointers(&holder);
        let needle = holder.heap_graph + 20 * 8;
        let found = pointer_paths(&handle, &pointers, needle, 5, 0x18, usize::MAX);
        assert!(!found.all_truncated);

        // Each path as the word its base is at and its offsets, in the order
        // found, hashed with FNV-1a.
        let mut hash = 0xcbf2_9ce4_8422_2325u64;
        let mut feed = |bytes: &[u8]| {
            for &b in bytes {
                hash = (hash ^ u64::from(b)).wrapping_mul(0x100_0000_01b3);
            }
        };

        for (base, offsets) in &found.all {
            assert!(*base >= holder.image_graph);
            feed(&((base - holder.image_graph) / 8).to_le_bytes());

            for o in offsets {
                feed(format!("{} ", o).as_bytes());
            }

            feed(b";");
        }

        // Captured from the scan as of e5ae649, which stored every
        // alternative path during the initial scan.
        assert_eq!(
            (found.scanned.len(), found.all.len(), hash),
            (32, 412, 16664696828504641264),
            "{:x?}",
            found.all
        );
    }

    /// Peak resident memory of this process since the last reset, in KiB.
    fn peak_rss_kib() -> u64 {
        let status = std::fs::read_to_string("/proc/self/status").unwrap();
        let line = status.lines().find(|l| l.starts_with("VmHWM:")).unwrap();
        line.split_whitespace().nth(1).unwrap().parse().unwrap()
    }

    /// Measures a pointer scan at the default settings on a python3 child,
    /// printing what it kept and the growth in peak memory.
    ///
    /// Run alone, in release: `cargo test --release -p ptscan-core --lib
    /// pointer_scan_python_peak -- --ignored --nocapture`.
    #[test]
    #[ignore = "measurement; needs python3"]
    fn pointer_scan_python_peak() {
        let mut child = Command::new("python3")
            .args([
                "-c",
                "import sys, asyncio, json, email, http.client, unittest, decimal; \
                 x = [object() for _ in range(1000)]; \
                 print(hex(id(x[500])), flush=True); sys.stdin.read()",
            ])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("spawn python3");
        let mut stdout = BufReader::new(child.stdout.take().unwrap()).lines();
        let child = Scratch(child);
        let line = stdout.next().unwrap().unwrap();
        let needle = u64::from_str_radix(line.trim_start_matches("0x"), 16).unwrap();

        let mut handle = ProcessHandle::open(child.0.id()).unwrap().unwrap();
        handle.refresh_modules().unwrap();
        let pool = rayon::ThreadPoolBuilder::new().build().unwrap();
        let filter = FilterExpr::pointer(ValueExpr::Value, &handle.process).unwrap();
        let mut addresses = BlockAddresses::new(1).unwrap();
        let mut values = Values::new(handle.process.pointer_width.into_type());
        handle
            .initial_scan(
                ScanContext::new(&pool, None, NoProgress),
                &filter,
                &mut addresses,
                &mut values,
                Default::default(),
            )
            .unwrap();

        // At the server's default cap on stored paths, then without one.
        for max_paths in [1_000_000, usize::MAX] {
            let token = Token::new();
            let mut scan = PointerScan::new(&pool, &handle, &token);
            scan.max_paths = max_paths;
            scan.build_references(addresses.iter(), values.iter())
                .unwrap();

            std::fs::write("/proc/self/clear_refs", "5").unwrap();
            let before = peak_rss_kib();
            let start = std::time::Instant::now();
            let mut pointers = Vec::new();
            scan.scan(Address::new(needle), &mut pointers, &mut NoPathProgress)
                .unwrap();
            let scanned = pointers.len();
            let alternatives = scan.visited_count;
            scan.backreference_scan(&mut pointers, &mut NoPathProgress)
                .unwrap();
            let peak = peak_rss_kib().saturating_sub(before);

            println!(
                "max_paths {} references {} paths {} all {} alternatives {} \
                 truncated {} peak +{} MiB in {:?}",
                max_paths,
                scan.forward.len(),
                scanned,
                pointers.len(),
                alternatives,
                scan.truncated(),
                peak / 1024,
                start.elapsed()
            );
        }
    }

    #[test]
    fn write_child_memory() {
        let mut holder = Holder::spawn();
        let raw = holder.handle().process.handle;
        let value = Address::new(holder.value);

        let written = 0x0123_4567_89ab_cdefu64;
        assert!(raw.write_memory(value, &written.to_ne_bytes()).unwrap());
        let mut buf = [0u8; 8];
        assert_eq!(raw.read_memory(value, &mut buf).unwrap(), 8);
        assert_eq!(u64::from_ne_bytes(buf), written);
        assert_eq!(holder.held(), written);

        assert!(raw
            .write_proc_mem(holder.value, &HELD.to_ne_bytes())
            .unwrap());
        assert_eq!(holder.held(), HELD);

        assert!(!raw.write_memory(Address::new(0), &[0; 8]).unwrap());
        assert!(!raw.write_proc_mem(0, &[0; 8]).unwrap());

        // A write running off the end of a mapping is incomplete.
        let regions = raw.memory_regions().unwrap().unwrap();
        let region = regions
            .windows(2)
            .find(|w| w[0].is_writable() && w[1].state == MemoryState::Free)
            .map(|w| &w[0])
            .unwrap();
        let end = region.range.base.0 + region.range.size.into_inner();
        let mut buf = [0u8; 16];
        assert_eq!(raw.read_memory(Address::new(end - 8), &mut buf).unwrap(), 8);
        assert!(!raw.write_memory(Address::new(end - 8), &buf).unwrap());

        // Read-only private mappings are written through, as on Windows;
        // writing back the same bytes leaves the child unchanged.
        let maps = raw.maps().unwrap();
        let map = maps
            .iter()
            .find(|m| m.read && !m.write && !m.shared && m.path.is_some())
            .unwrap();
        let mut buf = [0u8; 8];
        assert_eq!(
            raw.read_memory(Address::new(map.start), &mut buf).unwrap(),
            8
        );
        assert!(raw.write_memory(Address::new(map.start), &buf).unwrap());
    }

    #[test]
    fn child_threads_and_stacks() {
        let holder = Holder::spawn();
        holder.wait_blocked();
        let pid = holder.child.0.id();
        let handle = holder.handle();

        let threads = threads(pid).unwrap();
        assert!(threads.iter().all(|t| t.process_id() == pid));
        assert!(threads.iter().any(|t| t.thread_id() == pid));

        let regions = handle.process.handle.memory_regions().unwrap().unwrap();
        // The child is blocked reading stdin, so every thread has a stack.
        let modules = handle.get_modules().unwrap();
        let stacks = handle.get_threads_with_modules(&modules).unwrap();
        assert_eq!(stacks.len(), threads.len());

        for thread in &stacks {
            assert!(regions
                .iter()
                .any(|r| r.range == thread.stack && r.is_writable()));
        }

        let main = Thread::open(pid)
            .unwrap()
            .thread_stack(&handle.process)
            .unwrap();
        assert!(stacks.iter().any(|t| t.stack == main));
        assert!(Thread::open(u32::MAX).is_err());
    }

    #[test]
    fn suspend_and_resume_child() {
        let mut holder = Holder::spawn();
        let process = holder.handle().process;

        let wait_for = |holder: &Holder, stopped: bool| {
            for _ in 0..100 {
                if (holder.state() == 'T') == stopped {
                    return;
                }

                std::thread::sleep(Duration::from_millis(10));
            }

            panic!("child did not reach stopped = {}", stopped);
        };

        process.suspend().unwrap();
        wait_for(&holder, true);
        process.resume().unwrap();
        wait_for(&holder, false);
        assert_eq!(holder.held(), HELD);
    }
}
