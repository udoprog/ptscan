//! Linux support through `/proc` and `process_vm_readv`. Writing memory and
//! threads are not yet implemented.

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

    pub(crate) fn write_memory(&self, _: Address, _: &[u8]) -> Result<bool, Error> {
        Err(Error::Unsupported)
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
        Err(Error::Unsupported)
    }

    pub(crate) fn resume(&self) -> Result<(), Error> {
        Err(Error::Unsupported)
    }
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

pub(crate) fn threads() -> Result<std::iter::Empty<system::Thread>, Error> {
    Err(Error::Unsupported)
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

    pub fn thread_stack(&self, _: &Process) -> anyhow::Result<AddressRange> {
        Err(Error::Unsupported.into())
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

    pub fn build(self, _: ThreadId) -> Result<Thread, Error> {
        Err(Error::Unsupported)
    }
}

#[cfg(test)]
mod tests {
    use std::{
        io::{BufRead, BufReader, Write},
        process::{Child, Command, Stdio},
        time::Duration,
    };

    use super::*;
    use crate::{
        process::ProcessInfo, Addresses, FilterExpr, Process, ProcessHandle, ScanProgress, Type,
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

    /// Run in a re-executed test binary by [`Holder::spawn`]: keeps `HELD` and
    /// a pointer to it on the heap and prints both addresses.
    #[test]
    fn hold_value_child() {
        if std::env::var_os(HOLD_ENV).is_none() {
            return;
        }

        let value = Box::new(std::hint::black_box(HELD));
        let pointer = Box::new(&*value as *const u64 as usize);
        println!(
            "held {:x} {:x}",
            &*value as *const u64 as usize, &*pointer as *const usize as usize
        );
        io::stdout().flush().unwrap();
        std::thread::sleep(Duration::from_secs(60));
        std::hint::black_box((value, pointer));
    }

    struct Holder {
        child: Scratch,
        value: u64,
        pointer: u64,
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
                    .stdout(Stdio::piped())
                    .spawn()
                    .expect("spawn holder"),
            );

            let stdout = BufReader::new(child.0.stdout.take().unwrap());

            for line in stdout.lines() {
                let line = line.unwrap();

                if let Some((_, rest)) = line.split_once("held ") {
                    let mut it = rest.split(' ').map(|a| u64::from_str_radix(a, 16).unwrap());
                    let value = it.next().unwrap();
                    let pointer = it.next().unwrap();
                    return Self {
                        child,
                        value,
                        pointer,
                    };
                }
            }

            panic!("holder exited without printing its addresses");
        }

        fn handle(&self) -> ProcessHandle {
            ProcessHandle::open(self.child.0.id()).unwrap().unwrap()
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
        let mut addresses = Addresses::new(handle.process.pointer_width);
        let mut values = Values::new(ty);
        handle
            .initial_scan(
                &pool,
                filter,
                &mut addresses,
                &mut values,
                None,
                NoProgress,
                Default::default(),
            )
            .unwrap();
        addresses.iter_copied().collect()
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
    fn pointer_scan_finds_child_pointer() {
        let holder = Holder::spawn();
        let handle = holder.handle();
        let filter = FilterExpr::pointer(ValueExpr::Value, &handle.process).unwrap();
        let found = scan(&handle, &filter, handle.process.pointer_width.into_type());
        assert!(found.contains(&Address::new(holder.pointer)));
    }
}
