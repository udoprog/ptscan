//! Linux support through `/proc`. Memory access and threads are not yet
//! implemented.

use std::{
    convert::TryInto,
    ffi::{OsStr, OsString},
    fs, io,
    os::unix::ffi::OsStrExt,
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

    pub(crate) fn read_memory(&self, _: Address, _: &mut [u8]) -> Result<usize, Error> {
        Err(Error::Unsupported)
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

            return Ok(Some(MemoryInformation {
                range: range(start, map.start),
                state: MemoryState::Free,
                ty: None,
                protect: fixed_map::Set::new(),
            }));
        }

        let ty = match &map.path {
            Some(path)
                if maps
                    .iter()
                    .any(|m| m.path.as_ref() == Some(path) && m.execute) =>
            {
                MemoryType::Image
            }
            _ if map.path.is_some() || map.shared => MemoryType::Mapped,
            _ => MemoryType::Private,
        };

        let mut protect = fixed_map::Set::new();
        protect.insert(map.protect());

        Ok(Some(MemoryInformation {
            range: range(map.start, map.end),
            state: MemoryState::Commit,
            ty: Some(ty),
            protect,
        }))
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
    use std::process::{Child, Command};

    use super::*;
    use crate::{process::ProcessInfo, Process};

    /// Kills the scratch child when dropped, so a failing test cleans up.
    struct Scratch(Child);

    impl Drop for Scratch {
        fn drop(&mut self) {
            let _ = self.0.kill();
            let _ = self.0.wait();
        }
    }

    fn scratch() -> Scratch {
        Scratch(
            Command::new("sleep")
                .arg("30")
                .spawn()
                .expect("spawn sleep"),
        )
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
}
