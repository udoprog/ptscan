//! Predicates used for matching against memory.

use crate::{
    address::{Address, Size},
    filter,
    process::{MemoryInformation, Process},
    scan, thread_buffers, Location, ProcessHandle, Token,
};
use byteorder::{ByteOrder, LittleEndian};
use std::{
    convert::TryFrom,
    error, fmt, mem, str,
    sync::{mpsc, Arc},
};

/// A single dynamic literal value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Value {
    U128(u128),
    I128(i128),
    U64(u64),
    I64(i64),
    U32(u32),
    I32(i32),
    U16(u16),
    I16(i16),
    U8(u8),
    I8(i8),
}

impl Value {
    /// Try to treat the value as an address.
    ///
    /// Returns `None` if this is not possible (e.g. value out of range).
    pub fn as_address(&self) -> Result<Address, failure::Error> {
        use self::Value::*;
        use std::convert::TryFrom;

        let out = match *self {
            U128(value) => Address::try_from(value)?,
            I128(value) => Address::try_from(value)?,
            U64(value) => Address::try_from(value)?,
            I64(value) => Address::try_from(value)?,
            U32(value) => Address::try_from(value)?,
            I32(value) => Address::try_from(value)?,
            U16(value) => Address::try_from(value)?,
            I16(value) => Address::try_from(value)?,
            U8(value) => Address::try_from(value)?,
            I8(value) => Address::try_from(value)?,
        };

        Ok(out)
    }

    /// Decode the given buffer into a value.
    pub fn encode(&self, buf: &mut [u8]) {
        use self::Value::*;

        match *self {
            U128(value) => u128::encode(buf, value),
            I128(value) => i128::encode(buf, value),
            U64(value) => u64::encode(buf, value),
            I64(value) => i64::encode(buf, value),
            U32(value) => u32::encode(buf, value),
            I32(value) => i32::encode(buf, value),
            U16(value) => u16::encode(buf, value),
            I16(value) => i16::encode(buf, value),
            U8(value) => u8::encode(buf, value),
            I8(value) => i8::encode(buf, value),
        }
    }

    /// Get the type of the value.
    pub fn ty(&self) -> Type {
        use self::Value::*;

        match *self {
            U128(..) => Type::U128,
            I128(..) => Type::I128,
            U64(..) => Type::U64,
            I64(..) => Type::I64,
            U32(..) => Type::U32,
            I32(..) => Type::I32,
            U16(..) => Type::U16,
            I16(..) => Type::I16,
            U8(..) => Type::U8,
            I8(..) => Type::I8,
        }
    }
}

impl str::FromStr for Value {
    type Err = failure::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (hex, s) = if s.starts_with("0x") {
            (true, &s[2..])
        } else {
            (false, s)
        };

        let mut it = s.split(|c| c == 'i' || c == 'u');

        let base = it
            .next()
            .ok_or_else(|| failure::format_err!("missing numeric base"))?;

        let (s, ty) = match it.next() {
            Some(suffix) => {
                // NB: need to extract full suffix from original input.
                let e = s.len() - suffix.len() - 1;
                (base, str::parse::<Type>(&s[e..])?)
            }
            None => (s, Type::I32),
        };

        if hex {
            ty.parse_hex(s)
        } else {
            ty.parse(s)
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Value::U128(value) => write!(fmt, "{}u128", value),
            Value::I128(value) => write!(fmt, "{}i128", value),
            Value::U64(value) => write!(fmt, "{}u64", value),
            Value::I64(value) => write!(fmt, "{}i64", value),
            Value::U32(value) => write!(fmt, "{}u32", value),
            Value::I32(value) => write!(fmt, "{}i32", value),
            Value::U16(value) => write!(fmt, "{}u16", value),
            Value::I16(value) => write!(fmt, "{}i16", value),
            Value::U8(value) => write!(fmt, "{}u8", value),
            Value::I8(value) => write!(fmt, "{}i8", value),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    U128,
    I128,
    U64,
    I64,
    U32,
    I32,
    U16,
    I16,
    U8,
    I8,
}

impl Type {
    /// Parse a string value of the type.
    pub fn parse(&self, input: &str) -> Result<Value, failure::Error> {
        use self::Type::*;

        let value = match *self {
            U128 => Value::U128(str::parse::<u128>(input)?),
            I128 => Value::I128(str::parse::<i128>(input)?),
            U64 => Value::U64(str::parse::<u64>(input)?),
            I64 => Value::I64(str::parse::<i64>(input)?),
            U32 => Value::U32(str::parse::<u32>(input)?),
            I32 => Value::I32(str::parse::<i32>(input)?),
            U16 => Value::U16(str::parse::<u16>(input)?),
            I16 => Value::I16(str::parse::<i16>(input)?),
            U8 => Value::U8(str::parse::<u8>(input)?),
            I8 => Value::I8(str::parse::<i8>(input)?),
        };

        Ok(value)
    }

    /// Parse a string as hex.
    pub fn parse_hex(&self, input: &str) -> Result<Value, failure::Error> {
        use self::Type::*;

        let value = match *self {
            U128 => Value::U128(u128::from_str_radix(input, 16)?),
            I128 => Value::I128(i128::from_str_radix(input, 16)?),
            U64 => Value::U64(u64::from_str_radix(input, 16)?),
            I64 => Value::I64(i64::from_str_radix(input, 16)?),
            U32 => Value::U32(u32::from_str_radix(input, 16)?),
            I32 => Value::I32(i32::from_str_radix(input, 16)?),
            U16 => Value::U16(u16::from_str_radix(input, 16)?),
            I16 => Value::I16(i16::from_str_radix(input, 16)?),
            U8 => Value::U8(u8::from_str_radix(input, 16)?),
            I8 => Value::I8(i8::from_str_radix(input, 16)?),
        };

        Ok(value)
    }

    /// The size in-memory that a value has.
    pub fn size(&self) -> usize {
        use self::Type::*;

        match *self {
            U128 => mem::size_of::<u128>(),
            I128 => mem::size_of::<i128>(),
            U64 => mem::size_of::<u64>(),
            I64 => mem::size_of::<i64>(),
            U32 => mem::size_of::<u32>(),
            I32 => mem::size_of::<i32>(),
            U16 => mem::size_of::<u16>(),
            I16 => mem::size_of::<i16>(),
            U8 => mem::size_of::<u8>(),
            I8 => mem::size_of::<i8>(),
        }
    }

    /// Decode the given buffer into a value.
    pub fn decode(&self, buf: &[u8]) -> Value {
        use self::Type::*;
        use byteorder::{ByteOrder, LittleEndian};

        match *self {
            U128 => Value::U128(LittleEndian::read_u128(buf)),
            I128 => Value::I128(LittleEndian::read_i128(buf)),
            U64 => Value::U64(LittleEndian::read_u64(buf)),
            I64 => Value::I64(LittleEndian::read_i64(buf)),
            U32 => Value::U32(LittleEndian::read_u32(buf)),
            I32 => Value::I32(LittleEndian::read_i32(buf)),
            U16 => Value::U16(LittleEndian::read_u16(buf)),
            I16 => Value::I16(LittleEndian::read_i16(buf)),
            U8 => Value::U8(buf[0]),
            I8 => Value::I8(buf[0] as i8),
        }
    }
}

#[derive(Debug)]
pub struct ParseTypeError(String);

impl error::Error for ParseTypeError {}

impl fmt::Display for ParseTypeError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "`{}` is not a valid", self.0)
    }
}

impl str::FromStr for Type {
    type Err = ParseTypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ty = match s {
            "u128" => Type::U128,
            "i128" => Type::I128,
            "u64" => Type::U64,
            "i64" => Type::I64,
            "u32" => Type::U32,
            "i32" => Type::I32,
            "u16" => Type::U16,
            "i16" => Type::I16,
            "u8" => Type::U8,
            "i8" => Type::I8,
            other => return Err(ParseTypeError(other.to_string())),
        };

        Ok(ty)
    }
}

/// A bunch of read-only zeros used for comparison.
static ZEROS: &'static [u8] = &[
    0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
];

/// Special matching mode to speed up scanning.
///
/// Special matchers are matchers which can look at an entire slice of memory and immediate (without decoding) decide
/// if it is a match or not.
///
/// This has the benefit of speeding up decoding of scanned memory significantly.
#[derive(Debug, Clone)]
pub enum Special {
    /// Must match the given buffer exactly.
    Exact { buffer: Vec<u8> },
    /// Must not match the given buffer exactly.
    NotExact { buffer: Vec<u8> },
    /// All bytes in the given range are expected to be zero.
    Zero,
    /// All bytes in the given range are expected to be non-zero.
    NotZero,
}

impl Special {
    /// Invert what the special matcher does.
    ///
    /// Non-zero matchers become zero matchers,
    /// Exact matchers become not-exact matchers.
    pub fn invert(self) -> Special {
        match self {
            Special::Zero => Special::NotZero,
            Special::NotZero => Special::Zero,
            Special::Exact { buffer } => Special::NotExact { buffer },
            Special::NotExact { buffer } => Special::Exact { buffer },
        }
    }

    /// Set up an exact match for the given value.
    pub fn exact<T>(value: T) -> Special
    where
        T: Encode,
    {
        let mut buffer = vec![0u8; mem::size_of::<T>()];
        T::encode(&mut buffer, value);
        Special::Exact { buffer }
    }

    /// Set up a not-exact match for the given value.
    pub fn not_exact<T>(value: T) -> Special
    where
        T: Encode,
    {
        let mut buffer = vec![0u8; mem::size_of::<T>()];
        T::encode(&mut buffer, value);
        Special::NotExact { buffer }
    }

    /// Test if this special scan matches the given slice of memory.
    pub fn test(&self, buf: &[u8]) -> Option<bool> {
        match self {
            Special::Zero => Some(buf == &ZEROS[..buf.len()]),
            Special::NotZero => {
                if buf == &ZEROS[..buf.len()] {
                    return Some(false);
                }

                None
            }
            Special::Exact { ref buffer } => Some(buf == buffer.as_slice()),
            Special::NotExact { ref buffer } => Some(buf != buffer.as_slice()),
        }
    }
}

pub trait Encode: Sized {
    /// Encode the value into the buffer.
    fn encode(buffer: &mut [u8], value: Self);
}

impl Encode for u128 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u128(buffer, value);
    }
}

impl Encode for i128 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i128(buffer, value);
    }
}

impl Encode for u64 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u64(buffer, value);
    }
}

impl Encode for i64 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i64(buffer, value);
    }
}

impl Encode for u32 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u32(buffer, value);
    }
}

impl Encode for i32 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i32(buffer, value);
    }
}

impl Encode for u16 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_u16(buffer, value);
    }
}

impl Encode for i16 {
    fn encode(buffer: &mut [u8], value: Self) {
        LittleEndian::write_i16(buffer, value);
    }
}

impl Encode for u8 {
    fn encode(buffer: &mut [u8], value: Self) {
        buffer[0] = value;
    }
}

impl Encode for i8 {
    fn encode(buffer: &mut [u8], value: Self) {
        buffer[0] = value as u8;
    }
}

/// A scan responsible for finding results in memory.
pub struct Scan {
    /// Thread pool this scan uses.
    thread_pool: Arc<rayon::ThreadPool>,
    /// Only scan for aligned values.
    aligned: bool,
    /// If this scan has a set of initial results.
    pub initial: bool,
    /// Current results in scan.
    pub results: Vec<ScanResult>,
}

impl Scan {
    /// Construct a new scan associated with a thread pool.
    pub fn new(thread_pool: &Arc<rayon::ThreadPool>) -> Self {
        Self {
            results: Vec::new(),
            aligned: false,
            initial: false,
            thread_pool: Arc::clone(thread_pool),
        }
    }

    /// Only scan for values which are aligned.
    pub fn aligned(self) -> Self {
        Self {
            aligned: true,
            ..self
        }
    }

    /// Refresh current value for scan results.
    pub fn refresh(
        &mut self,
        process: &Process,
        limit: usize,
        cancel: Option<&Token>,
        progress: (impl Progress + Send),
    ) -> Result<(), failure::Error> {
        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        let len = usize::min(self.results.len(), limit);
        let results = &mut self.results[..len];

        if results.is_empty() {
            return Ok(());
        }

        let buffers = thread_buffers::ThreadBuffers::new();

        let mut last_error = None;

        self.thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::sync_channel(1024);

                let mut reporter = Reporter::new(progress, results.len());

                for result in results {
                    let tx = tx.clone();
                    let buffers = &buffers;

                    s.spawn(move |_| {
                        if cancel.test() {
                            tx.send(Ok(())).expect("closed channel");
                            return;
                        }

                        let mut work = || {
                            let ty = result.value.ty();
                            let mut buf = buffers.get_mut(ty.size())?;
                            let value =
                                process.read_memory_of_type(result.address, ty, &mut *buf)?;
                            result.current = Some(value);
                            Ok::<_, failure::Error>(())
                        };

                        tx.send(work()).expect("closed channel");
                    });
                }

                while !reporter.is_done() {
                    let m = rx.recv().expect("closed channel");

                    if cancel.test() {
                        reporter.suppress();
                        last_error = Some(failure::format_err!("scan cancelled"));
                        cancel.set();
                    }

                    if let Err(e) = reporter.tick() {
                        last_error = Some(e);
                        cancel.set();
                    }

                    if let Err(e) = m {
                        last_error = Some(e);
                        cancel.set();
                    }
                }
            });
        });

        if let Some(e) = last_error {
            return Err(e);
        }

        Ok(())
    }

    /// rescan a set of results.
    pub fn rescan(
        &mut self,
        process: &Process,
        filter: &(dyn filter::Filter),
        cancel: Option<&Token>,
        progress: (impl Progress + Send),
    ) -> Result<(), failure::Error> {
        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        if self.results.is_empty() {
            return Ok(());
        }

        let buffers = thread_buffers::ThreadBuffers::new();
        let results = &mut self.results;

        let mut last_error = None;

        self.thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::sync_channel(1024);

                let mut reporter = Reporter::new(progress, results.len());

                for result in results {
                    let tx = tx.clone();
                    let buffers = &buffers;

                    s.spawn(move |_| {
                        if cancel.test() {
                            tx.send(Ok(())).expect("closed channel");
                            return;
                        }

                        let mut work = || {
                            let ty = result.value.ty();
                            let mut buf = buffers.get_mut(ty.size())?;
                            let value =
                                process.read_memory_of_type(result.address, ty, &mut *buf)?;

                            result.killed = !filter.test(Some(result), &value);
                            result.current = None;
                            result.value = value;

                            Ok::<_, failure::Error>(())
                        };

                        tx.send(work()).expect("closed channel");
                    });
                }

                while !reporter.is_done() {
                    let m = rx.recv().expect("closed channel");

                    if cancel.test() {
                        reporter.suppress();
                        last_error = Some(failure::format_err!("scan cancelled"));
                        cancel.set();
                    }

                    if let Err(e) = reporter.tick() {
                        last_error = Some(e);
                        cancel.set();
                    }

                    if let Err(e) = m {
                        last_error = Some(e);
                        cancel.set();
                    }
                }
            });
        });

        self.results.retain(|v| !v.killed);

        if let Some(e) = last_error {
            return Err(e);
        }

        Ok(())
    }

    /// Scan for the given value in memory.
    ///
    /// TODO: handle unaligned regions.
    ///
    /// Errors raised in worker threads will be collected, and the last error propagated to the user.
    /// Any error causes the processing to cancel.
    ///
    /// # Errors raised in Progress
    ///
    /// We can't just ignore errors which are raised by `scan::Progress`, since these might be important to the
    /// processing at hand.
    ///
    /// Therefore, any error raised by Progress will be treated as any error raised from a worker thread. Only the last
    /// error will be propagated to the user.
    pub fn initial_scan(
        &mut self,
        process: &Process,
        filter: &(dyn filter::Filter),
        cancel: Option<&Token>,
        progress: (impl Progress + Send),
    ) -> Result<(), failure::Error> {
        use crate::utils::IteratorExtension;

        let mut local_cancel = None;

        let cancel = match cancel {
            Some(cancel) => cancel,
            None => local_cancel.get_or_insert(Token::new()),
        };

        let scan_type = match filter.ty() {
            Some(scan_type) => scan_type,
            None => {
                return Err(failure::format_err!(
                    "can't perform initial scan with type-less filter"
                ));
            }
        };

        let size: usize = scan_type.size();
        let buffer_size = 0x1000;

        let buffers = thread_buffers::ThreadBuffers::new();

        let Scan {
            ref mut results,
            aligned,
            ref thread_pool,
            ..
        } = *self;

        // if true, threads should stop working
        let special = filter.special();
        let mut last_error = None;

        thread_pool.install(|| {
            rayon::scope(|s| {
                let (tx, rx) = mpsc::sync_channel(1024);
                let mut total = 0;
                let mut bytes = 0usize;

                for region in process.virtual_memory_regions().only_relevant() {
                    bytes += region
                        .as_ref()
                        .map_err(|_| ())
                        .and_then(|r| r.range.size.into_usize().map_err(|_| ()))
                        .unwrap_or(0);

                    total += 1;
                    let mut tx = tx.clone();

                    let task = Task {
                        buffers: &buffers,
                        process,
                        region,
                        buffer_size,
                        size,
                        aligned,
                        filter,
                        special: special.as_ref(),
                        scan_type,
                        cancel: &cancel,
                    };

                    s.spawn(move |_| {
                        let result = task.work(&mut tx);

                        tx.send(TaskProgress::Result(result))
                            .expect("channel send failed");
                    });
                }

                let mut reporter = Reporter::new(progress, total);

                if let Err(e) = reporter.report_bytes(bytes) {
                    last_error = Some(e);
                    cancel.set();
                }

                while !reporter.is_done() {
                    let m = rx.recv().expect("closed channel");

                    if cancel.test() {
                        reporter.suppress();
                        last_error = Some(failure::format_err!("scan cancelled"));
                        cancel.set();
                    }

                    match m {
                        // Scan result from a task.
                        TaskProgress::ScanResult(scan_result) => {
                            results.push(scan_result);
                        }
                        // Result from one task.
                        TaskProgress::Result(result) => {
                            if let Err(e) = result {
                                last_error = Some(e);
                                cancel.set();
                            }

                            if let Err(e) = reporter.tick() {
                                last_error = Some(e);
                                cancel.set();
                            }
                        }
                    }
                }
            })
        });

        if let Some(e) = last_error {
            return Err(e);
        }

        return Ok(());

        enum TaskProgress {
            Result(Result<(), failure::Error>),
            ScanResult(ScanResult),
        }

        struct Task<'a> {
            buffers: &'a thread_buffers::ThreadBuffers,
            process: &'a Process,
            region: Result<MemoryInformation, failure::Error>,
            buffer_size: usize,
            size: usize,
            aligned: bool,
            filter: &'a (dyn filter::Filter),
            special: Option<&'a scan::Special>,
            scan_type: scan::Type,
            cancel: &'a Token,
        }

        impl<'a> Task<'a> {
            fn emit(
                tx: &mut mpsc::SyncSender<TaskProgress>,
                loc: &Address,
                offset: usize,
                value: scan::Value,
            ) -> Result<(), failure::Error> {
                let address = loc.add(Size::try_from(offset)?)?;

                let scan_result = ScanResult {
                    address,
                    value,
                    current: None,
                    killed: false,
                };

                tx.send(TaskProgress::ScanResult(scan_result))?;
                Ok(())
            }

            fn work(self, tx: &mut mpsc::SyncSender<TaskProgress>) -> Result<(), failure::Error> {
                let Task {
                    buffers,
                    process,
                    region,
                    buffer_size,
                    size,
                    aligned,
                    filter,
                    special,
                    scan_type,
                    cancel,
                    ..
                } = self;

                let region = region?;
                let range = region.range;
                let len = range.size.into_usize()?;

                let mut start = 0;

                while start < len && !cancel.test() {
                    let len = usize::min(buffer_size, len - start);
                    let loc = range.base.add(Size::try_from(start)?)?;

                    // length of the buffer we need to read process memory.
                    let mut buf = buffers.get_mut(len)?;

                    // TODO: figure out why we are trying to read invalid memory region sometimes.
                    let buf = match process.read_process_memory(loc, &mut *buf) {
                        Ok(buf) => buf,
                        Err(_) => continue,
                    };

                    let mut n = 0;
                    let end = buf.len() - size;

                    while n < end && !cancel.test() {
                        let w = &buf[n..(n + size)];

                        // A special, more efficient kind of matching is available.
                        if let Some(special) = special {
                            if let Some(result) = special.test(w) {
                                if result {
                                    let value = scan_type.decode(w);
                                    Self::emit(tx, &loc, n, value)?;
                                }

                                if aligned {
                                    n += size;
                                } else {
                                    n += 1;
                                }

                                continue;
                            }
                        }

                        let value = scan_type.decode(w);

                        if filter.test(None, &value) {
                            Self::emit(tx, &loc, n, value)?;
                        }

                        if aligned {
                            n += size;
                        } else {
                            n += 1;
                        }
                    }

                    start += buffer_size;
                }

                Ok(())
            }
        }
    }
}

pub struct Reporter<P> {
    progress: P,
    /// Current progress.
    current: usize,
    /// how many ticks constitute a percentage.
    percentage: usize,
    /// Total.
    total: usize,
    /// Whether to report progress or not.
    suppressed: bool,
}

impl<P> Reporter<P> {
    pub fn new(progress: P, total: usize) -> Reporter<P> {
        let mut percentage = total / 100;

        if percentage <= 0 {
            percentage = 1;
        }

        Reporter {
            progress,
            current: 0,
            percentage,
            total,
            suppressed: false,
        }
    }

    /// Supress any more reporting.
    pub fn suppress(&mut self) {
        self.suppressed = true;
    }

    // Report a number of bytes.
    pub fn report_bytes(&mut self, bytes: usize) -> Result<(), failure::Error>
    where
        P: Progress,
    {
        self.progress.report_bytes(bytes)
    }

    // Tick a single task.
    pub fn tick(&mut self) -> Result<(), failure::Error>
    where
        P: Progress,
    {
        self.current += 1;

        if self.suppressed {
            return Ok(());
        }

        if self.current % self.percentage == 0 {
            return self.progress.report((self.current * 100) / self.total);
        }

        Ok(())
    }

    // Are we done?
    pub fn is_done(&self) -> bool {
        self.current >= self.total
    }
}

/// A single scan result.
#[derive(Debug)]
pub struct ScanResult {
    /// Address where the scanned value lives.
    pub address: Address,
    /// Value from last scan.
    pub value: scan::Value,
    /// Last seen value.
    pub current: Option<scan::Value>,
    /// If the result is valid, or if it should be pruned.
    pub killed: bool,
}

impl ScanResult {
    /// An improved display implemented with a process handle.
    pub fn address_display<'a>(&'a self, handle: &'a ProcessHandle) -> AddressDisplay<'a> {
        AddressDisplay {
            result: self,
            handle,
        }
    }
}

// A displayed scan result.
pub struct AddressDisplay<'a> {
    result: &'a ScanResult,
    handle: &'a ProcessHandle,
}

impl<'a> fmt::Display for AddressDisplay<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let AddressDisplay {
            ref handle,
            ref result,
        } = *self;

        let ScanResult { address, .. } = *result;

        let location = match handle.find_location(*address).ok() {
            Some(location) => location,
            None => {
                write!(fmt, "{}", address)?;
                return Ok(());
            }
        };

        match location {
            Location::Module(module) => match address.offset_of(module.range.base).ok() {
                Some(offset) => {
                    write!(fmt, "{}{}", module.name, offset)?;
                }
                None => {
                    write!(fmt, "{}+? ({})", module.name, address)?;
                }
            },
            Location::Thread(thread) => {
                let base = thread
                    .stack_exit
                    .as_ref()
                    .cloned()
                    .unwrap_or(thread.stack.base);

                match address.offset_of(base).ok() {
                    Some(offset) => {
                        write!(fmt, "THREADSTACK{}{}", thread.id, offset)?;
                    }
                    None => {
                        write!(fmt, "THREADSTACK{}+? ({})", thread.id, address)?;
                    }
                }
            }
            Location::None => {
                write!(fmt, "{}", address)?;
            }
        }

        Ok(())
    }
}

/// A trait to track the progress of processes.
pub trait Progress {
    /// Report the total number of bytes to process.
    fn report_bytes(&mut self, bytes: usize) -> Result<(), failure::Error>;

    /// Report that the process has progresses to the given percentage.
    fn report(&mut self, percentage: usize) -> Result<(), failure::Error>;
}
