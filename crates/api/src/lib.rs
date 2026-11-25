//! The websocket API shared by `ptscan` and the web frontend.

mod config_field;
mod config_key;

pub use self::config_field::{ConfigField, UnknownConfigField};
pub use self::config_key::{
    ConfigKey, ConfigKeyErrorKind, ConfigKeyFull, ConfigSegment, MAX_SEGMENTS, ParseConfigKeyError,
};

use musli::{Decode, Encode};
use musli_web::api;

/// The default address the server binds to.
pub const DEFAULT_BIND: &str = "127.0.0.1:7431";

/// Most scan results a single page read returns.
pub const MAX_SCAN_PAGE: u32 = 1000;

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct Empty;

#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode)]
pub struct ProcessInfo {
    pub pid: u32,
    pub name: Option<String>,
}

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct ListProcessesRequest;

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct ListProcessesResponse {
    pub processes: Vec<ProcessInfo>,
    /// Processes which exist and may be opened but could not be queried, with
    /// the reason.
    pub errors: Vec<String>,
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct AttachRequest {
    pub pid: u32,
}

/// The outcome of an attach: the resulting state, or why it failed.
#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct AttachResponse {
    pub state: AttachState,
    pub error: Option<String>,
}

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct DetachRequest;

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct GetAttachStateRequest;

/// Which process the server is attached to, if any.
#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct AttachState {
    pub process: Option<ProcessInfo>,
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct ScanRequest {
    /// Filter expression, such as `value == 42u32`.
    pub filter: String,
    /// Value expression; empty means the value itself.
    pub value: String,
    pub alignment: Option<u32>,
    /// Suspend the process for the duration of the scan.
    pub suspend: bool,
    /// Only scan the memory of loaded modules. Applies to initial scans.
    pub modules_only: bool,
}

/// Whether a scan was started, and if not why.
#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct ScanResponse {
    pub filter_error: Option<String>,
    pub value_error: Option<String>,
    pub error: Option<String>,
}

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct CancelScanRequest;

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct ResetScanRequest;

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct UndoScanRequest;

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct RedoScanRequest;

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct GetScanStateRequest;

/// The scan generations held by the server.
#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct ScanState {
    /// Number of scan generations, the last one being current.
    pub generations: u32,
    /// Number of undone generations which can be redone.
    pub undone: u32,
    /// Number of results in the current generation.
    pub results: u64,
    pub running: bool,
    /// The outcome of the last finished scan.
    pub last: Option<ScanOutcome>,
}

impl ScanState {
    pub fn can_undo(&self) -> bool {
        self.generations > 1
    }

    pub fn can_redo(&self) -> bool {
        self.undone > 0
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct ScanOutcome {
    pub results: u64,
    /// The scan was cancelled and its partial results kept.
    pub cancelled: bool,
    /// The rescan replaced the current generation instead of adding one, since
    /// it was too large to copy.
    pub in_place: bool,
    pub error: Option<String>,
    pub duration_ms: u64,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct ScanProgress {
    pub percentage: u32,
    pub results: u64,
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct ScanResultsRequest {
    pub offset: u64,
    pub limit: u32,
}

/// A page of the current generation, with values read now.
#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct ScanResultsResponse {
    pub total: u64,
    pub offset: u64,
    pub results: Vec<ScanResult>,
    pub error: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode)]
pub struct ScanResult {
    pub address: u64,
    /// The address relative to its module or thread stack, if any.
    pub pointer: String,
    pub value_type: String,
    /// The value recorded by the last scan.
    pub last: String,
    /// The value in memory when the page was read.
    pub current: String,
}

/// Remove results from the current generation, which is edited in place.
#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct RemoveScanResultsRequest {
    pub addresses: Vec<u64>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct RemoveScanResultsResponse {
    pub state: ScanState,
    pub removed: u64,
    pub error: Option<String>,
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct WriteScanValueRequest {
    pub address: u64,
    pub value_type: String,
    pub value: String,
}

/// The value at the address after a write, or why it was refused.
#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct WriteScanValueResponse {
    pub current: Option<String>,
    pub value_type_error: Option<String>,
    pub value_error: Option<String>,
    pub error: Option<String>,
}

/// An entry of the scratch list, with its value read now.
#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode)]
pub struct ScratchEntry {
    pub id: u64,
    pub pointer: String,
    pub value_type: String,
    /// The address the pointer currently follows to.
    pub address: Option<u64>,
    pub value: Option<String>,
    /// Why a stored pointer or type cannot be used, such as one saved in a
    /// syntax this build no longer reads.
    pub error: Option<String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct ScratchList {
    pub entries: Vec<ScratchEntry>,
}

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct GetScratchRequest;

#[derive(Debug, Clone, Encode, Decode)]
pub struct AddScratchRequest {
    /// A pointer such as `0x1234` or a scan result's `pointer`.
    pub pointer: String,
    pub value_type: String,
}

/// Change the pointer or type of an entry; `None` keeps it.
#[derive(Debug, Clone, Encode, Decode)]
pub struct EditScratchRequest {
    pub id: u64,
    pub pointer: Option<String>,
    pub value_type: Option<String>,
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct WriteScratchValueRequest {
    pub id: u64,
    pub value: String,
}

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct RemoveScratchRequest {
    pub ids: Vec<u64>,
}

/// Put the entries `ids` first, in that order; the rest follow in the order
/// they had.
#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct ReorderScratchRequest {
    pub ids: Vec<u64>,
}

/// The entry after a change, or why the change was refused.
#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct ScratchResponse {
    pub entry: Option<ScratchEntry>,
    pub pointer_error: Option<String>,
    pub value_type_error: Option<String>,
    pub value_error: Option<String>,
    pub error: Option<String>,
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct PointerScanRequest {
    /// The scratch entry whose address to find pointer paths to.
    pub id: u64,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct PointerScanResponse {
    pub error: Option<String>,
}

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct CancelPointerScanRequest;

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct GetPointerScanStateRequest;

/// The last pointer scan held by the server.
#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct PointerScanState {
    /// The scratch entry scanned for.
    pub entry: Option<u64>,
    /// The scanned entry's type, to add found paths to the scratch list with.
    pub value_type: Option<String>,
    /// Number of paths found by the last finished scan.
    pub results: u64,
    pub running: bool,
    pub last: Option<PointerScanOutcome>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct PointerScanOutcome {
    pub results: u64,
    /// The scan was cancelled and the paths found so far kept.
    pub cancelled: bool,
    pub error: Option<String>,
    pub duration_ms: u64,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Encode, Decode)]
pub enum PointerScanStage {
    /// Scanning memory for pointers.
    #[default]
    Initial,
    /// Following pointers back from the address.
    Paths,
    /// Adding paths through already visited pointers.
    Backreferences,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct PointerScanProgress {
    pub stage: PointerScanStage,
    /// Progress of the initial stage.
    pub percentage: u32,
    /// Pointers queued or paths left to visit in the later stages.
    pub remaining: u64,
    pub results: u64,
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct PointerScanResultsRequest {
    pub offset: u64,
    pub limit: u32,
}

/// A page of the paths found by the last pointer scan, at most
/// [`MAX_SCAN_PAGE`] long.
#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct PointerScanResultsResponse {
    pub total: u64,
    pub offset: u64,
    /// Pointers in the syntax [`AddScratchRequest::pointer`] takes.
    pub results: Vec<String>,
    pub value_type: Option<String>,
}

/// Which memory regions [`GetProcessInformation`] returns; every set flag
/// must hold.
#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct MemoryFilter {
    pub readable: bool,
    pub writable: bool,
    pub executable: bool,
    pub modules: bool,
    pub thread_stacks: bool,
    /// Only regions which are neither accessible nor part of a module or
    /// thread stack; overrides the other flags.
    pub other: bool,
}

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct ProcessInformationRequest {
    pub filter: MemoryFilter,
}

#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode)]
pub struct ModuleInformation {
    pub name: String,
    pub file_name: Option<String>,
    pub base: u64,
    pub size: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode)]
pub struct ThreadInformation {
    pub id: u32,
    pub stack_base: u64,
    pub stack_size: u64,
    pub stack_exit: Option<u64>,
}

#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode)]
pub struct MemoryRegion {
    pub base: u64,
    pub size: u64,
    /// Such as `MEM_COMMIT`.
    pub state: String,
    /// Such as `MEM_PRIVATE`, if known.
    pub ty: Option<String>,
    /// Such as `PAGE_READWRITE`.
    pub protect: Vec<String>,
    pub readable: bool,
    pub writable: bool,
    pub executable: bool,
    /// The module the region belongs to.
    pub module: Option<String>,
    /// The thread whose stack the region holds.
    pub thread_stack: Option<u32>,
}

/// The attached process's modules, threads and filtered memory regions, read
/// when requested.
#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct ProcessInformation {
    pub name: Option<String>,
    pub file_name: Option<String>,
    pub modules: Vec<ModuleInformation>,
    pub threads: Vec<ThreadInformation>,
    pub regions: Vec<MemoryRegion>,
    pub error: Option<String>,
}

/// The process a session targets. Pids do not survive restarts, so it is
/// remembered by name.
#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode)]
pub struct SessionTarget {
    pub name: String,
    /// The executable, where known.
    pub path: Option<String>,
}

/// The scan form of a session, as [`ScanRequest`] takes it.
#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct ScanForm {
    pub filter: String,
    pub value: String,
    pub alignment: Option<u32>,
    pub suspend: bool,
    pub modules_only: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode)]
pub struct PointerScanSettings {
    /// Most pointers followed in one path.
    pub max_depth: u32,
    /// Largest offset added to a pointer at each step.
    pub max_offset: u32,
}

impl PointerScanSettings {
    pub const MAX_DEPTH: u32 = 16;
    pub const MAX_OFFSET: u32 = 0x10_0000;
}

impl Default for PointerScanSettings {
    fn default() -> Self {
        Self {
            max_depth: 7,
            max_offset: 0x1000,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode)]
pub struct SessionInfo {
    pub id: u64,
    pub name: String,
    /// Unix time in seconds.
    pub created: u64,
    /// Unix time in seconds.
    pub opened: u64,
    pub target: Option<SessionTarget>,
}

/// The sessions, and the settings of the open one.
#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct SessionState {
    /// The open session; `None` when the server keeps no database.
    pub current: Option<u64>,
    /// Every session, most recently opened first.
    pub sessions: Vec<SessionInfo>,
    pub scan: ScanForm,
    pub pointer_scan: PointerScanSettings,
}

impl SessionState {
    pub fn current(&self) -> Option<&SessionInfo> {
        let id = self.current?;
        self.sessions.iter().find(|s| s.id == id)
    }
}

/// The sessions after a change, or why it was refused.
#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct SessionResponse {
    pub state: SessionState,
    pub error: Option<String>,
}

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct GetSessionStateRequest;

#[derive(Debug, Clone, Encode, Decode)]
pub struct CreateSessionRequest {
    pub name: String,
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct RenameSessionRequest {
    pub id: u64,
    pub name: String,
}

/// Switch to a session: detaches, drops scan and pointer-scan results and
/// loads its settings.
#[derive(Debug, Clone, Encode, Decode)]
pub struct OpenSessionRequest {
    pub id: u64,
}

/// Delete a session other than the open one.
#[derive(Debug, Clone, Encode, Decode)]
pub struct DeleteSessionRequest {
    pub id: u64,
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct SetScanFormRequest {
    pub form: ScanForm,
}

#[derive(Debug, Clone, Encode, Decode)]
pub struct SetPointerScanSettingsRequest {
    pub settings: PointerScanSettings,
}

api::define! {
    pub type ListProcesses;
    impl Endpoint for ListProcesses {
        impl Request for ListProcessesRequest;
        type Response<'de> = ListProcessesResponse;
    }

    pub type Attach;
    impl Endpoint for Attach {
        impl Request for AttachRequest;
        type Response<'de> = AttachResponse;
    }

    pub type Detach;
    impl Endpoint for Detach {
        impl Request for DetachRequest;
        type Response<'de> = AttachState;
    }

    pub type GetAttachState;
    impl Endpoint for GetAttachState {
        impl Request for GetAttachStateRequest;
        type Response<'de> = AttachState;
    }

    pub type AttachStateChanged;
    impl Broadcast for AttachStateChanged {
        impl Event for AttachState;
    }
    pub type Scan;
    impl Endpoint for Scan {
        impl Request for ScanRequest;
        type Response<'de> = ScanResponse;
    }

    pub type CancelScan;
    impl Endpoint for CancelScan {
        impl Request for CancelScanRequest;
        type Response<'de> = ScanState;
    }

    pub type ResetScan;
    impl Endpoint for ResetScan {
        impl Request for ResetScanRequest;
        type Response<'de> = ScanState;
    }

    pub type UndoScan;
    impl Endpoint for UndoScan {
        impl Request for UndoScanRequest;
        type Response<'de> = ScanState;
    }

    pub type RedoScan;
    impl Endpoint for RedoScan {
        impl Request for RedoScanRequest;
        type Response<'de> = ScanState;
    }

    pub type GetScanState;
    impl Endpoint for GetScanState {
        impl Request for GetScanStateRequest;
        type Response<'de> = ScanState;
    }

    pub type ScanResults;
    impl Endpoint for ScanResults {
        impl Request for ScanResultsRequest;
        type Response<'de> = ScanResultsResponse;
    }

    pub type RemoveScanResults;
    impl Endpoint for RemoveScanResults {
        impl Request for RemoveScanResultsRequest;
        type Response<'de> = RemoveScanResultsResponse;
    }

    pub type WriteScanValue;
    impl Endpoint for WriteScanValue {
        impl Request for WriteScanValueRequest;
        type Response<'de> = WriteScanValueResponse;
    }

    pub type ScanStateChanged;
    impl Broadcast for ScanStateChanged {
        impl Event for ScanState;
    }

    pub type ScanProgressed;
    impl Broadcast for ScanProgressed {
        impl Event for ScanProgress;
    }

    pub type GetScratch;
    impl Endpoint for GetScratch {
        impl Request for GetScratchRequest;
        type Response<'de> = ScratchList;
    }

    pub type AddScratch;
    impl Endpoint for AddScratch {
        impl Request for AddScratchRequest;
        type Response<'de> = ScratchResponse;
    }

    pub type EditScratch;
    impl Endpoint for EditScratch {
        impl Request for EditScratchRequest;
        type Response<'de> = ScratchResponse;
    }

    pub type WriteScratchValue;
    impl Endpoint for WriteScratchValue {
        impl Request for WriteScratchValueRequest;
        type Response<'de> = ScratchResponse;
    }

    pub type RemoveScratch;
    impl Endpoint for RemoveScratch {
        impl Request for RemoveScratchRequest;
        type Response<'de> = ScratchList;
    }

    pub type ReorderScratch;
    impl Endpoint for ReorderScratch {
        impl Request for ReorderScratchRequest;
        type Response<'de> = ScratchList;
    }

    pub type ScratchChanged;
    impl Broadcast for ScratchChanged {
        impl Event for ScratchList;
    }

    pub type PointerScan;
    impl Endpoint for PointerScan {
        impl Request for PointerScanRequest;
        type Response<'de> = PointerScanResponse;
    }

    pub type CancelPointerScan;
    impl Endpoint for CancelPointerScan {
        impl Request for CancelPointerScanRequest;
        type Response<'de> = PointerScanState;
    }

    pub type GetPointerScanState;
    impl Endpoint for GetPointerScanState {
        impl Request for GetPointerScanStateRequest;
        type Response<'de> = PointerScanState;
    }

    pub type PointerScanResults;
    impl Endpoint for PointerScanResults {
        impl Request for PointerScanResultsRequest;
        type Response<'de> = PointerScanResultsResponse;
    }

    pub type PointerScanStateChanged;
    impl Broadcast for PointerScanStateChanged {
        impl Event for PointerScanState;
    }

    pub type PointerScanProgressed;
    impl Broadcast for PointerScanProgressed {
        impl Event for PointerScanProgress;
    }

    pub type GetProcessInformation;
    impl Endpoint for GetProcessInformation {
        impl Request for ProcessInformationRequest;
        type Response<'de> = ProcessInformation;
    }

    pub type GetSessionState;
    impl Endpoint for GetSessionState {
        impl Request for GetSessionStateRequest;
        type Response<'de> = SessionState;
    }

    pub type CreateSession;
    impl Endpoint for CreateSession {
        impl Request for CreateSessionRequest;
        type Response<'de> = SessionResponse;
    }

    pub type RenameSession;
    impl Endpoint for RenameSession {
        impl Request for RenameSessionRequest;
        type Response<'de> = SessionResponse;
    }

    pub type OpenSession;
    impl Endpoint for OpenSession {
        impl Request for OpenSessionRequest;
        type Response<'de> = SessionResponse;
    }

    pub type DeleteSession;
    impl Endpoint for DeleteSession {
        impl Request for DeleteSessionRequest;
        type Response<'de> = SessionResponse;
    }

    pub type SetScanForm;
    impl Endpoint for SetScanForm {
        impl Request for SetScanFormRequest;
        type Response<'de> = SessionResponse;
    }

    pub type SetPointerScanSettings;
    impl Endpoint for SetPointerScanSettings {
        impl Request for SetPointerScanSettingsRequest;
        type Response<'de> = SessionResponse;
    }

    pub type SessionStateChanged;
    impl Broadcast for SessionStateChanged {
        impl Event for SessionState;
    }
}
