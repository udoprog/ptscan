//! The websocket API shared by `ptscan-server` and the web frontend.

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
    /// Processes which could not be queried, with the reason.
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

/// An entry of the scratch list, with its value read now.
#[derive(Debug, Clone, PartialEq, Eq, Encode, Decode)]
pub struct ScratchEntry {
    pub id: u64,
    pub pointer: String,
    pub value_type: String,
    /// The address the pointer currently follows to.
    pub address: Option<u64>,
    pub value: Option<String>,
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

/// The entry after a change, or why the change was refused.
#[derive(Debug, Clone, Default, PartialEq, Eq, Encode, Decode)]
pub struct ScratchResponse {
    pub entry: Option<ScratchEntry>,
    pub pointer_error: Option<String>,
    pub value_type_error: Option<String>,
    pub value_error: Option<String>,
    pub error: Option<String>,
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

    pub type ScratchChanged;
    impl Broadcast for ScratchChanged {
        impl Event for ScratchList;
    }
}
