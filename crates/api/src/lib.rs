//! The websocket API shared by `ptscan-server` and the web frontend.

use musli::{Decode, Encode};
use musli_web::api;

/// The default address the server binds to.
pub const DEFAULT_BIND: &str = "127.0.0.1:7431";

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
}
