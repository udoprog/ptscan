use musli_web::web03::prelude::*;
use sockel::{Button, ButtonSize, Count, Gap, Pill, Stack, Tone, Toolbar, ToolbarSpacer};
use yew::prelude::*;

use crate::class;
use crate::error::ErrorEntry;
use crate::ui::error_log::ErrorLog;
use crate::ui::pointer_scan_view::PointerScanView;
use crate::ui::process_information::ProcessInformation;
use crate::ui::process_picker::ProcessPicker;
use crate::ui::scan_view::ScanView;
use crate::ui::scratch_view::ScratchView;
use crate::ui::session_dialog::SessionDialog;

pub enum Msg {
    Connection(ws::State),
    WebSocketError(ws::Error),
    AttachState(Result<ws::Packet<api::GetAttachState>, ws::Error>),
    AttachChanged(Result<ws::Packet<api::AttachStateChanged>, ws::Error>),
    SessionState(Result<ws::Packet<api::GetSessionState>, ws::Error>),
    SessionChanged(Result<ws::Packet<api::SessionStateChanged>, ws::Error>),
    ToggleSessions,
    Error(String),
    ToggleErrors,
    ToggleProcessInformation,
    ClearErrors,
}

pub struct App {
    service: ws::Service,
    _state_listener: ws::StateListener,
    _attach_listener: ws::Listener,
    _session_listener: ws::Listener,
    attach_request: ws::Request,
    session_request: ws::Request,
    connection: ws::State,
    attach: api::AttachState,
    session: api::SessionState,
    show_sessions: bool,
    errors: Vec<ErrorEntry>,
    show_errors: bool,
    show_process_information: bool,
    on_error: Callback<String>,
}

impl App {
    fn raise(&mut self, message: String) -> bool {
        self.errors.push(ErrorEntry::now(message));
        true
    }

    fn request_attach_state(&mut self, ctx: &Context<Self>) {
        self.attach_request = self
            .service
            .handle()
            .request()
            .body(api::GetAttachStateRequest)
            .on_packet(ctx.link().callback(Msg::AttachState))
            .send();

        self.session_request = self
            .service
            .handle()
            .request()
            .body(api::GetSessionStateRequest)
            .on_packet(ctx.link().callback(Msg::SessionState))
            .send();
    }
}

impl Component for App {
    type Message = Msg;
    type Properties = ();

    fn create(ctx: &Context<Self>) -> Self {
        let service = ws::connect(ws::Connect::location("ws"))
            .on_error(ctx.link().callback(Msg::WebSocketError))
            .close_before_unload()
            .build();

        service.open();

        let (connection, state_listener) = service
            .handle()
            .on_state_change(ctx.link().callback(Msg::Connection));

        let attach_listener = service
            .handle()
            .on_broadcast(ctx.link().callback(Msg::AttachChanged));

        let session_listener = service
            .handle()
            .on_broadcast(ctx.link().callback(Msg::SessionChanged));

        let mut this = Self {
            service,
            _state_listener: state_listener,
            _attach_listener: attach_listener,
            _session_listener: session_listener,
            attach_request: ws::Request::default(),
            session_request: ws::Request::default(),
            connection,
            attach: api::AttachState::default(),
            session: api::SessionState::default(),
            show_sessions: false,
            errors: Vec::new(),
            show_errors: false,
            show_process_information: false,
            on_error: ctx.link().callback(Msg::Error),
        };

        if this.connection.is_open() {
            this.request_attach_state(ctx);
        }

        this
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::Connection(state) => {
                let lost = self.connection.is_open() && !state.is_open();
                self.connection = state;

                if self.connection.is_open() {
                    self.request_attach_state(ctx);
                } else {
                    self.attach = api::AttachState::default();
                }

                if lost {
                    self.raise(String::from("Lost connection to the server"));
                }

                true
            }
            Msg::WebSocketError(error) => self.raise(format!("Connection: {error}")),
            Msg::AttachState(result) => {
                match result.and_then(|packet| packet.decode_response()) {
                    Ok(state) => self.attach = state,
                    Err(error) => return self.raise(format!("Reading attach state: {error}")),
                }

                true
            }
            Msg::AttachChanged(result) => {
                match result.and_then(|packet| packet.decode_event()) {
                    Ok(state) => self.attach = state,
                    Err(error) => return self.raise(format!("Attach state update: {error}")),
                }

                true
            }
            Msg::SessionState(result) => {
                match result.and_then(|packet| packet.decode_response()) {
                    Ok(state) => self.session = state,
                    Err(error) => return self.raise(format!("Reading sessions: {error}")),
                }

                true
            }
            Msg::SessionChanged(result) => {
                match result.and_then(|packet| packet.decode_event()) {
                    Ok(state) => self.session = state,
                    Err(error) => return self.raise(format!("Session update: {error}")),
                }

                true
            }
            Msg::ToggleSessions => {
                self.show_sessions = !self.show_sessions;
                true
            }
            Msg::Error(message) => self.raise(message),
            Msg::ToggleErrors => {
                self.show_errors = !self.show_errors;
                true
            }
            Msg::ToggleProcessInformation => {
                self.show_process_information = !self.show_process_information;
                true
            }
            Msg::ClearErrors => {
                self.errors.clear();
                true
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let (tone, label) = match self.connection {
            ws::State::Open => (Tone::Success, String::from("Connected")),
            ws::State::Closed => (Tone::Warning, String::from("Connecting...")),
            ws::State::Incompatible { server, client } => (
                Tone::Danger,
                format!("Incompatible server (version {server}, page {client}); reload the page"),
            ),
            _ => (Tone::Warning, String::from("Disconnected")),
        };

        let attached = match &self.attach.process {
            Some(process) => html! {
                <span>{format!("Attached to {}", describe(process))}</span>
            },
            None => html! { <span class={class::MUTED}>{"Not attached"}</span> },
        };

        let session_name = match self.session.current() {
            Some(session) => session.name.clone(),
            None => String::from("No session"),
        };

        let error_count = u32::try_from(self.errors.len()).unwrap_or(u32::MAX);
        let error_after = (error_count > 0).then(|| html! { <Count value={error_count} /> });

        html! {
            <Stack gap={Gap::None} class={class::APP}>
                <Toolbar label="Application" band=true chrome=true>
                    <span class={class::TITLE}>{"ptscan"}</span>
                    <Pill {tone}>{label}</Pill>
                    {attached}
                    <ToolbarSpacer />
                    <Button size={ButtonSize::Small}
                        disabled={self.session.current.is_none()}
                        title="Switch, create, rename and delete sessions"
                        onclick={ctx.link().callback(|_| Msg::ToggleSessions)}>
                        {format!("Session: {session_name}")}
                    </Button>
                    <Button size={ButtonSize::Small}
                        disabled={self.attach.process.is_none()}
                        title="Show the attached process's modules, threads and memory regions"
                        onclick={ctx.link().callback(|_| Msg::ToggleProcessInformation)}>
                        {"Process info"}
                    </Button>
                    <Button size={ButtonSize::Small}
                        after={error_after}
                        onclick={ctx.link().callback(|_| Msg::ToggleErrors)}>
                        {"Errors"}
                    </Button>
                </Toolbar>
                <div class={class::WORKSPACE}>
                    <div class={class::SIDE}>
                        <ProcessPicker
                            ws={self.service.handle().clone()}
                            open={self.connection.is_open()}
                            attach={self.attach.clone()}
                            target={self.session.current().and_then(|s| s.target.clone())}
                            on_error={self.on_error.clone()}
                        />
                    </div>
                    <div class={class::MAIN}>
                        <ScanView
                            ws={self.service.handle().clone()}
                            open={self.connection.is_open()}
                            attached={self.attach.process.is_some()}
                            session={self.session.current}
                            form={self.session.scan.clone()}
                            on_error={self.on_error.clone()}
                        />
                        <div class={class::LOWER}>
                            <ScratchView
                                ws={self.service.handle().clone()}
                                open={self.connection.is_open()}
                                attached={self.attach.process.is_some()}
                                on_error={self.on_error.clone()}
                            />
                            <PointerScanView
                                ws={self.service.handle().clone()}
                                open={self.connection.is_open()}
                                attached={self.attach.process.is_some()}
                                session={self.session.current}
                                settings={self.session.pointer_scan.clone()}
                                on_error={self.on_error.clone()}
                            />
                        </div>
                    </div>
                </div>
                <ProcessInformation
                    open={self.show_process_information}
                    ws={self.service.handle().clone()}
                    connected={self.connection.is_open()}
                    process={self.attach.process.clone()}
                    on_error={self.on_error.clone()}
                    on_close={ctx.link().callback(|()| Msg::ToggleProcessInformation)}
                />
                <SessionDialog
                    open={self.show_sessions}
                    ws={self.service.handle().clone()}
                    connected={self.connection.is_open()}
                    state={self.session.clone()}
                    on_close={ctx.link().callback(|()| Msg::ToggleSessions)}
                />
                <ErrorLog
                    open={self.show_errors}
                    errors={self.errors.clone()}
                    on_clear={ctx.link().callback(|()| Msg::ClearErrors)}
                    on_close={ctx.link().callback(|()| Msg::ToggleErrors)}
                />
            </Stack>
        }
    }
}

pub fn describe(process: &api::ProcessInfo) -> String {
    match &process.name {
        Some(name) => format!("{name} ({})", process.pid),
        None => format!("pid {}", process.pid),
    }
}
