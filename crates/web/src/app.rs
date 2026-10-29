use musli_web::web03::prelude::*;
use yew::prelude::*;

use crate::error::ErrorEntry;
use crate::ui::error_log::ErrorLog;
use crate::ui::pointer_scan_view::PointerScanView;
use crate::ui::process_information::ProcessInformation;
use crate::ui::process_picker::ProcessPicker;
use crate::ui::scan_view::ScanView;
use crate::ui::scratch_view::ScratchView;

pub enum Msg {
    Connection(ws::State),
    WebSocketError(ws::Error),
    AttachState(Result<ws::Packet<api::GetAttachState>, ws::Error>),
    AttachChanged(Result<ws::Packet<api::AttachStateChanged>, ws::Error>),
    Error(String),
    ToggleErrors,
    ToggleProcessInformation,
    ClearErrors,
}

pub struct App {
    service: ws::Service,
    _state_listener: ws::StateListener,
    _attach_listener: ws::Listener,
    attach_request: ws::Request,
    connection: ws::State,
    attach: api::AttachState,
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

        let mut this = Self {
            service,
            _state_listener: state_listener,
            _attach_listener: attach_listener,
            attach_request: ws::Request::default(),
            connection,
            attach: api::AttachState::default(),
            errors: Vec::new(),
            show_errors: false,
            show_process_information: false,
            on_error: ctx.link().callback(Msg::Error),
        };

        if connection.is_open() {
            this.request_attach_state(ctx);
        }

        this
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::Connection(state) => {
                let lost = self.connection.is_open() && !state.is_open();
                self.connection = state;

                if state.is_open() {
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
        let (class, label) = match self.connection {
            ws::State::Open => ("open", String::from("Connected")),
            ws::State::Closed => ("closed", String::from("Connecting...")),
            ws::State::Incompatible { server, client } => (
                "incompatible",
                format!("Incompatible server (version {server}, page {client}); reload the page"),
            ),
            _ => ("closed", String::from("Disconnected")),
        };

        let attached = match &self.attach.process {
            Some(process) => html! {
                <span>{format!("Attached to {}", describe(process))}</span>
            },
            None => html! { <span class="muted">{"Not attached"}</span> },
        };

        let errors = match self.errors.len() {
            0 => html! { {"Errors"} },
            n => html! { <>{"Errors "}<span class="badge">{format!("({n})")}</span></> },
        };

        html! {
            <column class="app" style="gap: 0">
                <row class="toolbar center">
                    <span class="title">{"ptscan"}</span>
                    <span class={classes!("status", class)}>{label}</span>
                    {attached}
                    <div class="fill" />
                    <button disabled={self.attach.process.is_none()}
                        title="Show the attached process's modules, threads and memory regions"
                        onclick={ctx.link().callback(|_| Msg::ToggleProcessInformation)}>{"Process info"}</button>
                    <button onclick={ctx.link().callback(|_| Msg::ToggleErrors)}>{errors}</button>
                </row>
                <row class="body fill">
                    <column class="side">
                        <ProcessPicker
                            ws={self.service.handle().clone()}
                            open={self.connection.is_open()}
                            attach={self.attach.clone()}
                            on_error={self.on_error.clone()}
                        />
                    </column>
                    <column class="fill main">
                        <ScanView
                            ws={self.service.handle().clone()}
                            open={self.connection.is_open()}
                            attached={self.attach.process.is_some()}
                            on_error={self.on_error.clone()}
                        />
                        <row class="lower">
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
                                on_error={self.on_error.clone()}
                            />
                        </row>
                    </column>
                </row>
                if self.show_process_information {
                    <ProcessInformation
                        ws={self.service.handle().clone()}
                        open={self.connection.is_open()}
                        process={self.attach.process.clone()}
                        on_error={self.on_error.clone()}
                        on_close={ctx.link().callback(|()| Msg::ToggleProcessInformation)}
                    />
                }
                if self.show_errors {
                    <ErrorLog
                        errors={self.errors.clone()}
                        on_clear={ctx.link().callback(|()| Msg::ClearErrors)}
                        on_close={ctx.link().callback(|()| Msg::ToggleErrors)}
                    />
                }
            </column>
        }
    }
}

pub fn describe(process: &api::ProcessInfo) -> String {
    match &process.name {
        Some(name) => format!("{name} ({})", process.pid),
        None => format!("pid {}", process.pid),
    }
}
