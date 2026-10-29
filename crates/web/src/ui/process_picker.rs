use musli_web::web03::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::HtmlInputElement;
use yew::prelude::*;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub ws: ws::Handle,
    /// The connection is open.
    pub open: bool,
    pub attach: api::AttachState,
    pub on_error: Callback<String>,
}

pub enum Msg {
    Refresh,
    Listed(Result<ws::Packet<api::ListProcesses>, ws::Error>),
    Filter(String),
    Select(u32),
    Attach(u32),
    Attached(Result<ws::Packet<api::Attach>, ws::Error>),
    Detach,
    Detached(Result<ws::Packet<api::Detach>, ws::Error>),
}

pub struct ProcessPicker {
    processes: Vec<api::ProcessInfo>,
    unreadable: Vec<String>,
    filter: String,
    selected: Option<u32>,
    list: ws::Request,
    action: ws::Request,
}

impl ProcessPicker {
    fn refresh(&mut self, ctx: &Context<Self>) {
        self.list = ctx
            .props()
            .ws
            .request()
            .body(api::ListProcessesRequest)
            .on_packet(ctx.link().callback(Msg::Listed))
            .send();
    }

    fn matches(&self, process: &api::ProcessInfo) -> bool {
        if self.filter.is_empty() {
            return true;
        }

        let filter = self.filter.to_lowercase();

        process.pid.to_string().contains(&filter)
            || process
                .name
                .as_deref()
                .is_some_and(|name| name.to_lowercase().contains(&filter))
    }
}

impl Component for ProcessPicker {
    type Message = Msg;
    type Properties = Props;

    fn create(ctx: &Context<Self>) -> Self {
        let mut this = Self {
            processes: Vec::new(),
            unreadable: Vec::new(),
            filter: String::new(),
            selected: None,
            list: ws::Request::default(),
            action: ws::Request::default(),
        };

        if ctx.props().open {
            this.refresh(ctx);
        }

        this
    }

    fn changed(&mut self, ctx: &Context<Self>, old: &Self::Properties) -> bool {
        if ctx.props().open && !old.open {
            self.refresh(ctx);
        }

        true
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        let on_error = &ctx.props().on_error;

        match msg {
            Msg::Refresh => {
                self.refresh(ctx);
                true
            }
            Msg::Listed(result) => {
                match result.and_then(|packet| packet.decode_response()) {
                    Ok(response) => {
                        self.processes = response.processes;
                        self.processes.sort_by_key(|p| p.pid);
                        self.unreadable = response.errors;
                    }
                    Err(error) => on_error.emit(format!("Listing processes: {error}")),
                }

                true
            }
            Msg::Filter(filter) => {
                self.filter = filter;
                true
            }
            Msg::Select(pid) => {
                self.selected = Some(pid);
                true
            }
            Msg::Attach(pid) => {
                self.action = ctx
                    .props()
                    .ws
                    .request()
                    .body(api::AttachRequest { pid })
                    .on_packet(ctx.link().callback(Msg::Attached))
                    .send();
                true
            }
            Msg::Attached(result) => {
                match result.and_then(|packet| packet.decode_response()) {
                    Ok(api::AttachResponse {
                        error: Some(error), ..
                    }) => on_error.emit(format!("Attaching: {error}")),
                    Ok(_) => {}
                    Err(error) => on_error.emit(format!("Attaching: {error}")),
                }

                true
            }
            Msg::Detach => {
                self.action = ctx
                    .props()
                    .ws
                    .request()
                    .body(api::DetachRequest)
                    .on_packet(ctx.link().callback(Msg::Detached))
                    .send();
                true
            }
            Msg::Detached(result) => {
                if let Err(error) = result.and_then(|packet| packet.decode_response().map(drop)) {
                    on_error.emit(format!("Detaching: {error}"));
                }

                true
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let props = ctx.props();
        let link = ctx.link();
        let busy = self.action.is_pending();
        let attached = props.attach.process.as_ref().map(|p| p.pid);

        let oninput = link.callback(|e: InputEvent| {
            let value = e
                .target()
                .and_then(|t| t.dyn_into::<HtmlInputElement>().ok())
                .map(|input| input.value())
                .unwrap_or_default();
            Msg::Filter(value)
        });

        let attach = match self.selected {
            Some(pid) if props.open && !busy && attached != Some(pid) => {
                Some(link.callback(move |_| Msg::Attach(pid)))
            }
            _ => None,
        };

        let detach =
            (props.open && !busy && attached.is_some()).then(|| link.callback(|_| Msg::Detach));

        let rows = self
            .processes
            .iter()
            .filter(|process| self.matches(process))
            .map(|process| {
                let pid = process.pid;
                let class = classes!(
                    (self.selected == Some(pid)).then_some("selected"),
                    (attached == Some(pid)).then_some("attached"),
                );

                html! {
                    <tr key={pid} {class}
                        onclick={link.callback(move |_| Msg::Select(pid))}
                        ondblclick={link.callback(move |_| Msg::Attach(pid))}>
                        <td class="pid">{pid}</td>
                        <td>{process.name.as_deref().unwrap_or("")}</td>
                        <td class="muted">{if attached == Some(pid) { "attached" } else { "" }}</td>
                    </tr>
                }
            })
            .collect::<Html>();

        let summary = match self.unreadable.len() {
            0 => html! {},
            n => html! {
                <span class="muted" title={self.unreadable.join("\n")}>
                    {format!("{n} could not be read")}
                </span>
            },
        };

        html! {
            <column class="panel fill" style="gap: 0">
                <row class="toolbar center">
                    <span class="title">{"Processes"}</span>
                    <input type="search" placeholder="Filter by name or pid"
                        value={self.filter.clone()} {oninput} />
                    <button onclick={link.callback(|_| Msg::Refresh)} disabled={!props.open}>
                        {"Refresh"}
                    </button>
                    {summary}
                    <div class="fill" />
                    <button class="primary" disabled={attach.is_none()}
                        onclick={attach.unwrap_or_default()}>{"Attach"}</button>
                    <button disabled={detach.is_none()}
                        onclick={detach.unwrap_or_default()}>{"Detach"}</button>
                </row>
                <div class="list fill">
                    if self.processes.is_empty() {
                        <div class="empty muted">
                            {if props.open { "No processes" } else { "Waiting for the server" }}
                        </div>
                    } else {
                        <table>
                            <thead><tr><th>{"PID"}</th><th>{"Name"}</th><th></th></tr></thead>
                            <tbody>{rows}</tbody>
                        </table>
                    }
                </div>
            </column>
        }
    }
}
