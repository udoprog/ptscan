use musli_web::web03::prelude::*;
use sockel::{
    Button, ButtonSize, ButtonVariant, Card, EmptyState, InputSize, Inset, Pill, Table, TableCell,
    TextInput, Tone, Toolbar, ToolbarSpacer,
};
use yew::prelude::*;

use crate::class;

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

        let on_input = link.callback(Msg::Filter);

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
                    sockel::class::TABLE_ROW,
                    class::ROW,
                    (self.selected == Some(pid)).then_some(class::SELECTED),
                );

                html! {
                    <tr key={pid} {class}
                        onclick={link.callback(move |_| Msg::Select(pid))}
                        ondblclick={link.callback(move |_| Msg::Attach(pid))}>
                        <TableCell numeric=true>{pid}</TableCell>
                        <TableCell cut=true>{process.name.as_deref().unwrap_or("")}</TableCell>
                        <TableCell>
                            if attached == Some(pid) {
                                <Pill tone={Tone::Success}>{"attached"}</Pill>
                            }
                        </TableCell>
                    </tr>
                }
            })
            .collect::<Html>();

        let summary = match self.unreadable.len() {
            0 => html! {},
            n => html! {
                <span class={class::MUTED} title={self.unreadable.join("\n")}>
                    {format!("{n} could not be read")}
                </span>
            },
        };

        let head = html! {
            <>
                <TableCell head=true numeric=true>{"PID"}</TableCell>
                <TableCell head=true>{"Name"}</TableCell>
                <TableCell head=true><span class={sockel::class::VISUALLY_HIDDEN}>{"State"}</span></TableCell>
            </>
        };

        let empty = self.processes.is_empty().then(|| {
            let title = if props.open {
                "No processes"
            } else {
                "Waiting for the server"
            };
            html! { <EmptyState {title} compact=true /> }
        });

        html! {
            <Card inset={Inset::Flush} class={class::PANE}>
                <Toolbar label="Processes" band=true>
                    <span class={class::TITLE}>{"Processes"}</span>
                    <TextInput size={InputSize::Small}
                        class={class::SEARCH}
                        placeholder="Filter by name or pid" label="Filter processes"
                        value={self.filter.clone()} {on_input} />
                    <Button size={ButtonSize::Small} busy={self.list.is_pending()}
                        onclick={link.callback(|_| Msg::Refresh)} disabled={!props.open}>
                        {"Refresh"}
                    </Button>
                </Toolbar>
                <Toolbar label="Process actions" band=true>
                    {summary}
                    <ToolbarSpacer />
                    <Button size={ButtonSize::Small} variant={ButtonVariant::Primary}
                        busy={busy} disabled={attach.is_none()}
                        onclick={attach.unwrap_or_default()}>{"Attach"}</Button>
                    <Button size={ButtonSize::Small} disabled={detach.is_none()}
                        onclick={detach.unwrap_or_default()}>{"Detach"}</Button>
                </Toolbar>
                <div class={class::SCROLL}>
                    <Table {head} columns={3} {empty} label="Processes">{rows}</Table>
                </div>
            </Card>
        }
    }
}
