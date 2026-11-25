use musli_web::web03::prelude::*;
use sockel::{
    Button, ButtonSize, ButtonVariant, EmptyState, InputSize, Pill, Table, TableCell, TextInput,
    Tone, Toolbar, ToolbarSpacer,
};
use yew::prelude::*;

use crate::class;
use crate::ui::pane::Pane;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub ws: ws::Handle,
    /// The connection is open.
    pub open: bool,
    pub attach: api::AttachState,
    /// The process the open session targets.
    pub target: Option<api::SessionTarget>,
    pub on_error: Callback<String>,
}

pub enum Msg {
    Refresh,
    Listed(Result<ws::Packet<api::ListProcesses>, ws::Error>),
    Filter(String),
    FilterTarget(String),
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
    /// The target the filter was last set to, so it is only set once.
    filtered_for: Option<String>,
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

    /// Running processes named like the session's target.
    fn targets<'a>(&'a self, ctx: &'a Context<Self>) -> impl Iterator<Item = &'a api::ProcessInfo> {
        let name = ctx.props().target.as_ref().map(|t| t.name.as_str());

        self.processes
            .iter()
            .filter(move |p| name.is_some() && p.name.as_deref() == name)
    }

    /// With several processes named like the target and nothing attached,
    /// filter the list down to them once.
    fn filter_to_target(&mut self, ctx: &Context<Self>) {
        let props = ctx.props();

        let Some(target) = &props.target else {
            return;
        };

        if props.attach.process.is_some()
            || self.filtered_for.as_ref() == Some(&target.name)
            || self.targets(ctx).count() < 2
        {
            return;
        }

        self.filter = target.name.clone();
        self.filtered_for = Some(target.name.clone());
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
            filtered_for: None,
            list: ws::Request::default(),
            action: ws::Request::default(),
        };

        if ctx.props().open {
            this.refresh(ctx);
        }

        this
    }

    fn changed(&mut self, ctx: &Context<Self>, old: &Self::Properties) -> bool {
        let props = ctx.props();

        if props.open && (!old.open || props.target != old.target) {
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
                        self.filter_to_target(ctx);
                    }
                    Err(error) => on_error.emit(format!("Listing processes: {error}")),
                }

                true
            }
            Msg::FilterTarget(name) => {
                self.filter = name;
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

        let offer = self.offer(ctx, busy);

        html! {
            <Pane label="Processes">
                <Toolbar label="Processes" band=true chrome=true>
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
                <Toolbar label="Process actions" band=true chrome=true>
                    {summary}
                    <ToolbarSpacer />
                    <Button size={ButtonSize::Small} variant={ButtonVariant::Primary}
                        busy={busy} disabled={attach.is_none()}
                        onclick={attach.unwrap_or_default()}>{"Attach"}</Button>
                    <Button size={ButtonSize::Small} disabled={detach.is_none()}
                        onclick={detach.unwrap_or_default()}>{"Detach"}</Button>
                </Toolbar>
                {offer}
                <div class={class::SCROLL}>
                    <Table {head} columns={3} {empty} label="Processes">{rows}</Table>
                </div>
            </Pane>
        }
    }
}

impl ProcessPicker {
    /// Offer to attach to the open session's target when nothing is attached.
    fn offer(&self, ctx: &Context<Self>, busy: bool) -> Html {
        let props = ctx.props();
        let link = ctx.link();

        let Some(target) = &props.target else {
            return html! {};
        };

        if props.attach.process.is_some() {
            return html! {};
        }

        let name = target.name.clone();
        let running = self.targets(ctx).map(|p| p.pid).collect::<Vec<_>>();

        let (text, action) = match running.as_slice() {
            [] => (format!("Session target {name} is not running"), html! {}),
            [pid] => {
                let pid = *pid;
                (
                    format!("Session target {name} is running ({pid})"),
                    html! {
                        <Button size={ButtonSize::Small} variant={ButtonVariant::Primary}
                            busy={busy} disabled={!props.open || busy}
                            onclick={link.callback(move |_| Msg::Attach(pid))}>
                            {"Attach"}
                        </Button>
                    },
                )
            }
            several => {
                let show = name.clone();
                (
                    format!(
                        "{} processes are named {name}; pick one to attach",
                        several.len()
                    ),
                    html! {
                        <Button size={ButtonSize::Small} disabled={self.filter == name}
                            onclick={link.callback(move |_| Msg::FilterTarget(show.clone()))}>
                            {"Show them"}
                        </Button>
                    },
                )
            }
        };

        html! {
            <Toolbar label="Session target" band=true chrome=true>
                <span class={classes!(class::MUTED, sockel::class::TRUNCATE)} title={text.clone()}>{text}</span>
                <ToolbarSpacer />
                {action}
            </Toolbar>
        }
    }
}
