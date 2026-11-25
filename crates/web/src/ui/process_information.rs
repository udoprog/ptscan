use musli_web::web03::prelude::*;
use sockel::{
    Button, ButtonSize, Checkbox, Cluster, Dialog, DialogSize, EmptyState, TabItem, Table,
    TableCell, TableRow, Tabs,
};
use yew::prelude::*;

use crate::class;

#[derive(Properties, PartialEq)]
pub struct Props {
    /// The dialog is shown.
    pub open: bool,
    pub ws: ws::Handle,
    /// The connection is open.
    pub connected: bool,
    pub process: Option<api::ProcessInfo>,
    pub on_error: Callback<String>,
    pub on_close: Callback<()>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Tab {
    Modules,
    Threads,
    Memory,
}

#[derive(Clone, Copy)]
pub enum Flag {
    Readable,
    Writable,
    Executable,
    Modules,
    ThreadStacks,
    Other,
}

pub enum Msg {
    Refresh,
    Loaded(Result<ws::Packet<api::GetProcessInformation>, ws::Error>),
    Tab(Tab),
    Filter(Flag, bool),
}

pub struct ProcessInformation {
    info: Option<api::ProcessInformation>,
    filter: api::MemoryFilter,
    tab: Tab,
    request: ws::Request,
}

impl ProcessInformation {
    fn refresh(&mut self, ctx: &Context<Self>) {
        if !ctx.props().open || !ctx.props().connected || ctx.props().process.is_none() {
            self.request = ws::Request::default();
            self.info = None;
            return;
        }

        self.request = ctx
            .props()
            .ws
            .request()
            .body(api::ProcessInformationRequest {
                filter: self.filter.clone(),
            })
            .on_packet(ctx.link().callback(Msg::Loaded))
            .send();
    }
}

impl Component for ProcessInformation {
    type Message = Msg;
    type Properties = Props;

    fn create(ctx: &Context<Self>) -> Self {
        let mut this = Self {
            info: None,
            filter: api::MemoryFilter::default(),
            tab: Tab::Modules,
            request: ws::Request::default(),
        };

        this.refresh(ctx);
        this
    }

    fn changed(&mut self, ctx: &Context<Self>, old: &Self::Properties) -> bool {
        let props = ctx.props();

        if props.open != old.open
            || props.connected != old.connected
            || props.process != old.process
        {
            self.refresh(ctx);
        }

        true
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::Refresh => self.refresh(ctx),
            Msg::Loaded(result) => match result.and_then(|p| p.decode_response()) {
                Ok(info) => {
                    if let Some(error) = &info.error {
                        ctx.props()
                            .on_error
                            .emit(format!("Reading process information: {error}"));
                    }

                    self.info = Some(info);
                }
                Err(error) => ctx
                    .props()
                    .on_error
                    .emit(format!("Reading process information: {error}")),
            },
            Msg::Tab(tab) => self.tab = tab,
            Msg::Filter(flag, value) => {
                let filter = &mut self.filter;

                *match flag {
                    Flag::Readable => &mut filter.readable,
                    Flag::Writable => &mut filter.writable,
                    Flag::Executable => &mut filter.executable,
                    Flag::Modules => &mut filter.modules,
                    Flag::ThreadStacks => &mut filter.thread_stacks,
                    Flag::Other => &mut filter.other,
                } = value;

                self.refresh(ctx);
            }
        }

        true
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let props = ctx.props();
        let link = ctx.link();
        let close = props.on_close.reform(|_| ());

        let title = match &props.process {
            Some(process) => format!("Process information: {}", crate::app::describe(process)),
            None => String::from("Process information"),
        };

        let info = self.info.as_ref();
        let name = info.and_then(|i| i.name.as_deref()).unwrap_or("n/a");
        let file_name = info.and_then(|i| i.file_name.as_deref()).unwrap_or("n/a");

        let tab = |tab: Tab, count: Option<usize>| {
            let label = match count {
                Some(n) => format!("{} ({n})", tab.label()),
                None => tab.label().to_string(),
            };

            TabItem::new(tab.value(), label)
        };

        let tabs = vec![
            tab(Tab::Modules, info.map(|i| i.modules.len())),
            tab(Tab::Threads, info.map(|i| i.threads.len())),
            tab(Tab::Memory, info.map(|i| i.regions.len())),
        ];

        let on_tab = link.batch_callback(|value: AttrValue| Tab::from_value(&value).map(Msg::Tab));

        let body = match info {
            None if self.request.is_pending() => {
                html! { <EmptyState title="Loading..." compact=true /> }
            }
            None => html! { <EmptyState title="Not attached" compact=true /> },
            Some(info) => match self.tab {
                Tab::Modules => modules(&info.modules),
                Tab::Threads => threads(&info.threads),
                Tab::Memory => regions(&info.regions),
            },
        };

        let head_actions = html! {
            <Button size={ButtonSize::Small}
                busy={self.request.is_pending()}
                disabled={!props.connected || props.process.is_none()}
                title="Read the process information again"
                onclick={link.callback(|_| Msg::Refresh)}>{"Refresh"}</Button>
        };

        html! {
            <Dialog open={props.open} on_dismiss={close} title={title}
                size={DialogSize::WideTall} flush=true {head_actions}>
                <div class={class::DIALOG_BODY}>
                    <div class={class::DETAILS}>
                        <span class={class::MUTED}>{"Name"}</span>
                        <span>{name}</span>
                        <span class={class::MUTED}>{"File"}</span>
                        <span class={class::MONO}>{file_name}</span>
                    </div>
                    <Tabs class={class::TABS} label="Process information" {tabs}
                        value={AttrValue::from(self.tab.value())} on_change={on_tab} />
                    if self.tab == Tab::Memory {
                        {self.filters(ctx)}
                    }
                    <div class={class::SCROLL}>{body}</div>
                </div>
            </Dialog>
        }
    }
}

impl Tab {
    fn value(self) -> &'static str {
        match self {
            Tab::Modules => "modules",
            Tab::Threads => "threads",
            Tab::Memory => "memory",
        }
    }

    fn label(self) -> &'static str {
        match self {
            Tab::Modules => "Modules",
            Tab::Threads => "Threads",
            Tab::Memory => "Memory",
        }
    }

    fn from_value(value: &str) -> Option<Self> {
        match value {
            "modules" => Some(Tab::Modules),
            "threads" => Some(Tab::Threads),
            "memory" => Some(Tab::Memory),
            _ => None,
        }
    }
}

impl ProcessInformation {
    fn filters(&self, ctx: &Context<Self>) -> Html {
        let link = ctx.link();
        let busy = self.request.is_pending();
        let filter = &self.filter;

        let check = |flag: Flag, label: &str, checked: bool, disabled: bool, title: &str| {
            let title = (!title.is_empty()).then(|| AttrValue::from(title.to_string()));

            html! {
                <Checkbox {checked} {disabled} {title}
                    on_change={link.callback(move |value: bool| Msg::Filter(flag, value))}>
                    {label.to_string()}
                </Checkbox>
            }
        };

        let flags = busy || filter.other;

        html! {
            <Cluster class={class::FIELDS}>
                <span class={class::MUTED}>{"Only regions which are"}</span>
                {check(Flag::Readable, "Readable", filter.readable, flags, "")}
                {check(Flag::Writable, "Writable", filter.writable, flags, "")}
                {check(Flag::Executable, "Executable", filter.executable, flags, "")}
                {check(Flag::Modules, "Modules", filter.modules, flags, "Part of a module")}
                {check(Flag::ThreadStacks, "Thread stacks", filter.thread_stacks, flags, "Holding a thread's stack")}
                {check(Flag::Other, "Other", filter.other, busy,
                    "Neither accessible nor part of a module or thread stack; overrides the other filters")}
            </Cluster>
        }
    }
}

fn head(label: &'static str) -> Html {
    html! { <TableCell head=true>{label}</TableCell> }
}

fn hex(value: u64) -> Html {
    html! { <TableCell class={class::MONO}>{format!("{value:#x}")}</TableCell> }
}

fn modules(modules: &[api::ModuleInformation]) -> Html {
    if modules.is_empty() {
        return html! { <EmptyState title="No modules" compact=true /> };
    }

    let rows = modules
        .iter()
        .enumerate()
        .map(|(index, m)| {
            html! {
                <TableRow key={index}>
                    <TableCell tip={m.file_name.clone().map(AttrValue::from)}>{&m.name}</TableCell>
                    {hex(m.base)}
                    {hex(m.size)}
                </TableRow>
            }
        })
        .collect::<Html>();

    let head = html! { <>{head("Name")}{head("Base")}{head("Size")}</> };

    html! {
        <Table {head} columns={3} label="Modules">{rows}</Table>
    }
}

fn threads(threads: &[api::ThreadInformation]) -> Html {
    if threads.is_empty() {
        return html! { <EmptyState title="No threads" compact=true /> };
    }

    let rows = threads
        .iter()
        .map(|t| {
            let exit = t
                .stack_exit
                .map(|e| format!("{e:#x}"))
                .unwrap_or_else(|| String::from("n/a"));

            html! {
                <TableRow key={t.id}>
                    <TableCell numeric=true>{t.id}</TableCell>
                    {hex(t.stack_base)}
                    {hex(t.stack_size)}
                    <TableCell class={class::MONO}>{exit}</TableCell>
                </TableRow>
            }
        })
        .collect::<Html>();

    let head = html! {
        <>
            <TableCell head=true numeric=true>{"ID"}</TableCell>
            {head("Stack base")}
            {head("Stack size")}
            <TableCell head=true tip="Address of the thread's exit frame on its stack">{"Stack exit"}</TableCell>
        </>
    };

    html! {
        <Table {head} columns={4} label="Threads">{rows}</Table>
    }
}

fn regions(regions: &[api::MemoryRegion]) -> Html {
    if regions.is_empty() {
        return html! { <EmptyState title="No regions match the filter" compact=true /> };
    }

    let rows = regions
        .iter()
        .enumerate()
        .map(|(index, r)| {
            let kind = match (&r.module, r.thread_stack) {
                (Some(module), _) => format!("Module: {module}"),
                (None, Some(id)) => format!("Thread stack (id: {id})"),
                (None, None) => String::new(),
            };

            html! {
                <TableRow key={index}>
                    {hex(r.base)}
                    {hex(r.size)}
                    <TableCell>{&r.state}</TableCell>
                    <TableCell>{r.ty.as_deref().unwrap_or("")}</TableCell>
                    <TableCell>{r.protect.join(" | ")}</TableCell>
                    <TableCell>{kind}</TableCell>
                </TableRow>
            }
        })
        .collect::<Html>();

    let head = html! {
        <>
            {head("Base")}
            {head("Size")}
            {head("State")}
            {head("Type")}
            {head("Protection")}
            {head("Kind")}
        </>
    };

    html! {
        <Table {head} columns={6} label="Memory regions">{rows}</Table>
    }
}
