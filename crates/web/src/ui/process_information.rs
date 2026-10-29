use musli_web::web03::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::HtmlInputElement;
use yew::prelude::*;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub ws: ws::Handle,
    /// The connection is open.
    pub open: bool,
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
        if !ctx.props().open || ctx.props().process.is_none() {
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

        if props.open != old.open || props.process != old.process {
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
        let close = props.on_close.reform(|_: MouseEvent| ());
        let keep_open = Callback::from(|e: MouseEvent| e.stop_propagation());

        let title = match &props.process {
            Some(process) => format!("Process information: {}", crate::app::describe(process)),
            None => String::from("Process information"),
        };

        let info = self.info.as_ref();
        let name = info.and_then(|i| i.name.as_deref()).unwrap_or("n/a");
        let file_name = info.and_then(|i| i.file_name.as_deref()).unwrap_or("n/a");

        let tab = |tab: Tab, label: &str, count: Option<usize>| {
            let class = classes!("tab", (self.tab == tab).then_some("active"));
            let label = match count {
                Some(n) => format!("{label} ({n})"),
                None => label.to_string(),
            };

            html! {
                <button {class} onclick={link.callback(move |_| Msg::Tab(tab))}>{label}</button>
            }
        };

        let body = match info {
            None if self.request.is_pending() => {
                html! { <div class="empty muted">{"Loading..."}</div> }
            }
            None => html! { <div class="empty muted">{"Not attached"}</div> },
            Some(info) => match self.tab {
                Tab::Modules => modules(&info.modules),
                Tab::Threads => threads(&info.threads),
                Tab::Memory => regions(&info.regions),
            },
        };

        html! {
            <div class="backdrop" onclick={close.clone()}>
                <column class="panel dialog wide" style="gap: 0" onclick={keep_open}>
                    <row class="toolbar center">
                        <span class="title">{title}</span>
                        <div class="fill" />
                        if self.request.is_pending() {
                            <span class="muted">{"Loading..."}</span>
                        }
                        <button disabled={!props.open || props.process.is_none() || self.request.is_pending()}
                            title="Read the process information again"
                            onclick={link.callback(|_| Msg::Refresh)}>{"Refresh"}</button>
                        <button onclick={close}>{"Close"}</button>
                    </row>
                    <div class="details">
                        <span class="field-label">{"Name"}</span>
                        <span>{name}</span>
                        <span class="field-label">{"File"}</span>
                        <span class="mono">{file_name}</span>
                    </div>
                    <row class="toolbar center tabs">
                        {tab(Tab::Modules, "Modules", info.map(|i| i.modules.len()))}
                        {tab(Tab::Threads, "Threads", info.map(|i| i.threads.len()))}
                        {tab(Tab::Memory, "Memory", info.map(|i| i.regions.len()))}
                    </row>
                    if self.tab == Tab::Memory {
                        {self.filters(ctx)}
                    }
                    <div class="list fill">{body}</div>
                </column>
            </div>
        }
    }
}

impl ProcessInformation {
    fn filters(&self, ctx: &Context<Self>) -> Html {
        let link = ctx.link();
        let busy = self.request.is_pending();
        let filter = &self.filter;

        let check = |flag: Flag, label: &str, checked: bool, disabled: bool, title: &str| {
            html! {
                <label class="check" title={title.to_string()}>
                    <input type="checkbox" {checked} {disabled}
                        onchange={link.callback(move |e: Event| Msg::Filter(flag, input_checked(&e)))} />
                    {label}
                </label>
            }
        };

        let flags = busy || filter.other;

        html! {
            <row class="toolbar center options">
                <span class="muted">{"Only regions which are"}</span>
                {check(Flag::Readable, "Readable", filter.readable, flags, "")}
                {check(Flag::Writable, "Writable", filter.writable, flags, "")}
                {check(Flag::Executable, "Executable", filter.executable, flags, "")}
                {check(Flag::Modules, "Modules", filter.modules, flags, "Part of a module")}
                {check(Flag::ThreadStacks, "Thread stacks", filter.thread_stacks, flags, "Holding a thread's stack")}
                {check(Flag::Other, "Other", filter.other, busy,
                    "Neither accessible nor part of a module or thread stack; overrides the other filters")}
            </row>
        }
    }
}

fn modules(modules: &[api::ModuleInformation]) -> Html {
    if modules.is_empty() {
        return html! { <div class="empty muted">{"No modules"}</div> };
    }

    let rows = modules
        .iter()
        .enumerate()
        .map(|(index, m)| {
            html! {
                <tr key={index}>
                    <td title={m.file_name.clone().unwrap_or_default()}>{&m.name}</td>
                    <td class="mono">{format!("{:#x}", m.base)}</td>
                    <td class="mono">{format!("{:#x}", m.size)}</td>
                </tr>
            }
        })
        .collect::<Html>();

    html! {
        <table>
            <thead><tr><th>{"Name"}</th><th>{"Base"}</th><th>{"Size"}</th></tr></thead>
            <tbody>{rows}</tbody>
        </table>
    }
}

fn threads(threads: &[api::ThreadInformation]) -> Html {
    if threads.is_empty() {
        return html! { <div class="empty muted">{"No threads"}</div> };
    }

    let rows = threads
        .iter()
        .map(|t| {
            let exit = t
                .stack_exit
                .map(|e| format!("{e:#x}"))
                .unwrap_or_else(|| String::from("n/a"));

            html! {
                <tr key={t.id}>
                    <td class="num">{t.id}</td>
                    <td class="mono">{format!("{:#x}", t.stack_base)}</td>
                    <td class="mono">{format!("{:#x}", t.stack_size)}</td>
                    <td class="mono">{exit}</td>
                </tr>
            }
        })
        .collect::<Html>();

    html! {
        <table>
            <thead><tr>
                <th class="num">{"ID"}</th>
                <th>{"Stack base"}</th>
                <th>{"Stack size"}</th>
                <th title="Address of the thread's exit frame on its stack">{"Stack exit"}</th>
            </tr></thead>
            <tbody>{rows}</tbody>
        </table>
    }
}

fn regions(regions: &[api::MemoryRegion]) -> Html {
    if regions.is_empty() {
        return html! { <div class="empty muted">{"No regions match the filter"}</div> };
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
                <tr key={index}>
                    <td class="mono">{format!("{:#x}", r.base)}</td>
                    <td class="mono">{format!("{:#x}", r.size)}</td>
                    <td>{&r.state}</td>
                    <td>{r.ty.as_deref().unwrap_or("")}</td>
                    <td>{r.protect.join(" | ")}</td>
                    <td>{kind}</td>
                </tr>
            }
        })
        .collect::<Html>();

    html! {
        <table>
            <thead><tr>
                <th>{"Base"}</th>
                <th>{"Size"}</th>
                <th>{"State"}</th>
                <th>{"Type"}</th>
                <th>{"Protection"}</th>
                <th>{"Kind"}</th>
            </tr></thead>
            <tbody>{rows}</tbody>
        </table>
    }
}

fn input_checked(e: &Event) -> bool {
    e.target()
        .and_then(|t| t.dyn_into::<HtmlInputElement>().ok())
        .is_some_and(|input| input.checked())
}
