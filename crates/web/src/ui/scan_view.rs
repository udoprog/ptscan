use std::collections::BTreeSet;

use gloo_timers::callback::Interval;
use musli_web::web03::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::HtmlInputElement;
use yew::prelude::*;

use crate::ui::scan_result_dialog::ScanResultDialog;

const PAGE: u32 = 100;
const REFRESH_MS: u32 = 1000;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub ws: ws::Handle,
    /// The connection is open.
    pub open: bool,
    pub attached: bool,
    pub on_error: Callback<String>,
}

pub enum Msg {
    Filter(String),
    Value(String),
    Alignment(String),
    Suspend(bool),
    ModulesOnly(bool),
    Scan,
    Started(Result<ws::Packet<api::Scan>, ws::Error>),
    Cancel,
    Reset,
    Undo,
    Redo,
    Done(&'static str, Result<api::ScanState, ws::Error>),
    StateChanged(Result<ws::Packet<api::ScanStateChanged>, ws::Error>),
    Progressed(Result<ws::Packet<api::ScanProgressed>, ws::Error>),
    Refresh,
    Tick,
    Page(u64),
    Loaded(Result<ws::Packet<api::ScanResults>, ws::Error>),
    Select(u64, bool),
    SelectPage,
    Show(u64),
    CloseDialog,
    AddSelected,
    Added(Result<ws::Packet<api::AddScratch>, ws::Error>),
}

pub struct ScanView {
    filter: String,
    value: String,
    alignment: String,
    suspend: bool,
    modules_only: bool,
    filter_error: Option<String>,
    value_error: Option<String>,
    state: api::ScanState,
    progress: api::ScanProgress,
    offset: u64,
    total: u64,
    results: Vec<api::ScanResult>,
    page_error: Option<String>,
    /// Addresses of the selected results.
    selected: BTreeSet<u64>,
    dialog: Option<u64>,
    added: Option<String>,
    scan: ws::Request,
    action: ws::Request,
    state_request: ws::Request,
    page: ws::Request,
    adding: Vec<ws::Request>,
    _state_listener: ws::Listener,
    _progress_listener: ws::Listener,
    _timer: Interval,
}

impl ScanView {
    fn load_state(&mut self, ctx: &Context<Self>) {
        self.state_request = ctx
            .props()
            .ws
            .request()
            .body(api::GetScanStateRequest)
            .on_packet(
                ctx.link()
                    .callback(|r: Result<ws::Packet<api::GetScanState>, _>| {
                        Msg::Done("Reading scan state", r.and_then(|p| p.decode_response()))
                    }),
            )
            .send();
    }

    fn load_page(&mut self, ctx: &Context<Self>) {
        self.page = ctx
            .props()
            .ws
            .request()
            .body(api::ScanResultsRequest {
                offset: self.offset,
                limit: PAGE,
            })
            .on_packet(ctx.link().callback(Msg::Loaded))
            .send();
    }

    fn set_state(&mut self, ctx: &Context<Self>, state: api::ScanState) {
        let changed = state != self.state;
        self.state = state;

        if changed {
            if self.state.generations == 0 {
                self.selected.clear();
                self.dialog = None;
            }

            self.offset = self.offset.min(last_page(self.state.results));
            self.reload(ctx);
        }
    }

    fn reload(&mut self, ctx: &Context<Self>) {
        if self.state.results == 0 || self.state.running {
            self.results.clear();
            self.total = self.state.results;
            self.page_error = None;
            self.page = ws::Request::default();
        } else {
            self.load_page(ctx);
        }
    }

    fn scan_request(&self) -> Result<api::ScanRequest, String> {
        let alignment = match self.alignment.trim() {
            "" => None,
            text => match text.parse::<u32>() {
                Ok(n) if n > 0 => Some(n),
                _ => return Err(format!("Alignment `{text}` is not a positive number")),
            },
        };

        Ok(api::ScanRequest {
            filter: self.filter.clone(),
            value: self.value.clone(),
            alignment,
            suspend: self.suspend,
            modules_only: self.modules_only,
        })
    }

    fn add_to_scratch(&mut self, ctx: &Context<Self>, pointer: String, value_type: String) {
        let request = ctx
            .props()
            .ws
            .request()
            .body(api::AddScratchRequest {
                pointer,
                value_type,
            })
            .on_packet(ctx.link().callback(Msg::Added))
            .send();

        self.adding.retain(|r| r.is_pending());
        self.adding.push(request);
    }
}

impl Component for ScanView {
    type Message = Msg;
    type Properties = Props;

    fn create(ctx: &Context<Self>) -> Self {
        let ws = &ctx.props().ws;
        let link = ctx.link().clone();

        let mut this = Self {
            filter: String::new(),
            value: String::new(),
            alignment: String::new(),
            suspend: false,
            modules_only: false,
            filter_error: None,
            value_error: None,
            state: api::ScanState::default(),
            progress: api::ScanProgress::default(),
            offset: 0,
            total: 0,
            results: Vec::new(),
            page_error: None,
            selected: BTreeSet::new(),
            dialog: None,
            added: None,
            scan: ws::Request::default(),
            action: ws::Request::default(),
            state_request: ws::Request::default(),
            page: ws::Request::default(),
            adding: Vec::new(),
            _state_listener: ws.on_broadcast(ctx.link().callback(Msg::StateChanged)),
            _progress_listener: ws.on_broadcast(ctx.link().callback(Msg::Progressed)),
            _timer: Interval::new(REFRESH_MS, move || link.send_message(Msg::Tick)),
        };

        if ctx.props().open {
            this.load_state(ctx);
        }

        this
    }

    fn changed(&mut self, ctx: &Context<Self>, old: &Self::Properties) -> bool {
        let props = ctx.props();

        if props.open && (!old.open || props.attached != old.attached) {
            self.load_state(ctx);
        }

        true
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        let on_error = ctx.props().on_error.clone();
        let ws = ctx.props().ws.clone();

        match msg {
            Msg::Filter(text) => {
                self.filter = text;
                self.filter_error = None;
            }
            Msg::Value(text) => {
                self.value = text;
                self.value_error = None;
            }
            Msg::Alignment(text) => self.alignment = text,
            Msg::Suspend(on) => self.suspend = on,
            Msg::ModulesOnly(on) => self.modules_only = on,
            Msg::Scan => {
                let request = match self.scan_request() {
                    Ok(request) => request,
                    Err(error) => {
                        on_error.emit(error);
                        return true;
                    }
                };

                self.scan = ws
                    .request()
                    .body(request)
                    .on_packet(ctx.link().callback(Msg::Started))
                    .send();
            }
            Msg::Started(result) => match result.and_then(|p| p.decode_response()) {
                Ok(response) => {
                    self.filter_error = response.filter_error;
                    self.value_error = response.value_error;

                    if let Some(error) = response.error {
                        on_error.emit(format!("Scanning: {error}"));
                    }
                }
                Err(error) => on_error.emit(format!("Scanning: {error}")),
            },
            Msg::Cancel => {
                self.action =
                    ws.request()
                        .body(api::CancelScanRequest)
                        .on_packet(ctx.link().callback(
                            |r: Result<ws::Packet<api::CancelScan>, _>| {
                                Msg::Done("Cancelling", r.and_then(|p| p.decode_response()))
                            },
                        ))
                        .send();
            }
            Msg::Reset => {
                self.action =
                    ws.request()
                        .body(api::ResetScanRequest)
                        .on_packet(ctx.link().callback(
                            |r: Result<ws::Packet<api::ResetScan>, _>| {
                                Msg::Done("Resetting", r.and_then(|p| p.decode_response()))
                            },
                        ))
                        .send();
            }
            Msg::Undo => {
                self.action =
                    ws.request()
                        .body(api::UndoScanRequest)
                        .on_packet(ctx.link().callback(
                            |r: Result<ws::Packet<api::UndoScan>, _>| {
                                Msg::Done("Undoing", r.and_then(|p| p.decode_response()))
                            },
                        ))
                        .send();
            }
            Msg::Redo => {
                self.action =
                    ws.request()
                        .body(api::RedoScanRequest)
                        .on_packet(ctx.link().callback(
                            |r: Result<ws::Packet<api::RedoScan>, _>| {
                                Msg::Done("Redoing", r.and_then(|p| p.decode_response()))
                            },
                        ))
                        .send();
            }
            Msg::Done(what, result) => match result {
                Ok(state) => self.set_state(ctx, state),
                Err(error) => on_error.emit(format!("{what}: {error}")),
            },
            Msg::StateChanged(result) => match result.and_then(|p| p.decode_event()) {
                Ok(state) => self.set_state(ctx, state),
                Err(error) => on_error.emit(format!("Scan state update: {error}")),
            },
            Msg::Progressed(result) => match result.and_then(|p| p.decode_event()) {
                Ok(progress) => self.progress = progress,
                Err(error) => on_error.emit(format!("Scan progress: {error}")),
            },
            Msg::Refresh => self.reload(ctx),
            Msg::Tick => {
                let props = ctx.props();

                if !props.open
                    || !props.attached
                    || self.state.running
                    || self.state.results == 0
                    || self.page.is_pending()
                {
                    return false;
                }

                self.load_page(ctx);
                return false;
            }
            Msg::Page(offset) => {
                self.offset = offset;
                self.reload(ctx);
            }
            Msg::Loaded(result) => match result.and_then(|p| p.decode_response()) {
                Ok(page) if page.offset == self.offset => {
                    self.total = page.total;
                    self.results = page.results;
                    self.page_error = page.error;
                }
                Ok(_) => return false,
                Err(error) => on_error.emit(format!("Reading results: {error}")),
            },
            Msg::Select(address, extend) => {
                if !extend {
                    self.selected.clear();
                    self.selected.insert(address);
                } else if !self.selected.remove(&address) {
                    self.selected.insert(address);
                }
            }
            Msg::SelectPage => {
                let all = self
                    .results
                    .iter()
                    .all(|r| self.selected.contains(&r.address));

                for result in &self.results {
                    if all {
                        self.selected.remove(&result.address);
                    } else {
                        self.selected.insert(result.address);
                    }
                }
            }
            Msg::Show(address) => self.dialog = Some(address),
            Msg::CloseDialog => self.dialog = None,
            Msg::AddSelected => {
                let picked = self
                    .results
                    .iter()
                    .filter(|r| self.selected.contains(&r.address))
                    .map(|r| (r.pointer.clone(), r.value_type.clone()))
                    .collect::<Vec<_>>();

                for (pointer, value_type) in picked {
                    self.add_to_scratch(ctx, pointer, value_type);
                }
            }
            Msg::Added(result) => match result.and_then(|p| p.decode_response()) {
                Ok(response) => {
                    let errors = [
                        response.pointer_error.map(|e| format!("pointer: {e}")),
                        response.value_type_error.map(|e| format!("type: {e}")),
                        response.error,
                    ];

                    for error in errors.into_iter().flatten() {
                        on_error.emit(format!("Adding to scratch: {error}"));
                    }

                    if let Some(entry) = response.entry {
                        self.added = Some(format!("Added {} to the scratch list", entry.pointer));
                    }
                }
                Err(error) => on_error.emit(format!("Adding to scratch: {error}")),
            },
        }

        true
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let props = ctx.props();
        let link = ctx.link();
        let ready = props.open && props.attached;
        let running = self.state.running;
        let busy = self.action.is_pending() || self.scan.is_pending();

        let onsubmit = link.callback(|e: SubmitEvent| {
            e.prevent_default();
            Msg::Scan
        });

        let scan_label = if self.state.generations == 0 {
            "Scan"
        } else {
            "Rescan"
        };

        let undo_title = match &self.state.last {
            Some(last) if last.in_place && !self.state.can_undo() => {
                "The last scan replaced its results in place since they were too large to copy, so it cannot be undone"
            }
            Some(last) if last.in_place => {
                "The last scan replaced its results in place since they were too large to copy; undo returns to the scan before it"
            }
            _ => "Return to the previous scan",
        };

        let page_end = (self.offset + self.results.len() as u64).min(self.total);
        let can_prev = self.offset > 0;
        let can_next = page_end < self.total;
        let prev = self.offset.saturating_sub(PAGE as u64);
        let next = self.offset + PAGE as u64;
        let last = last_page(self.total);

        let selected_here = self
            .results
            .iter()
            .filter(|r| self.selected.contains(&r.address))
            .count();
        let all_here = !self.results.is_empty() && selected_here == self.results.len();

        let rows = self
            .results
            .iter()
            .enumerate()
            .map(|(index, result)| {
                let address = result.address;
                let changed = result.current != result.last;
                let class = classes!(self.selected.contains(&address).then_some("selected"));
                let onclick = link.callback(move |e: MouseEvent| {
                    Msg::Select(address, e.ctrl_key() || e.meta_key() || e.shift_key())
                });

                html! {
                    <tr key={self.offset + index as u64} {class} {onclick}
                        ondblclick={link.callback(move |_| Msg::Show(address))}>
                        <td class="num muted">{self.offset + index as u64}</td>
                        <td class="mono">{&result.pointer}</td>
                        <td class="mono">{&result.value_type}</td>
                        <td class="mono">{&result.last}</td>
                        <td class={classes!("mono", changed.then_some("changed"))}>{&result.current}</td>
                    </tr>
                }
            })
            .collect::<Html>();

        let dialog = self.dialog.and_then(|address| {
            let result = self.results.iter().find(|r| r.address == address)?;

            Some(html! {
                <ScanResultDialog
                    ws={props.ws.clone()}
                    result={result.clone()}
                    on_error={props.on_error.clone()}
                    on_close={link.callback(|()| Msg::CloseDialog)}
                />
            })
        });

        html! {
            <column class="panel fill" style="gap: 0">
                <form class="scan-form" {onsubmit}>
                    <row class="toolbar center">
                        <span class="title">{"Scan"}</span>
                        <div class="fill" />
                        <button type="submit" class="primary" disabled={!ready || running || busy}>
                            {scan_label}
                        </button>
                        if running {
                            <button type="button" onclick={link.callback(|_| Msg::Cancel)}>{"Cancel"}</button>
                        }
                        <button type="button" title={undo_title}
                            disabled={!ready || running || busy || !self.state.can_undo()}
                            onclick={link.callback(|_| Msg::Undo)}>{"Undo"}</button>
                        <button type="button" title="Restore the scan that was undone"
                            disabled={!ready || running || busy || !self.state.can_redo()}
                            onclick={link.callback(|_| Msg::Redo)}>{"Redo"}</button>
                        <button type="button" title="Discard every scan"
                            disabled={!ready || busy || self.state.generations == 0}
                            onclick={link.callback(|_| Msg::Reset)}>{"Reset"}</button>
                    </row>
                    <column class="fields">
                        {field("Filter", "value == 42u32", &self.filter, &self.filter_error,
                            link.callback(|e: InputEvent| Msg::Filter(input_value(&e))))}
                        {field("Value", "the value itself", &self.value, &self.value_error,
                            link.callback(|e: InputEvent| Msg::Value(input_value(&e))))}
                        <row class="center options">
                            <label class="field-label" for="scan-alignment">{"Alignment"}</label>
                            <input id="scan-alignment" class="narrow" placeholder="auto"
                                value={self.alignment.clone()}
                                oninput={link.callback(|e: InputEvent| Msg::Alignment(input_value(&e)))} />
                            <label class="check">
                                <input type="checkbox" checked={self.suspend}
                                    onchange={link.callback(|e: Event| Msg::Suspend(input_checked(&e)))} />
                                {"Suspend while scanning"}
                            </label>
                            <label class="check" title="Applies to the first scan">
                                <input type="checkbox" checked={self.modules_only}
                                    onchange={link.callback(|e: Event| Msg::ModulesOnly(input_checked(&e)))} />
                                {"Modules only"}
                            </label>
                        </row>
                    </column>
                </form>
                {self.status()}
                <row class="toolbar center results-bar">
                    <span class="muted">{self.range_label(page_end)}</span>
                    <button disabled={!can_prev} onclick={link.callback(|_| Msg::Page(0))}>{"First"}</button>
                    <button disabled={!can_prev} onclick={link.callback(move |_| Msg::Page(prev))}>{"Prev"}</button>
                    <button disabled={!can_next} onclick={link.callback(move |_| Msg::Page(next))}>{"Next"}</button>
                    <button disabled={!can_next} onclick={link.callback(move |_| Msg::Page(last))}>{"Last"}</button>
                    <button disabled={!ready || running || self.state.results == 0}
                        onclick={link.callback(|_| Msg::Refresh)}>{"Refresh"}</button>

                    <span class="muted added" title={self.added.clone().unwrap_or_default()}>
                        {self.added.clone().unwrap_or_default()}
                    </span>
                    <button disabled={self.selected.len() != 1}
                        onclick={self.selected.first().copied().map(|a| link.callback(move |_| Msg::Show(a))).unwrap_or_default()}>
                        {"Show"}
                    </button>
                    <button disabled={selected_here == 0 || !ready}
                        onclick={link.callback(|_| Msg::AddSelected)}>
                        {match selected_here {
                            0 | 1 => String::from("Add to scratch"),
                            n => format!("Add {n} to scratch"),
                        }}
                    </button>
                </row>
                <div class="list fill">
                    if self.results.is_empty() {
                        <div class="empty muted">{self.empty_label(ready)}</div>
                    } else {
                        <table>
                            <thead><tr>
                                <th class="num">
                                    <input type="checkbox" title="Select the results on this page"
                                        checked={all_here}
                                        onchange={link.callback(|_| Msg::SelectPage)} />
                                </th>
                                <th>{"Pointer"}</th><th>{"Type"}</th>
                                <th>{"Last"}</th><th>{"Current"}</th>
                            </tr></thead>
                            <tbody>{rows}</tbody>
                        </table>
                    }
                </div>
                {dialog}
            </column>
        }
    }
}

impl ScanView {
    fn status(&self) -> Html {
        if self.state.running {
            let percentage = self.progress.percentage.min(100);

            return html! {
                <row class="scan-status center">
                    <progress max="100" value={percentage.to_string()} />
                    <span>{format!("{percentage}%, {} results so far", self.progress.results)}</span>
                </row>
            };
        }

        let Some(last) = &self.state.last else {
            return html! {};
        };

        let mut text = match &last.error {
            Some(error) => format!("Scan failed: {error}"),
            None => format!(
                "Found {} results in {}",
                last.results,
                duration(last.duration_ms)
            ),
        };

        if last.cancelled {
            text.push_str("; cancelled, partial results kept");
        }

        if last.in_place {
            text.push_str(
                "; rescanned in place since the results were too large to copy, so this scan cannot be undone",
            );
        }

        let class = classes!(
            "scan-status",
            last.error.is_some().then_some("bad"),
            (last.cancelled || last.in_place).then_some("warn"),
        );

        html! { <row {class}>{text}</row> }
    }

    fn range_label(&self, page_end: u64) -> String {
        if self.total == 0 {
            return String::from("No results");
        }

        format!(
            "{}-{} of {}",
            self.offset + 1,
            page_end.max(self.offset + 1),
            self.total
        )
    }

    fn empty_label(&self, ready: bool) -> String {
        if let Some(error) = &self.page_error {
            return error.clone();
        }

        if !ready {
            return String::from("Attach to a process to scan it");
        }

        if self.state.running {
            return String::from("Scanning...");
        }

        if self.state.generations == 0 {
            return String::from("Enter a filter and scan");
        }

        String::from("No results")
    }
}

fn field(
    label: &'static str,
    placeholder: &'static str,
    value: &str,
    error: &Option<String>,
    oninput: Callback<InputEvent>,
) -> Html {
    let id = format!("scan-{}", label.to_lowercase());

    html! {
        <row class="center">
            <label class="field-label" for={id.clone()}>{label}</label>
            <column class="fill" style="gap: 2px">
                <input {id} class={classes!("mono", error.is_some().then_some("invalid"))}
                    {placeholder} value={value.to_owned()} {oninput} />
                if let Some(error) = error {
                    <span class="field-error">{error}</span>
                }
            </column>
        </row>
    }
}

fn last_page(total: u64) -> u64 {
    total.saturating_sub(1) / PAGE as u64 * PAGE as u64
}

fn duration(ms: u64) -> String {
    if ms < 1000 {
        format!("{ms} ms")
    } else {
        format!("{:.1} s", ms as f64 / 1000.0)
    }
}

fn input_value(e: &InputEvent) -> String {
    e.target()
        .and_then(|t| t.dyn_into::<HtmlInputElement>().ok())
        .map(|input| input.value())
        .unwrap_or_default()
}

fn input_checked(e: &Event) -> bool {
    e.target()
        .and_then(|t| t.dyn_into::<HtmlInputElement>().ok())
        .is_some_and(|input| input.checked())
}
