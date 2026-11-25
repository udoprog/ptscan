use std::collections::BTreeSet;

use musli_web::web03::prelude::*;
use sockel::{
    Button, ButtonSize, ButtonVariant, Card, Checkbox, Cluster, EmptyState, Gap, Inset, Notice,
    Table, TableCell, Tone, Toolbar, ToolbarSpacer,
};
use yew::prelude::*;

use crate::class;

const PAGE: u32 = 100;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub ws: ws::Handle,
    /// The connection is open.
    pub open: bool,
    pub attached: bool,
    pub on_error: Callback<String>,
}

pub enum Msg {
    State(&'static str, Result<api::PointerScanState, ws::Error>),
    StateChanged(Result<ws::Packet<api::PointerScanStateChanged>, ws::Error>),
    Progressed(Result<ws::Packet<api::PointerScanProgressed>, ws::Error>),
    Cancel,
    Page(u64),
    Loaded(Result<ws::Packet<api::PointerScanResults>, ws::Error>),
    Select(u64, bool),
    SelectPage,
    AddSelected,
    Added(Result<ws::Packet<api::AddScratch>, ws::Error>),
}

pub struct PointerScanView {
    state: api::PointerScanState,
    progress: api::PointerScanProgress,
    offset: u64,
    total: u64,
    paths: Vec<String>,
    value_type: Option<String>,
    /// Indexes of the selected paths.
    selected: BTreeSet<u64>,
    added: Option<String>,
    state_request: ws::Request,
    cancel: ws::Request,
    page: ws::Request,
    adding: Vec<ws::Request>,
    _state_listener: ws::Listener,
    _progress_listener: ws::Listener,
}

impl PointerScanView {
    fn load_state(&mut self, ctx: &Context<Self>) {
        self.state_request =
            ctx.props()
                .ws
                .request()
                .body(api::GetPointerScanStateRequest)
                .on_packet(ctx.link().callback(
                    |r: Result<ws::Packet<api::GetPointerScanState>, _>| {
                        Msg::State(
                            "Reading pointer scan state",
                            r.and_then(|p| p.decode_response()),
                        )
                    },
                ))
                .send();
    }

    fn set_state(&mut self, ctx: &Context<Self>, state: api::PointerScanState) {
        if state == self.state {
            return;
        }

        self.state = state;
        self.selected.clear();
        self.offset = self.offset.min(last_page(self.state.results));
        self.reload(ctx);
    }

    fn reload(&mut self, ctx: &Context<Self>) {
        if self.state.results == 0 || self.state.running {
            self.paths.clear();
            self.total = self.state.results;
            self.page = ws::Request::default();
            return;
        }

        self.page = ctx
            .props()
            .ws
            .request()
            .body(api::PointerScanResultsRequest {
                offset: self.offset,
                limit: PAGE,
            })
            .on_packet(ctx.link().callback(Msg::Loaded))
            .send();
    }

    fn page_indexes(&self) -> std::ops::Range<u64> {
        self.offset..self.offset + self.paths.len() as u64
    }
}

impl Component for PointerScanView {
    type Message = Msg;
    type Properties = Props;

    fn create(ctx: &Context<Self>) -> Self {
        let ws = &ctx.props().ws;

        let mut this = Self {
            state: api::PointerScanState::default(),
            progress: api::PointerScanProgress::default(),
            offset: 0,
            total: 0,
            paths: Vec::new(),
            value_type: None,
            selected: BTreeSet::new(),
            added: None,
            state_request: ws::Request::default(),
            cancel: ws::Request::default(),
            page: ws::Request::default(),
            adding: Vec::new(),
            _state_listener: ws.on_broadcast(ctx.link().callback(Msg::StateChanged)),
            _progress_listener: ws.on_broadcast(ctx.link().callback(Msg::Progressed)),
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

        match msg {
            Msg::State(what, result) => match result {
                Ok(state) => self.set_state(ctx, state),
                Err(error) => on_error.emit(format!("{what}: {error}")),
            },
            Msg::StateChanged(result) => match result.and_then(|p| p.decode_event()) {
                Ok(state) => {
                    if state.running {
                        self.added = None;
                    }

                    self.set_state(ctx, state);
                }
                Err(error) => on_error.emit(format!("Pointer scan state update: {error}")),
            },
            Msg::Progressed(result) => match result.and_then(|p| p.decode_event()) {
                Ok(progress) => self.progress = progress,
                Err(error) => on_error.emit(format!("Pointer scan progress: {error}")),
            },
            Msg::Cancel => {
                self.cancel = ctx
                    .props()
                    .ws
                    .request()
                    .body(api::CancelPointerScanRequest)
                    .on_packet(ctx.link().callback(
                        |r: Result<ws::Packet<api::CancelPointerScan>, _>| {
                            Msg::State(
                                "Cancelling pointer scan",
                                r.and_then(|p| p.decode_response()),
                            )
                        },
                    ))
                    .send();
            }
            Msg::Page(offset) => {
                self.offset = offset;
                self.selected.clear();
                self.reload(ctx);
            }
            Msg::Loaded(result) => match result.and_then(|p| p.decode_response()) {
                Ok(page) if page.offset == self.offset => {
                    self.total = page.total;
                    self.paths = page.results;
                    self.value_type = page.value_type;
                }
                Ok(_) => return false,
                Err(error) => on_error.emit(format!("Reading pointer paths: {error}")),
            },
            Msg::Select(index, extend) => {
                if !extend {
                    self.selected.clear();
                    self.selected.insert(index);
                } else if !self.selected.remove(&index) {
                    self.selected.insert(index);
                }
            }
            Msg::SelectPage => {
                if self.selected.len() == self.paths.len() {
                    self.selected.clear();
                } else {
                    self.selected = self.page_indexes().collect();
                }
            }
            Msg::AddSelected => {
                let Some(value_type) = self.value_type.clone() else {
                    return false;
                };

                let picked = self
                    .page_indexes()
                    .zip(&self.paths)
                    .filter(|(index, _)| self.selected.contains(index))
                    .map(|(_, path)| path.clone())
                    .collect::<Vec<_>>();

                self.adding.retain(|r| r.is_pending());

                for pointer in picked {
                    let request = ctx
                        .props()
                        .ws
                        .request()
                        .body(api::AddScratchRequest {
                            pointer,
                            value_type: value_type.clone(),
                        })
                        .on_packet(ctx.link().callback(Msg::Added))
                        .send();

                    self.adding.push(request);
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
                        on_error.emit(format!("Adding path to scratch: {error}"));
                    }

                    if let Some(entry) = response.entry {
                        self.added = Some(format!("Added {} to the scratch list", entry.pointer));
                    }
                }
                Err(error) => on_error.emit(format!("Adding path to scratch: {error}")),
            },
        }

        true
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let props = ctx.props();
        let link = ctx.link();
        let running = self.state.running;

        let page_end = self.offset + self.paths.len() as u64;
        let can_prev = self.offset > 0;
        let can_next = page_end < self.total;
        let prev = self.offset.saturating_sub(PAGE as u64);
        let next = self.offset + PAGE as u64;
        let last = last_page(self.total);

        let rows = self
            .page_indexes()
            .zip(&self.paths)
            .map(|(index, path)| {
                let class = classes!(
                    sockel::class::TABLE_ROW,
                    class::ROW,
                    self.selected.contains(&index).then_some(class::SELECTED),
                );
                let onclick = link.callback(move |e: MouseEvent| {
                    Msg::Select(index, e.ctrl_key() || e.meta_key() || e.shift_key())
                });

                html! {
                    <tr key={index} {class} {onclick}>
                        <TableCell numeric=true class={class::MUTED}>{index}</TableCell>
                        <TableCell class={class::MONO}>{path}</TableCell>
                    </tr>
                }
            })
            .collect::<Html>();

        let all = !self.paths.is_empty() && self.selected.len() == self.paths.len();
        let can_add = !self.selected.is_empty() && props.open && self.value_type.is_some();

        let head = html! {
            <>
                <TableCell head=true numeric=true>
                    <Checkbox title="Select every path on this page"
                        checked={all} on_change={link.callback(|_| Msg::SelectPage)}>{"#"}</Checkbox>
                </TableCell>
                <TableCell head=true>{"Path"}</TableCell>
            </>
        };

        let empty = self
            .paths
            .is_empty()
            .then(|| html! { <EmptyState title={self.empty_label()} compact=true /> });

        let added = self.added.clone().unwrap_or_default();

        html! {
            <Card inset={Inset::Flush} class={class::PANE}>
                <Toolbar label="Pointer scan" band=true chrome=true>
                    <span class={class::TITLE}>{"Pointer scan"}</span>
                    <span class={class::MUTED}>{self.target()}</span>
                    <ToolbarSpacer />
                    if running {
                        <Button size={ButtonSize::Small} busy={self.cancel.is_pending()}
                            onclick={link.callback(|_| Msg::Cancel)}>{"Cancel"}</Button>
                    }
                </Toolbar>
                {self.status()}
                <Toolbar label="Pointer scan results" band=true chrome=true>
                    <span class={classes!(class::MUTED, sockel::class::NUMERIC)}>{self.range_label(page_end)}</span>
                    <Cluster gap={Gap::Hair} nowrap=true>
                        <Button size={ButtonSize::Small} disabled={!can_prev} onclick={link.callback(|_| Msg::Page(0))}>{"First"}</Button>
                        <Button size={ButtonSize::Small} disabled={!can_prev} onclick={link.callback(move |_| Msg::Page(prev))}>{"Prev"}</Button>
                        <Button size={ButtonSize::Small} disabled={!can_next} onclick={link.callback(move |_| Msg::Page(next))}>{"Next"}</Button>
                        <Button size={ButtonSize::Small} disabled={!can_next} onclick={link.callback(move |_| Msg::Page(last))}>{"Last"}</Button>
                    </Cluster>
                    <span class={classes!(class::MUTED, class::ADDED, sockel::class::TRUNCATE)} title={added.clone()}>
                        {added}
                    </span>
                    <Button size={ButtonSize::Small} variant={ButtonVariant::Primary} disabled={!can_add}
                        title="Add the selected paths to the scratch list"
                        onclick={link.callback(|_| Msg::AddSelected)}>
                        {match self.selected.len() {
                            0 | 1 => String::from("Add to scratch"),
                            n => format!("Add {n} to scratch"),
                        }}
                    </Button>
                </Toolbar>
                <div class={class::SCROLL}>
                    <Table {head} columns={2} {empty} label="Pointer paths">{rows}</Table>
                </div>
            </Card>
        }
    }
}

impl PointerScanView {
    fn target(&self) -> String {
        match (self.state.entry, &self.state.value_type) {
            (Some(id), Some(value_type)) => format!("scratch entry {id} ({value_type})"),
            (Some(id), None) => format!("scratch entry {id}"),
            _ => String::new(),
        }
    }

    fn status(&self) -> Html {
        if self.state.running {
            let progress = &self.progress;

            let (bar, text) = match progress.stage {
                api::PointerScanStage::Initial => {
                    let percentage = progress.percentage.min(100);
                    (
                        html! { <progress class={class::PROGRESS} max="100" value={percentage.to_string()} /> },
                        format!(
                            "Scanning memory, {percentage}%, {} pointers found",
                            progress.results
                        ),
                    )
                }
                api::PointerScanStage::Paths => (
                    html! { <progress class={class::PROGRESS} /> },
                    format!(
                        "Following pointers, {} queued, {} paths so far",
                        progress.remaining, progress.results
                    ),
                ),
                api::PointerScanStage::Backreferences => (
                    html! { <progress class={class::PROGRESS} /> },
                    format!(
                        "Adding back references, {} left, {} paths so far",
                        progress.remaining, progress.results
                    ),
                ),
            };

            return html! {
                <Cluster nowrap=true class={class::STATUS_BAR}>
                    {bar}
                    <span class={sockel::class::TRUNCATE}>{text}</span>
                </Cluster>
            };
        }

        let Some(last) = &self.state.last else {
            return html! {};
        };

        let mut text = match &last.error {
            Some(error) => format!("Pointer scan failed: {error}"),
            None => format!(
                "Found {} paths in {}",
                last.results,
                duration(last.duration_ms)
            ),
        };

        if last.cancelled {
            text.push_str("; cancelled, paths found so far kept");
        }

        let tone = if last.error.is_some() {
            Tone::Danger
        } else if last.cancelled {
            Tone::Warning
        } else {
            Tone::Neutral
        };

        html! {
            <div class={class::STATUS_BAR}>
                <Notice {tone} status=true>{text}</Notice>
            </div>
        }
    }

    fn range_label(&self, page_end: u64) -> String {
        if self.total == 0 {
            return String::from("No paths");
        }

        format!(
            "{}-{} of {}",
            self.offset + 1,
            page_end.max(self.offset + 1),
            self.total
        )
    }

    fn empty_label(&self) -> &'static str {
        if self.state.running {
            "Scanning..."
        } else if self.state.last.is_some() {
            "No paths"
        } else {
            "Select a scratch entry and press Pointer scan to find paths to it"
        }
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
