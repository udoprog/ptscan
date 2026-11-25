use std::collections::BTreeSet;

use gloo_timers::callback::Timeout;
use musli_web::web03::prelude::*;
use sockel::{
    Button, ButtonSize, ButtonVariant, Card, Checkbox, Cluster, EmptyState, Gap, InputSize, Inset,
    Notice, Table, TableCell, TextInput, Tone, Toolbar, ToolbarSpacer,
};
use yew::prelude::*;

use crate::class;

const PAGE: u32 = 100;
/// How long the settings rest after an edit before they are saved.
const SAVE_MS: u32 = 500;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub ws: ws::Handle,
    /// The connection is open.
    pub open: bool,
    pub attached: bool,
    /// The open session.
    pub session: Option<u64>,
    /// The open session's settings, as the server holds them.
    pub settings: api::PointerScanSettings,
    pub on_error: Callback<String>,
}

pub enum Msg {
    State(&'static str, Result<api::PointerScanState, ws::Error>),
    StateChanged(Result<ws::Packet<api::PointerScanStateChanged>, ws::Error>),
    Progressed(Result<ws::Packet<api::PointerScanProgressed>, ws::Error>),
    Cancel,
    Depth(String),
    Offset(String),
    MaxPaths(String),
    Save,
    Saved(Result<ws::Packet<api::SetPointerScanSettings>, ws::Error>),
    Page(u64),
    Loaded(Result<ws::Packet<api::PointerScanResults>, ws::Error>),
    Select(u64, bool),
    SelectPage,
    AddSelected,
    Added(Result<ws::Packet<api::AddScratch>, ws::Error>),
}

pub struct PointerScanView {
    depth: String,
    offset_limit: String,
    max_paths: String,
    session: Option<u64>,
    /// The settings as last read from or saved to the server.
    synced: api::PointerScanSettings,
    save_timer: Option<Timeout>,
    save: ws::Request,
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

    fn settings(&self) -> Option<api::PointerScanSettings> {
        Some(api::PointerScanSettings {
            max_depth: parse_depth(&self.depth)?,
            max_offset: parse_offset(&self.offset_limit)?,
            max_paths: parse_max_paths(&self.max_paths)?,
        })
    }

    fn apply_settings(&mut self, settings: &api::PointerScanSettings) {
        self.depth = settings.max_depth.to_string();
        self.offset_limit = format!("{:#x}", settings.max_offset);
        self.max_paths = settings.max_paths.to_string();
        self.synced = settings.clone();
    }

    /// Follow the server's settings, unless an edit of ours is still to be
    /// saved.
    fn sync_settings(&mut self, ctx: &Context<Self>) {
        let props = ctx.props();

        if props.session != self.session {
            self.session = props.session;
            self.save_timer = None;
            self.save = ws::Request::default();
            self.apply_settings(&props.settings);
        } else if props.settings != self.synced {
            if self.save_timer.is_none() && !self.save.is_pending() {
                self.apply_settings(&props.settings);
            } else {
                self.synced = props.settings.clone();
            }
        }
    }

    fn schedule_save(&mut self, ctx: &Context<Self>) {
        let link = ctx.link().clone();
        self.save_timer = Some(Timeout::new(SAVE_MS, move || link.send_message(Msg::Save)));
    }
}

impl Component for PointerScanView {
    type Message = Msg;
    type Properties = Props;

    fn create(ctx: &Context<Self>) -> Self {
        let ws = &ctx.props().ws;

        let mut this = Self {
            depth: String::new(),
            offset_limit: String::new(),
            max_paths: String::new(),
            session: ctx.props().session,
            synced: api::PointerScanSettings::default(),
            save_timer: None,
            save: ws::Request::default(),
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

        this.apply_settings(&ctx.props().settings);

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

        self.sync_settings(ctx);

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
            Msg::Depth(text) => {
                self.depth = text;
                self.schedule_save(ctx);
            }
            Msg::Offset(text) => {
                self.offset_limit = text;
                self.schedule_save(ctx);
            }
            Msg::MaxPaths(text) => {
                self.max_paths = text;
                self.schedule_save(ctx);
            }
            Msg::Save => {
                self.save_timer = None;

                let Some(settings) = self.settings().filter(|s| *s != self.synced) else {
                    return false;
                };

                self.synced = settings.clone();
                self.save = ctx
                    .props()
                    .ws
                    .request()
                    .body(api::SetPointerScanSettingsRequest { settings })
                    .on_packet(ctx.link().callback(Msg::Saved))
                    .send();
                return false;
            }
            Msg::Saved(result) => {
                match result.and_then(|p| p.decode_response()) {
                    Ok(response) => {
                        if let Some(error) = response.error {
                            on_error.emit(format!("Saving pointer scan settings: {error}"));
                        }
                    }
                    Err(error) => on_error.emit(format!("Saving pointer scan settings: {error}")),
                }

                return false;
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
                    <Cluster gap={Gap::Tight} nowrap=true>
                        <label class={class::MUTED} for="pointer-depth">{"Depth"}</label>
                        <TextInput id="pointer-depth" size={InputSize::Small} class={class::NARROW}
                            inputmode="numeric" invalid={parse_depth(&self.depth).is_none()}
                            disabled={running} value={self.depth.clone()}
                            on_input={link.callback(Msg::Depth)} />
                        <label class={class::MUTED} for="pointer-offset">{"Max offset"}</label>
                        <TextInput id="pointer-offset" size={InputSize::Small}
                            class={classes!(class::NARROW, class::MONO)}
                            invalid={parse_offset(&self.offset_limit).is_none()}
                            disabled={running} value={self.offset_limit.clone()}
                            on_input={link.callback(Msg::Offset)} />
                        <label class={class::MUTED} for="pointer-max-paths"
                            title="The scan stops once it has found this many paths">{"Max paths"}</label>
                        <TextInput id="pointer-max-paths" size={InputSize::Small} class={class::NARROW}
                            inputmode="numeric" invalid={parse_max_paths(&self.max_paths).is_none()}
                            disabled={running} value={self.max_paths.clone()}
                            on_input={link.callback(Msg::MaxPaths)} />
                    </Cluster>
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
        } else if last.truncated {
            text.push_str("; stopped at the max paths limit, raise it to find more");
        }

        let tone = if last.error.is_some() {
            Tone::Danger
        } else if last.cancelled || last.truncated {
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

/// Most pointers followed in one path, from 1 to the server's limit.
fn parse_depth(text: &str) -> Option<u32> {
    let depth = text.trim().parse::<u32>().ok()?;
    (1..=api::PointerScanSettings::MAX_DEPTH)
        .contains(&depth)
        .then_some(depth)
}

/// An offset in hex (`0x1000`) or decimal, from 1 to the server's limit.
fn parse_offset(text: &str) -> Option<u32> {
    let text = text.trim();

    let offset = match text.strip_prefix("0x").or_else(|| text.strip_prefix("0X")) {
        Some(hex) => u32::from_str_radix(hex, 16).ok()?,
        None => text.parse::<u32>().ok()?,
    };

    (1..=api::PointerScanSettings::MAX_OFFSET)
        .contains(&offset)
        .then_some(offset)
}

/// Most paths a scan stores, from 1 to the server's limit.
fn parse_max_paths(text: &str) -> Option<u32> {
    let paths = text.trim().replace('_', "").parse::<u32>().ok()?;
    (1..=api::PointerScanSettings::MAX_PATHS)
        .contains(&paths)
        .then_some(paths)
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

#[cfg(test)]
mod tests {
    use super::{parse_depth, parse_max_paths, parse_offset};

    #[test]
    fn depth() {
        assert_eq!(parse_depth(" 7 "), Some(7));
        assert_eq!(parse_depth("0"), None);
        assert_eq!(parse_depth("17"), None);
        assert_eq!(parse_depth("x"), None);
    }

    #[test]
    fn offset() {
        assert_eq!(parse_offset("0x1000"), Some(0x1000));
        assert_eq!(parse_offset("0X10"), Some(0x10));
        assert_eq!(parse_offset("4096"), Some(4096));
        assert_eq!(parse_offset("0x0"), None);
        assert_eq!(parse_offset("0x100001"), None);
        assert_eq!(parse_offset("0x"), None);
    }

    #[test]
    fn max_paths() {
        assert_eq!(parse_max_paths(" 1000000 "), Some(1_000_000));
        assert_eq!(parse_max_paths("1_000"), Some(1000));
        assert_eq!(parse_max_paths("0"), None);
        assert_eq!(parse_max_paths("100000001"), None);
        assert_eq!(parse_max_paths("x"), None);
    }
}
