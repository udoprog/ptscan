use std::collections::BTreeSet;

use gloo_timers::callback::Timeout;
use musli_web::web03::prelude::*;
use sockel::{
    Button, ButtonSize, ButtonVariant, Checkbox, EmptyState, InputSize, Menu, MenuWidth, Pager,
    PagerCounts, Table, TableCell, TextInput, ToolbarSpacer,
};
use yew::prelude::*;

use crate::class;
use crate::clipboard;
use crate::icons::{AppIcon, icon};
use crate::ui::actions::{Action, RowMenu, more};
use crate::ui::pane::{Pane, PaneBar, PaneHead, StatusLine, StatusTone};

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
    /// Told the settings as edited whenever they change, none while invalid,
    /// so a scan started elsewhere uses what is shown.
    pub on_edit: Callback<Option<api::PointerScanSettings>>,
    pub on_error: Callback<String>,
    /// Tells of a finished action in passing.
    pub on_notice: Callback<String>,
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
    /// Selects a path a context menu opens on, unless it already is.
    Pick(u64),
    SelectPage,
    CopyPaths,
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
    /// Paths sent to the scratch list whose responses are still to come, and
    /// how many of the received ones were added; one notice tells of them all.
    adding_left: usize,
    added: usize,
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

    fn report(&self, ctx: &Context<Self>) {
        ctx.props().on_edit.emit(self.settings());
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
            adding_left: 0,
            added: 0,
            state_request: ws::Request::default(),
            cancel: ws::Request::default(),
            page: ws::Request::default(),
            adding: Vec::new(),
            _state_listener: ws.on_broadcast(ctx.link().callback(Msg::StateChanged)),
            _progress_listener: ws.on_broadcast(ctx.link().callback(Msg::Progressed)),
        };

        this.apply_settings(&ctx.props().settings);
        this.report(ctx);

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
        self.report(ctx);

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
                Ok(state) => self.set_state(ctx, state),
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
                self.report(ctx);
                self.schedule_save(ctx);
            }
            Msg::Offset(text) => {
                self.offset_limit = text;
                self.report(ctx);
                self.schedule_save(ctx);
            }
            Msg::MaxPaths(text) => {
                self.max_paths = text;
                self.report(ctx);
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
            Msg::Pick(index) => {
                if self.selected.contains(&index) {
                    return false;
                }

                self.selected.clear();
                self.selected.insert(index);
            }
            Msg::CopyPaths => {
                let text = clipboard::text(self.picked().map(|path| vec![path.as_str()]));
                clipboard::copy(text, on_error);
                return false;
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

                let picked = self.picked().cloned().collect::<Vec<_>>();

                self.adding.retain(|r| r.is_pending());
                if self.adding_left == 0 {
                    self.added = 0;
                }

                self.adding_left = self.adding_left.saturating_add(picked.len());

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
            Msg::Added(result) => {
                match result.and_then(|p| p.decode_response()) {
                    Ok(response) => {
                        let errors = [
                            response.pointer_error.map(|e| format!("pointer: {e}")),
                            response.value_type_error.map(|e| format!("type: {e}")),
                            response.error,
                        ];

                        for error in errors.into_iter().flatten() {
                            on_error.emit(format!("Adding path to scratch: {error}"));
                        }

                        if response.entry.is_some() {
                            self.added = self.added.saturating_add(1);
                        }
                    }
                    Err(error) => on_error.emit(format!("Adding path to scratch: {error}")),
                }

                self.adding_left = self.adding_left.saturating_sub(1);

                if self.adding_left == 0 {
                    match self.added {
                        0 => {}
                        1 => ctx
                            .props()
                            .on_notice
                            .emit(String::from("Added 1 path to the scratch list")),
                        n => ctx
                            .props()
                            .on_notice
                            .emit(format!("Added {n} paths to the scratch list")),
                    }
                }

                return false;
            }
        }

        true
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let props = ctx.props();
        let link = ctx.link();
        let running = self.state.running;

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
                    <tr key={index} {class} {onclick}
                        oncontextmenu={link.callback(move |_| Msg::Pick(index))}>
                        <TableCell numeric=true cut=true class={class::MUTED}>{index}</TableCell>
                        <TableCell class={class::MONO} cut=true tip={AttrValue::from(path.clone())}>{path}</TableCell>
                    </tr>
                }
            })
            .collect::<Html>();

        let all = !self.paths.is_empty() && self.selected.len() == self.paths.len();
        let selected = self.selected.len();
        let can_add = selected > 0 && props.open && self.value_type.is_some();

        let head = html! {
            <>
                <TableCell head=true numeric=true width="5.5rem">
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

        let cancel = running.then(|| {
            html! {
                <Button size={ButtonSize::Small} icon={icon(AppIcon::Stop)}
                    busy={self.cancel.is_pending()} tip="Stop the pointer scan, keeping the paths found so far"
                    onclick={link.callback(|_| Msg::Cancel)}>{"Cancel"}</Button>
            }
        });

        let actions = vec![
            Action::new(
                AppIcon::Add,
                "Add to scratch",
                link.callback(|_| Msg::AddSelected),
            )
            .disabled(!can_add),
            Action::new(
                AppIcon::Copy,
                "Copy as text",
                link.callback(|_| Msg::CopyPaths),
            )
            .disabled(selected == 0),
        ];

        let pages = pages(self.total);
        let page = usize::try_from(self.offset / PAGE as u64).unwrap_or(usize::MAX);
        let total = usize::try_from(self.total).unwrap_or(usize::MAX);
        let meta = Some(self.target())
            .filter(|t| !t.is_empty())
            .map(AttrValue::from);

        html! {
            <Pane label="Pointer scan">
                <PaneHead title="Pointer scan" {meta}>
                    {self.settings_menu(ctx)}
                    {cancel}
                </PaneHead>
                {self.status()}
                <PaneBar label="Pointer scan results">
                    <Pager {page} {pages} label="Pointer path pages" titles=true
                        counts={PagerCounts::new(total, PAGE as usize)}
                        on_page={link.callback(|page: usize| {
                            Msg::Page(u64::try_from(page).unwrap_or(u64::MAX).saturating_mul(PAGE as u64))
                        })} />
                    <ToolbarSpacer />
                    <Button size={ButtonSize::Small} variant={ButtonVariant::Primary}
                        icon={icon(AppIcon::Add)} tip="Add the selected paths to the scratch list"
                        disabled={!can_add}
                        onclick={link.callback(|_| Msg::AddSelected)}>
                        {match selected {
                            0 | 1 => String::from("Add"),
                            n => format!("Add {n}"),
                        }}
                    </Button>
                    {more("More path actions", &actions)}
                </PaneBar>
                <RowMenu label="Path actions" actions={actions.clone()}>
                    <div class={class::SCROLL} tabindex="0" onkeydown={link.batch_callback(|e: KeyboardEvent| {
                        (e.key() == "Enter").then_some(Msg::AddSelected)
                    })}>
                        <Table {head} columns={2} {empty} fixed=true label="Pointer paths">{rows}</Table>
                    </div>
                </RowMenu>
            </Pane>
        }
    }
}

impl PointerScanView {
    fn picked(&self) -> impl Iterator<Item = &String> {
        self.page_indexes()
            .zip(&self.paths)
            .filter(|(index, _)| self.selected.contains(index))
            .map(|(_, path)| path)
    }

    /// The scan's limits, in a menu so the head stays one line in a narrow
    /// pane; they save as they are edited.
    fn settings_menu(&self, ctx: &Context<Self>) -> Html {
        let link = ctx.link();
        let running = self.state.running;
        let invalid = self.settings().is_none();
        let tip = if invalid {
            "Pointer scan settings: some are invalid"
        } else {
            "Pointer scan settings"
        };

        let field = |id: &'static str,
                     label: &'static str,
                     tip: &'static str,
                     value: &str,
                     valid: bool,
                     mono: bool,
                     on_input: Callback<String>| {
            html! {
                <div class={class::MENU_FIELD}>
                    <label class={class::MUTED} for={id} title={tip}>{label}</label>
                    <TextInput {id} size={InputSize::Small}
                        class={classes!(class::NARROW, mono.then_some(class::MONO))}
                        inputmode={(!mono).then_some("numeric")} invalid={!valid}
                        disabled={running} value={value.to_owned()} {on_input}
                        on_change={link.callback(|_| Msg::Save)} />
                </div>
            }
        };

        html! {
            <Menu icon={icon(AppIcon::Options)} label={tip} tip={tip}
                size={ButtonSize::Small} variant={ButtonVariant::Quiet} width={MenuWidth::Wide}>
                {field("pointer-depth", "Depth", "Most pointers followed in one path",
                    &self.depth, parse_depth(&self.depth).is_some(), false, link.callback(Msg::Depth))}
                {field("pointer-offset", "Max offset", "Largest offset from a pointer to the next address, in hex or decimal",
                    &self.offset_limit, parse_offset(&self.offset_limit).is_some(), true, link.callback(Msg::Offset))}
                {field("pointer-max-paths", "Max paths", "The scan stops once it has found this many paths",
                    &self.max_paths, parse_max_paths(&self.max_paths).is_some(), false, link.callback(Msg::MaxPaths))}
            </Menu>
        }
    }

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

            return html! { <StatusLine lead={bar} {text} /> };
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
            StatusTone::Danger
        } else if last.cancelled || last.truncated {
            StatusTone::Warning
        } else {
            StatusTone::Neutral
        };

        html! { <StatusLine {tone} {text} /> }
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

/// How many pages `total` paths fill; at least one.
fn pages(total: u64) -> usize {
    usize::try_from(total.div_ceil(PAGE as u64).max(1)).unwrap_or(usize::MAX)
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
