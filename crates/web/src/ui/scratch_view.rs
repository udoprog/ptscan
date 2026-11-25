use std::collections::BTreeSet;

use gloo_timers::callback::Interval;
use musli_web::web03::prelude::*;
use sockel::{
    Button, ButtonVariant, Checkbox, Cluster, Dialog, DialogDismiss, EmptyState, FormRow, FormRows,
    Table, TableCell, TextInput,
};
use yew::prelude::*;

use crate::class;
use crate::clipboard;
use crate::icons::{AppIcon, icon};
use crate::ui::actions::{Action, RowMenu, more, tool};
use crate::ui::file_browser::{FileBrowser, FileMode, file_name, refused, split_path};
use crate::ui::pane::{Pane, PaneHead};

const REFRESH_MS: u32 = 1000;
const EXTENSION: &str = ".ptscan.json";
const DEFAULT_NAME: &str = "scratch.ptscan.json";

#[derive(Properties, PartialEq)]
pub struct Props {
    pub ws: ws::Handle,
    /// The connection is open.
    pub open: bool,
    pub attached: bool,
    /// The pointer scan settings as edited, or none while they are invalid.
    pub pointer_scan: Option<api::PointerScanSettings>,
    pub on_error: Callback<String>,
    pub on_notice: Callback<String>,
}

pub enum Msg {
    Loaded(Result<ws::Packet<api::GetScratch>, ws::Error>),
    Changed(Result<ws::Packet<api::ScratchChanged>, ws::Error>),
    Tick,
    Select(u64, bool),
    /// Selects an entry a context menu opens on, unless it already is.
    Pick(u64),
    SelectAll,
    Edit(u64),
    CloseDialog,
    RemoveSelected,
    Removed(Result<ws::Packet<api::RemoveScratch>, ws::Error>),
    CopyText,
    CopyJson,
    PointerScan(u64),
    PointerScanStarted(Result<ws::Packet<api::PointerScan>, ws::Error>),
    PointerScanState(Result<ws::Packet<api::GetPointerScanState>, ws::Error>),
    PointerScanChanged(Result<ws::Packet<api::PointerScanStateChanged>, ws::Error>),
    Browse(FileMode),
    CloseBrowser,
    Chosen(String),
    /// Load the chosen file, replacing the entries or not.
    LoadFile(bool),
    CancelLoad,
    FileSaved(Result<ws::Packet<api::SaveScratch>, ws::Error>),
    FileLoaded(Result<ws::Packet<api::LoadScratch>, ws::Error>),
}

pub struct ScratchView {
    entries: Vec<api::ScratchEntry>,
    selected: BTreeSet<u64>,
    dialog: Option<u64>,
    load: ws::Request,
    remove: ws::Request,
    pointer_scan: ws::Request,
    pointer_scan_state: ws::Request,
    pointer_scan_running: bool,
    browser: Option<FileMode>,
    /// The file chosen to load, while asking whether it replaces the entries.
    loading: Option<String>,
    /// The file last saved or loaded, which the browser starts at.
    file: Option<String>,
    file_request: ws::Request,
    _listener: ws::Listener,
    _pointer_scan_listener: ws::Listener,
    _timer: Interval,
}

impl ScratchView {
    fn load(&mut self, ctx: &Context<Self>) {
        self.load = ctx
            .props()
            .ws
            .request()
            .body(api::GetScratchRequest)
            .on_packet(ctx.link().callback(Msg::Loaded))
            .send();
    }

    fn load_pointer_scan_state(&mut self, ctx: &Context<Self>) {
        self.pointer_scan_state = ctx
            .props()
            .ws
            .request()
            .body(api::GetPointerScanStateRequest)
            .on_packet(ctx.link().callback(Msg::PointerScanState))
            .send();
    }

    fn set_entries(&mut self, entries: Vec<api::ScratchEntry>) {
        self.selected
            .retain(|id| entries.iter().any(|e| e.id == *id));

        if let Some(id) = self.dialog
            && !entries.iter().any(|e| e.id == id)
        {
            self.dialog = None;
        }

        self.entries = entries;
    }

    fn load_file(&mut self, ctx: &Context<Self>, path: String, replace: bool) {
        self.file_request = ctx
            .props()
            .ws
            .request()
            .body(api::LoadScratchRequest {
                path: path.clone(),
                replace,
            })
            .on_packet(ctx.link().callback(Msg::FileLoaded))
            .send();
        self.file = Some(path);
    }

    fn picked(&self) -> impl Iterator<Item = &api::ScratchEntry> {
        self.entries
            .iter()
            .filter(|e| self.selected.contains(&e.id))
    }
}

impl Component for ScratchView {
    type Message = Msg;
    type Properties = Props;

    fn create(ctx: &Context<Self>) -> Self {
        let link = ctx.link().clone();

        let mut this = Self {
            entries: Vec::new(),
            selected: BTreeSet::new(),
            dialog: None,
            load: ws::Request::default(),
            remove: ws::Request::default(),
            pointer_scan: ws::Request::default(),
            pointer_scan_state: ws::Request::default(),
            pointer_scan_running: false,
            browser: None,
            loading: None,
            file: None,
            file_request: ws::Request::default(),
            _listener: ctx
                .props()
                .ws
                .on_broadcast(ctx.link().callback(Msg::Changed)),
            _pointer_scan_listener: ctx
                .props()
                .ws
                .on_broadcast(ctx.link().callback(Msg::PointerScanChanged)),
            _timer: Interval::new(REFRESH_MS, move || link.send_message(Msg::Tick)),
        };

        if ctx.props().open {
            this.load(ctx);
            this.load_pointer_scan_state(ctx);
        }

        this
    }

    fn changed(&mut self, ctx: &Context<Self>, old: &Self::Properties) -> bool {
        let props = ctx.props();

        if props.open && (!old.open || props.attached != old.attached) {
            self.load(ctx);
            self.load_pointer_scan_state(ctx);
        }

        true
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        let on_error = ctx.props().on_error.clone();

        match msg {
            Msg::Loaded(result) => match result.and_then(|p| p.decode_response()) {
                Ok(list) => {
                    if list.entries == self.entries {
                        return false;
                    }

                    self.set_entries(list.entries);
                }
                Err(error) => on_error.emit(format!("Reading scratch list: {error}")),
            },
            Msg::Changed(result) => match result.and_then(|p| p.decode_event()) {
                Ok(list) => self.set_entries(list.entries),
                Err(error) => on_error.emit(format!("Scratch list update: {error}")),
            },
            Msg::Tick => {
                if ctx.props().open && !self.entries.is_empty() && !self.load.is_pending() {
                    self.load(ctx);
                }

                return false;
            }
            Msg::Select(id, extend) => {
                if !extend {
                    self.selected.clear();
                    self.selected.insert(id);
                } else if !self.selected.remove(&id) {
                    self.selected.insert(id);
                }
            }
            Msg::Pick(id) => {
                if self.selected.contains(&id) {
                    return false;
                }

                self.selected.clear();
                self.selected.insert(id);
            }
            Msg::SelectAll => {
                if self.selected.len() == self.entries.len() {
                    self.selected.clear();
                } else {
                    self.selected = self.entries.iter().map(|e| e.id).collect();
                }
            }
            Msg::Edit(id) => self.dialog = Some(id),
            Msg::CloseDialog => self.dialog = None,
            Msg::RemoveSelected => {
                if self.selected.is_empty() {
                    return false;
                }

                self.remove = ctx
                    .props()
                    .ws
                    .request()
                    .body(api::RemoveScratchRequest {
                        ids: self.selected.iter().copied().collect(),
                    })
                    .on_packet(ctx.link().callback(Msg::Removed))
                    .send();
            }
            Msg::Removed(result) => match result.and_then(|p| p.decode_response()) {
                Ok(list) => self.set_entries(list.entries),
                Err(error) => on_error.emit(format!("Removing from scratch: {error}")),
            },
            Msg::CopyText => {
                let text = clipboard::text(self.picked().map(|e| {
                    vec![
                        e.pointer.as_str(),
                        e.value_type.as_str(),
                        e.value.as_deref().unwrap_or("?"),
                    ]
                }));

                clipboard::copy(text, on_error);
                return false;
            }
            Msg::CopyJson => {
                let addresses = self
                    .picked()
                    .map(|e| e.address.map(|a| format!("{a:#x}")))
                    .collect::<Vec<_>>();

                let json = clipboard::json(self.picked().zip(&addresses).map(|(e, address)| {
                    vec![
                        ("pointer", Some(e.pointer.as_str())),
                        ("type", Some(e.value_type.as_str())),
                        ("address", address.as_deref()),
                        ("value", e.value.as_deref()),
                    ]
                }));

                clipboard::copy(json, on_error);
                return false;
            }
            Msg::PointerScan(id) => {
                let Some(settings) = ctx.props().pointer_scan.clone() else {
                    return false;
                };

                self.pointer_scan = ctx
                    .props()
                    .ws
                    .request()
                    .body(api::PointerScanRequest { id, settings })
                    .on_packet(ctx.link().callback(Msg::PointerScanStarted))
                    .send();
            }
            Msg::PointerScanStarted(result) => match result.and_then(|p| p.decode_response()) {
                Ok(api::PointerScanResponse { error: Some(error) }) => {
                    on_error.emit(format!("Starting pointer scan: {error}"))
                }
                Ok(_) => {}
                Err(error) => on_error.emit(format!("Starting pointer scan: {error}")),
            },
            Msg::PointerScanState(result) => match result.and_then(|p| p.decode_response()) {
                Ok(state) => self.pointer_scan_running = state.running,
                Err(error) => on_error.emit(format!("Reading pointer scan state: {error}")),
            },
            Msg::PointerScanChanged(result) => match result.and_then(|p| p.decode_event()) {
                Ok(state) => self.pointer_scan_running = state.running,
                Err(error) => on_error.emit(format!("Pointer scan state update: {error}")),
            },
            Msg::Browse(mode) => self.browser = Some(mode),
            Msg::CloseBrowser => self.browser = None,
            Msg::Chosen(path) => match self.browser {
                Some(FileMode::Save) => {
                    self.file_request = ctx
                        .props()
                        .ws
                        .request()
                        .body(api::SaveScratchRequest { path: path.clone() })
                        .on_packet(ctx.link().callback(Msg::FileSaved))
                        .send();
                    self.file = Some(path);
                }
                Some(FileMode::Open) if self.entries.is_empty() => {
                    self.load_file(ctx, path, false);
                }
                Some(FileMode::Open) => self.loading = Some(path),
                None => return false,
            },
            Msg::LoadFile(replace) => {
                let Some(path) = self.loading.take() else {
                    return false;
                };

                self.load_file(ctx, path, replace);
            }
            Msg::CancelLoad => self.loading = None,
            Msg::FileSaved(result) => {
                let name = self.file.as_deref().map(file_name).unwrap_or_default();

                match result.and_then(|p| p.decode_response()) {
                    Ok(response) => ctx.props().on_notice.emit(match response.refusal {
                        None => format!("Saved {} to {name}", entries(response.entries)),
                        Some(refusal) => {
                            refused("save", name, "a scratch list", refusal, response.error)
                        }
                    }),
                    Err(error) => on_error.emit(format!("Saving scratch list: {error}")),
                }
            }
            Msg::FileLoaded(result) => {
                let name = self.file.as_deref().map(file_name).unwrap_or_default();

                match result.and_then(|p| p.decode_response()) {
                    Ok(response) => {
                        let notice = match response.refusal {
                            None => format!("Loaded {} from {name}", entries(response.entries)),
                            Some(refusal) => {
                                refused("load", name, "a scratch list", refusal, response.error)
                            }
                        };

                        ctx.props().on_notice.emit(notice);
                        self.set_entries(response.list.entries);
                    }
                    Err(error) => on_error.emit(format!("Loading scratch list: {error}")),
                }
            }
        }

        true
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let props = ctx.props();
        let link = ctx.link();

        let rows = self
            .entries
            .iter()
            .map(|entry| {
                let id = entry.id;
                let class = classes!(
                    sockel::class::TABLE_ROW,
                    class::ROW,
                    self.selected.contains(&id).then_some(class::SELECTED),
                );
                let onclick = link.callback(move |e: MouseEvent| {
                    Msg::Select(id, e.ctrl_key() || e.meta_key() || e.shift_key())
                });
                let address = entry
                    .address
                    .map(|a| format!("{a:#x}"))
                    .unwrap_or_else(|| String::from("?"));

                html! {
                    <tr key={id} {class} {onclick}
                        oncontextmenu={link.callback(move |_| Msg::Pick(id))}
                        ondblclick={link.callback(move |_| Msg::Edit(id))}>
                        <TableCell class={class::MONO} cut=true>{&entry.pointer}</TableCell>
                        <TableCell class={class::MONO} cut=true>{&entry.value_type}</TableCell>
                        <TableCell class={classes!(class::MONO, class::MUTED)} cut=true>{address}</TableCell>
                        <TableCell class={class::MONO} cut=true>{entry.value.as_deref().unwrap_or("?")}</TableCell>
                    </tr>
                }
            })
            .collect::<Html>();

        let dialog = self.dialog.and_then(|id| {
            let entry = self.entries.iter().find(|e| e.id == id)?;

            Some(html! {
                <ScratchDialog
                    key={id}
                    ws={props.ws.clone()}
                    entry={entry.clone()}
                    on_error={props.on_error.clone()}
                    on_close={link.callback(|()| Msg::CloseDialog)}
                />
            })
        });

        let all = !self.entries.is_empty() && self.selected.len() == self.entries.len();

        let head = html! {
            <>
                <TableCell head=true>
                    <Checkbox title="Select every entry" checked={all}
                        on_change={link.callback(|_| Msg::SelectAll)}>{"Pointer"}</Checkbox>
                </TableCell>
                <TableCell head=true width="4rem">{"Type"}</TableCell>
                <TableCell head=true width="22%">{"Address"}</TableCell>
                <TableCell head=true width="16%">{"Value"}</TableCell>
            </>
        };

        let empty = self.entries.is_empty().then(
            || html! { <EmptyState title="Add scan results to watch them here" compact=true /> },
        );

        let actions = self.actions(ctx);
        let overflow = [actions.clone(), self.file_actions(ctx)].concat();
        let meta = (!self.entries.is_empty()).then(|| {
            AttrValue::from(entries(
                u64::try_from(self.entries.len()).unwrap_or(u64::MAX),
            ))
        });

        html! {
            <Pane label="Scratch">
                <PaneHead title="Scratch" {meta}>
                    {for actions.iter().take(2).map(tool)}
                    {more("More scratch actions", &overflow)}
                </PaneHead>
                <RowMenu label="Scratch entry actions" actions={actions.clone()}>
                    <div class={class::SCROLL} tabindex="0" onkeydown={link.batch_callback(|e: KeyboardEvent| {
                        (e.key() == "Delete").then_some(Msg::RemoveSelected)
                    })}>
                        <Table {head} columns={4} {empty} fixed=true label="Scratch entries">{rows}</Table>
                    </div>
                </RowMenu>
                {dialog}
                {self.file_dialogs(ctx)}
            </Pane>
        }
    }
}

impl ScratchView {
    /// What can be done with the selected entries: the first two in the
    /// head, every one in its overflow menu and the rows' context menu.
    fn actions(&self, ctx: &Context<Self>) -> Vec<Action> {
        let props = ctx.props();
        let link = ctx.link();
        let none = self.selected.is_empty();
        let one = self
            .selected
            .first()
            .copied()
            .filter(|_| self.selected.len() == 1);

        let pointer_scan = if self.pointer_scan_running {
            "Pointer scan: one is running"
        } else if props.pointer_scan.is_none() {
            "Pointer scan: fix its settings first"
        } else {
            "Pointer scan: find pointer paths to the selected entry's address"
        };

        let remove = match self.selected.len() {
            0 | 1 => String::from("Remove"),
            n => format!("Remove {n}"),
        };

        vec![
            Action::new(
                AppIcon::Target,
                pointer_scan,
                one.map(|id| link.callback(move |_| Msg::PointerScan(id)))
                    .unwrap_or_default(),
            )
            .disabled(
                one.is_none()
                    || !props.open
                    || !props.attached
                    || self.pointer_scan_running
                    || self.pointer_scan.is_pending()
                    || props.pointer_scan.is_none(),
            ),
            Action::new(
                AppIcon::Edit,
                "Edit",
                one.map(|id| link.callback(move |_| Msg::Edit(id)))
                    .unwrap_or_default(),
            )
            .disabled(one.is_none()),
            Action::new(
                AppIcon::Copy,
                "Copy as text",
                link.callback(|_| Msg::CopyText),
            )
            .disabled(none),
            Action::new(
                AppIcon::Json,
                "Copy as JSON",
                link.callback(|_| Msg::CopyJson),
            )
            .disabled(none),
            Action::new(
                AppIcon::Remove,
                remove,
                link.callback(|_| Msg::RemoveSelected),
            )
            .disabled(none || !props.open || self.remove.is_pending())
            .danger(),
        ]
    }
}

impl ScratchView {
    /// Saving and loading the whole list, which only the overflow menu
    /// offers.
    fn file_actions(&self, ctx: &Context<Self>) -> Vec<Action> {
        let link = ctx.link();
        let busy = !ctx.props().open || self.file_request.is_pending();

        vec![
            Action::new(
                AppIcon::Save,
                "Save to file…",
                link.callback(|_| Msg::Browse(FileMode::Save)),
            )
            .disabled(busy),
            Action::new(
                AppIcon::Load,
                "Load from file…",
                link.callback(|_| Msg::Browse(FileMode::Open)),
            )
            .disabled(busy),
        ]
    }

    fn file_dialogs(&self, ctx: &Context<Self>) -> Html {
        let props = ctx.props();
        let link = ctx.link();

        let (dir, name) = match self.file.as_deref() {
            Some(path) => split_path(path),
            None => (None, DEFAULT_NAME),
        };

        let browser = self.browser.map(|mode| {
            let title = match mode {
                FileMode::Save => "Save scratch list",
                FileMode::Open => "Load scratch list",
            };

            html! {
                <FileBrowser ws={props.ws.clone()} open=true {mode} {title}
                    path={dir.map(AttrValue::from)} name={AttrValue::from(name.to_owned())}
                    extensions={vec![AttrValue::Static(EXTENSION)]}
                    on_choose={link.callback(Msg::Chosen)}
                    on_close={link.callback(|()| Msg::CloseBrowser)}
                    testid="scratch-file" />
            }
        });

        let loading = self.loading.as_deref().map(|path| {
            let actions = html! {
                <>
                    <Button variant={ButtonVariant::Quiet}
                        onclick={link.callback(|_| Msg::CancelLoad)}>{"Cancel"}</Button>
                    <Button variant={ButtonVariant::Danger} icon={icon(AppIcon::Remove)}
                        onclick={link.callback(|_| Msg::LoadFile(true))}>{"Replace"}</Button>
                    <Button variant={ButtonVariant::Primary} icon={icon(AppIcon::Add)}
                        onclick={link.callback(|_| Msg::LoadFile(false))}>{"Append"}</Button>
                </>
            };

            let count = entries(u64::try_from(self.entries.len()).unwrap_or(u64::MAX));

            html! {
                <Dialog open=true title="Load scratch list" icon={icon(AppIcon::Load)}
                    on_dismiss={link.callback(|_| Msg::CancelLoad)} {actions}
                    testid="scratch-load">
                    <p>
                        {format!("Add the entries of {} after the {count} in the list, or replace them?", file_name(path))}
                    </p>
                </Dialog>
            }
        });

        html! { <>{browser}{loading}</> }
    }
}

fn entries(count: u64) -> String {
    match count {
        1 => String::from("1 entry"),
        n => format!("{n} entries"),
    }
}

#[derive(Properties, PartialEq)]
pub struct DialogProps {
    pub ws: ws::Handle,
    /// The entry as of the latest read, so its value stays live.
    pub entry: api::ScratchEntry,
    pub on_error: Callback<String>,
    pub on_close: Callback<()>,
}

pub enum DialogMsg {
    Pointer(String),
    ValueType(String),
    Value(String),
    Save,
    Saved(Result<ws::Packet<api::EditScratch>, ws::Error>),
    Write,
    Written(Result<ws::Packet<api::WriteScratchValue>, ws::Error>),
}

pub struct ScratchDialog {
    pointer: String,
    value_type: String,
    value: String,
    pointer_error: Option<String>,
    value_type_error: Option<String>,
    value_error: Option<String>,
    status: Option<String>,
    request: ws::Request,
}

impl ScratchDialog {
    /// Report a response's errors, returning whether it succeeded.
    fn settle(&mut self, ctx: &Context<Self>, what: &str, response: api::ScratchResponse) -> bool {
        self.pointer_error = response.pointer_error;
        self.value_type_error = response.value_type_error;
        self.value_error = response.value_error;

        if let Some(error) = response.error {
            ctx.props().on_error.emit(format!("{what}: {error}"));
            return false;
        }

        self.pointer_error.is_none()
            && self.value_type_error.is_none()
            && self.value_error.is_none()
    }
}

impl Component for ScratchDialog {
    type Message = DialogMsg;
    type Properties = DialogProps;

    fn create(ctx: &Context<Self>) -> Self {
        let entry = &ctx.props().entry;

        Self {
            pointer: entry.pointer.clone(),
            value_type: entry.value_type.clone(),
            value: entry.value.clone().unwrap_or_default(),
            pointer_error: None,
            value_type_error: None,
            value_error: None,
            status: None,
            request: ws::Request::default(),
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        let props = ctx.props();

        match msg {
            DialogMsg::Pointer(text) => {
                self.pointer = text;
                self.pointer_error = None;
            }
            DialogMsg::ValueType(text) => {
                self.value_type = text;
                self.value_type_error = None;
            }
            DialogMsg::Value(text) => {
                self.value = text;
                self.value_error = None;
            }
            DialogMsg::Save => {
                let entry = &props.entry;
                self.status = None;
                self.request = props
                    .ws
                    .request()
                    .body(api::EditScratchRequest {
                        id: entry.id,
                        pointer: (self.pointer != entry.pointer).then(|| self.pointer.clone()),
                        value_type: (self.value_type != entry.value_type)
                            .then(|| self.value_type.clone()),
                    })
                    .on_packet(ctx.link().callback(DialogMsg::Saved))
                    .send();
            }
            DialogMsg::Saved(result) => match result.and_then(|p| p.decode_response()) {
                Ok(response) => {
                    if self.settle(ctx, "Editing scratch entry", response) {
                        self.status = Some(String::from("Saved"));
                    }
                }
                Err(error) => props
                    .on_error
                    .emit(format!("Editing scratch entry: {error}")),
            },
            DialogMsg::Write => {
                self.status = None;
                self.request = props
                    .ws
                    .request()
                    .body(api::WriteScratchValueRequest {
                        id: props.entry.id,
                        value: self.value.clone(),
                    })
                    .on_packet(ctx.link().callback(DialogMsg::Written))
                    .send();
            }
            DialogMsg::Written(result) => match result.and_then(|p| p.decode_response()) {
                Ok(response) => {
                    let value = response
                        .entry
                        .as_ref()
                        .and_then(|e| e.value.clone())
                        .unwrap_or_else(|| String::from("?"));

                    if self.settle(ctx, "Writing value", response) {
                        self.status = Some(format!("Now {value}"));
                    }
                }
                Err(error) => props.on_error.emit(format!("Writing value: {error}")),
            },
        }

        true
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let props = ctx.props();
        let link = ctx.link();
        let entry = &props.entry;
        let close = props.on_close.reform(|_| ());
        let busy = self.request.is_pending();

        let onsubmit = link.callback(|e: SubmitEvent| {
            e.prevent_default();
            DialogMsg::Save
        });

        let address = entry
            .address
            .map(|a| format!("{a:#x}"))
            .unwrap_or_else(|| String::from("?"));

        let error = |error: &Option<String>| error.as_ref().map(|e| html! { {e.clone()} });

        let on_write_key = link.batch_callback(|e: KeyboardEvent| {
            (e.key() == "Enter").then(|| {
                e.prevent_default();
                DialogMsg::Write
            })
        });

        let actions = html! {
            <>
                if let Some(status) = &self.status {
                    <span class={class::MUTED}>{status}</span>
                }
                <Button submit=true form="scratch-form" variant={ButtonVariant::Primary}
                    icon={icon(AppIcon::Edit)} {busy}>
                    {"Save"}
                </Button>
            </>
        };

        html! {
            <Dialog open=true on_dismiss={close} title="Scratch entry" dismiss={DialogDismiss::NoOutside} {actions}>
                <form id="scratch-form" class={class::FORM} {onsubmit}>
                    <FormRows>
                        <FormRow id="scratch-address" group=true label={html! {"Address"}}>
                            <span class={class::MONO}>{address}</span>
                        </FormRow>
                        <FormRow id="scratch-current" group=true label={html! {"Current value"}}>
                            <span class={class::MONO}>{entry.value.as_deref().unwrap_or("?")}</span>
                        </FormRow>
                        <FormRow id="scratch-pointer" label={html! {"Pointer"}} error={error(&self.pointer_error)}>
                            <TextInput id="scratch-pointer" class={class::MONO}
                                invalid={self.pointer_error.is_some()}
                                value={self.pointer.clone()}
                                on_input={link.callback(DialogMsg::Pointer)} />
                        </FormRow>
                        <FormRow id="scratch-type" label={html! {"Type"}} error={error(&self.value_type_error)}>
                            <TextInput id="scratch-type" class={class::MONO}
                                invalid={self.value_type_error.is_some()}
                                value={self.value_type.clone()}
                                on_input={link.callback(DialogMsg::ValueType)} />
                        </FormRow>
                        <FormRow id="scratch-value" label={html! {"Value"}} error={error(&self.value_error)}>
                            <Cluster nowrap=true>
                                <TextInput id="scratch-value" class={classes!(class::MONO, sockel::class::GROW)}
                                    invalid={self.value_error.is_some()}
                                    value={self.value.clone()}
                                    on_input={link.callback(DialogMsg::Value)}
                                    on_keydown={on_write_key} />
                                <Button tip="Write the value at the entry's address"
                                    icon={icon(AppIcon::Write)}
                                    {busy} onclick={link.callback(|_| DialogMsg::Write)}>
                                    {"Write"}
                                </Button>
                            </Cluster>
                        </FormRow>
                    </FormRows>
                </form>
            </Dialog>
        }
    }
}
