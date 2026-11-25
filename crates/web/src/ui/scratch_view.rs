use std::collections::BTreeSet;

use gloo_timers::callback::Interval;
use musli_web::web03::prelude::*;
use sockel::{
    Button, ButtonSize, ButtonVariant, Card, Checkbox, Cluster, Dialog, DialogDismiss, EmptyState,
    FormRow, FormRows, Inset, Table, TableCell, TextInput, Toolbar, ToolbarSpacer,
};
use yew::prelude::*;

use crate::class;
use crate::clipboard;

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
    Loaded(Result<ws::Packet<api::GetScratch>, ws::Error>),
    Changed(Result<ws::Packet<api::ScratchChanged>, ws::Error>),
    Tick,
    Select(u64, bool),
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
                self.pointer_scan = ctx
                    .props()
                    .ws
                    .request()
                    .body(api::PointerScanRequest { id })
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
        }

        true
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let props = ctx.props();
        let link = ctx.link();
        let none = self.selected.is_empty();

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
                    <tr key={id} {class} {onclick} ondblclick={link.callback(move |_| Msg::Edit(id))}>
                        <TableCell class={class::MONO} cut=true>{&entry.pointer}</TableCell>
                        <TableCell class={class::MONO}>{&entry.value_type}</TableCell>
                        <TableCell class={classes!(class::MONO, class::MUTED)}>{address}</TableCell>
                        <TableCell class={class::MONO}>{entry.value.as_deref().unwrap_or("?")}</TableCell>
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
                <TableCell head=true>{"Type"}</TableCell>
                <TableCell head=true>{"Address"}</TableCell>
                <TableCell head=true>{"Value"}</TableCell>
            </>
        };

        let empty = self.entries.is_empty().then(
            || html! { <EmptyState title="Add scan results to watch them here" compact=true /> },
        );

        let pointer_scan_title = if self.pointer_scan_running {
            "A pointer scan is running"
        } else {
            "Find pointer paths to the selected entry's address"
        };

        html! {
            <Card inset={Inset::Flush} class={class::PANE}>
                <Toolbar label="Scratch" band=true>
                    <span class={class::TITLE}>{"Scratch"}</span>
                    <span class={class::MUTED}>{match self.entries.len() {
                        1 => String::from("1 entry"),
                        n => format!("{n} entries"),
                    }}</span>
                    <ToolbarSpacer />
                    <Button size={ButtonSize::Small} disabled={self.selected.len() != 1}
                        onclick={self.selected.first().copied().map(|id| link.callback(move |_| Msg::Edit(id))).unwrap_or_default()}>
                        {"Edit"}
                    </Button>
                    <Button size={ButtonSize::Small} title={pointer_scan_title}
                        busy={self.pointer_scan.is_pending()}
                        disabled={self.selected.len() != 1 || !props.open || !props.attached || self.pointer_scan_running}
                        onclick={self.selected.first().copied().map(|id| link.callback(move |_| Msg::PointerScan(id))).unwrap_or_default()}>
                        {"Pointer scan"}
                    </Button>
                    <Button size={ButtonSize::Small} title="Copy the selected entries as tab-separated text"
                        disabled={none} onclick={link.callback(|_| Msg::CopyText)}>{"Copy"}</Button>
                    <Button size={ButtonSize::Small} title="Copy the selected entries as JSON"
                        disabled={none} onclick={link.callback(|_| Msg::CopyJson)}>{"Copy JSON"}</Button>
                    <Button size={ButtonSize::Small} variant={ButtonVariant::Danger}
                        busy={self.remove.is_pending()}
                        disabled={none || !props.open}
                        onclick={link.callback(|_| Msg::RemoveSelected)}>
                        {match self.selected.len() {
                            0 | 1 => String::from("Remove"),
                            n => format!("Remove {n}"),
                        }}
                    </Button>
                </Toolbar>
                <div class={class::SCROLL} tabindex="0" onkeydown={link.batch_callback(|e: KeyboardEvent| {
                    (e.key() == "Delete").then_some(Msg::RemoveSelected)
                })}>
                    <Table {head} columns={4} {empty} label="Scratch entries">{rows}</Table>
                </div>
                {dialog}
            </Card>
        }
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
                <Button submit=true form="scratch-form" variant={ButtonVariant::Primary} {busy}>
                    {"Save"}
                </Button>
            </>
        };

        html! {
            <Dialog open=true on_dismiss={close} title="Scratch entry" dismiss={DialogDismiss::NoOutside} {actions}>
                <form id="scratch-form" class={class::FORM} {onsubmit}>
                    <FormRows>
                        <FormRow id="scratch-address" label={html! {"Address"}}>
                            <span class={class::MONO}>{address}</span>
                        </FormRow>
                        <FormRow id="scratch-current" label={html! {"Current value"}}>
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
                                <Button title="Write the value at the entry's address"
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
