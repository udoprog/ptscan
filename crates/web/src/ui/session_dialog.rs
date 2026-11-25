use musli_web::web03::prelude::*;
use sockel::{
    Button, ButtonSize, ButtonVariant, Confirm, Dialog, DialogSize, EmptyState, InputSize, Notice,
    Pill, Table, TableCell, TextInput, Tone, Toolbar, ToolbarSpacer,
};
use wasm_bindgen::JsValue;
use yew::prelude::*;

use crate::class;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub ws: ws::Handle,
    /// The dialog is shown.
    pub open: bool,
    /// The connection is open.
    pub connected: bool,
    pub state: api::SessionState,
    pub on_close: Callback<()>,
}

pub enum Msg {
    NewName(String),
    Create,
    Select(u64),
    Name(String),
    Rename,
    Open(u64),
    AskDelete,
    CancelDelete,
    Delete,
    Created(Result<ws::Packet<api::CreateSession>, ws::Error>),
    Renamed(Result<ws::Packet<api::RenameSession>, ws::Error>),
    Opened(Result<ws::Packet<api::OpenSession>, ws::Error>),
    Deleted(Result<ws::Packet<api::DeleteSession>, ws::Error>),
}

pub struct SessionDialog {
    new_name: String,
    selected: Option<u64>,
    /// The name field of the selected session.
    name: String,
    confirm_delete: bool,
    error: Option<String>,
    request: ws::Request,
}

impl SessionDialog {
    fn select(&mut self, state: &api::SessionState, id: Option<u64>) {
        self.selected = id;
        self.name = id
            .and_then(|id| state.sessions.iter().find(|s| s.id == id))
            .map(|s| s.name.clone())
            .unwrap_or_default();
    }

    /// The outcome of a request, keeping its error to show.
    /// The state after a request which succeeded; otherwise keeps its error
    /// to show.
    fn outcome(
        &mut self,
        what: &str,
        result: Result<api::SessionResponse, ws::Error>,
    ) -> Option<api::SessionState> {
        match result {
            Ok(response) => {
                self.error = response.error.map(|e| format!("{what}: {e}"));
                self.error.is_none().then_some(response.state)
            }
            Err(error) => {
                self.error = Some(format!("{what}: {error}"));
                None
            }
        }
    }
}

impl Component for SessionDialog {
    type Message = Msg;
    type Properties = Props;

    fn create(ctx: &Context<Self>) -> Self {
        let mut this = Self {
            new_name: String::new(),
            selected: None,
            name: String::new(),
            confirm_delete: false,
            error: None,
            request: ws::Request::default(),
        };

        this.select(&ctx.props().state, ctx.props().state.current);
        this
    }

    fn changed(&mut self, ctx: &Context<Self>, old: &Self::Properties) -> bool {
        let props = ctx.props();

        if props.open && !old.open {
            self.error = None;
            self.confirm_delete = false;
            self.select(&props.state, props.state.current);
        } else if let Some(id) = self.selected
            && !props.state.sessions.iter().any(|s| s.id == id)
        {
            self.select(&props.state, props.state.current);
        }

        true
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        let props = ctx.props();
        let link = ctx.link();

        match msg {
            Msg::NewName(text) => self.new_name = text,
            Msg::Create => {
                self.request = props
                    .ws
                    .request()
                    .body(api::CreateSessionRequest {
                        name: self.new_name.clone(),
                    })
                    .on_packet(link.callback(Msg::Created))
                    .send();
            }
            Msg::Select(id) => {
                self.confirm_delete = false;
                self.select(&props.state, Some(id));
            }
            Msg::Name(text) => self.name = text,
            Msg::Rename => {
                let Some(id) = self.selected else {
                    return false;
                };

                self.request = props
                    .ws
                    .request()
                    .body(api::RenameSessionRequest {
                        id,
                        name: self.name.clone(),
                    })
                    .on_packet(link.callback(Msg::Renamed))
                    .send();
            }
            Msg::Open(id) => {
                self.request = props
                    .ws
                    .request()
                    .body(api::OpenSessionRequest { id })
                    .on_packet(link.callback(Msg::Opened))
                    .send();
            }
            Msg::AskDelete => self.confirm_delete = self.selected.is_some(),
            Msg::CancelDelete => self.confirm_delete = false,
            Msg::Delete => {
                let Some(id) = self.selected else {
                    return false;
                };

                self.request = props
                    .ws
                    .request()
                    .body(api::DeleteSessionRequest { id })
                    .on_packet(link.callback(Msg::Deleted))
                    .send();
            }
            Msg::Created(result) => {
                if let Some(state) =
                    self.outcome("Creating", result.and_then(|p| p.decode_response()))
                {
                    // The state broadcast may arrive before the response, so
                    // the newest session is taken to be the one created.
                    let created = state.sessions.iter().map(|s| s.id).max();

                    self.new_name.clear();
                    self.select(&state, created);
                }
            }
            Msg::Renamed(result) => {
                self.outcome("Renaming", result.and_then(|p| p.decode_response()));
            }
            Msg::Opened(result) => {
                if self
                    .outcome("Opening", result.and_then(|p| p.decode_response()))
                    .is_some()
                {
                    props.on_close.emit(());
                }
            }
            Msg::Deleted(result) => {
                self.confirm_delete = false;

                if let Some(state) =
                    self.outcome("Deleting", result.and_then(|p| p.decode_response()))
                {
                    self.select(&state, state.current);
                }
            }
        }

        true
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let props = ctx.props();
        let link = ctx.link();
        let state = &props.state;
        let busy = self.request.is_pending();
        let ready = props.connected && !busy;

        let selected = self
            .selected
            .and_then(|id| state.sessions.iter().find(|s| s.id == id));
        let is_current = selected.is_some_and(|s| Some(s.id) == state.current);

        let rows = state
            .sessions
            .iter()
            .map(|session| {
                let id = session.id;
                let class = classes!(
                    sockel::class::TABLE_ROW,
                    class::ROW,
                    (self.selected == Some(id)).then_some(class::SELECTED),
                );

                html! {
                    <tr key={id} {class}
                        onclick={link.callback(move |_| Msg::Select(id))}
                        ondblclick={link.callback(move |_| Msg::Open(id))}>
                        <TableCell cut=true>{&session.name}</TableCell>
                        <TableCell cut=true class={class::MUTED}>
                            {session.target.as_ref().map(|t| t.name.clone()).unwrap_or_default()}
                        </TableCell>
                        <TableCell class={class::MUTED}>{time(session.opened)}</TableCell>
                        <TableCell>
                            if state.current == Some(id) {
                                <Pill tone={Tone::Success}>{"open"}</Pill>
                            }
                        </TableCell>
                    </tr>
                }
            })
            .collect::<Html>();

        let head = html! {
            <>
                <TableCell head=true>{"Name"}</TableCell>
                <TableCell head=true>{"Target"}</TableCell>
                <TableCell head=true>{"Last opened"}</TableCell>
                <TableCell head=true><span class={sockel::class::VISUALLY_HIDDEN}>{"State"}</span></TableCell>
            </>
        };

        let empty = state.sessions.is_empty().then(|| {
            let title = if state.current.is_none() {
                "The server keeps no database, so it has no sessions"
            } else {
                "No sessions"
            };
            html! { <EmptyState {title} compact=true /> }
        });

        let on_create = link.callback(|e: SubmitEvent| {
            e.prevent_default();
            Msg::Create
        });

        let on_rename = link.callback(|e: SubmitEvent| {
            e.prevent_default();
            Msg::Rename
        });

        let open = selected
            .filter(|_| ready && !is_current)
            .map(|s| s.id)
            .map(|id| link.callback(move |_| Msg::Open(id)));

        let error = self.error.as_ref().map(|error| {
            html! {
                <div class={class::STATUS_BAR}>
                    <Notice tone={Tone::Danger} status=true>{error.clone()}</Notice>
                </div>
            }
        });

        let actions = html! {
            <Button variant={ButtonVariant::Primary} disabled={open.is_none()}
                title="Switch to the selected session: detaches and drops the scan and pointer-scan results"
                onclick={open.unwrap_or_default()}>
                {"Open"}
            </Button>
        };

        let delete_name = selected.map(|s| s.name.clone()).unwrap_or_default();

        html! {
            <>
                <Dialog open={props.open} on_dismiss={props.on_close.reform(|_| ())} title="Sessions"
                    size={DialogSize::Wide} flush=true {actions}>
                    <div class={class::DIALOG_BODY}>
                        <form class={class::FORM} onsubmit={on_create}>
                            <Toolbar label="New session" band=true chrome=true>
                                <TextInput size={InputSize::Small} class={class::SEARCH}
                                    label="New session name" placeholder="New session name"
                                    value={self.new_name.clone()} on_input={link.callback(Msg::NewName)} />
                                <Button size={ButtonSize::Small} submit=true
                                    disabled={!ready || self.new_name.trim().is_empty()}>
                                    {"Create"}
                                </Button>
                            </Toolbar>
                        </form>
                        {error}
                        <div class={class::SCROLL}>
                            <Table {head} columns={4} {empty} label="Sessions">{rows}</Table>
                        </div>
                        <form class={class::FORM} onsubmit={on_rename}>
                            <Toolbar label="Selected session" band=true chrome=true>
                                <TextInput size={InputSize::Small} class={class::SEARCH}
                                    label="Name of the selected session" placeholder="Session name"
                                    disabled={selected.is_none()}
                                    value={self.name.clone()} on_input={link.callback(Msg::Name)} />
                                <Button size={ButtonSize::Small} submit=true
                                    disabled={!ready || selected.is_none_or(|s| s.name == self.name || self.name.trim().is_empty())}>
                                    {"Rename"}
                                </Button>
                                <ToolbarSpacer />
                                <Button size={ButtonSize::Small} variant={ButtonVariant::Danger}
                                    title={if is_current { "The open session cannot be deleted" } else { "Delete the selected session and its watches" }}
                                    disabled={!ready || selected.is_none() || is_current}
                                    onclick={link.callback(|_| Msg::AskDelete)}>
                                    {"Delete"}
                                </Button>
                            </Toolbar>
                        </form>
                    </div>
                </Dialog>
                <Confirm open={props.open && self.confirm_delete}
                    title="Delete session?"
                    explanation={format!("The session {delete_name} and its watches and settings are deleted.")}
                    accept="Delete" danger=true busy={busy}
                    on_accept={link.callback(|()| Msg::Delete)}
                    on_cancel={link.callback(|()| Msg::CancelDelete)} />
            </>
        }
    }
}

fn time(seconds: u64) -> String {
    let date = js_sys::Date::new(&JsValue::from_f64(seconds as f64 * 1000.0));
    date.to_locale_string("default", &JsValue::UNDEFINED)
        .as_string()
        .unwrap_or_default()
}
