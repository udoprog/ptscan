use musli_web::web03::prelude::*;
use sockel::{Button, ButtonVariant, Cluster, Dialog, DialogDismiss, FormRow, FormRows, TextInput};
use yew::prelude::*;

use crate::class;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub ws: ws::Handle,
    /// The result as of the latest page read, so its current value stays live.
    pub result: api::ScanResult,
    pub on_error: Callback<String>,
    pub on_close: Callback<()>,
}

pub enum Msg {
    Pointer(String),
    ValueType(String),
    Add,
    Added(Result<ws::Packet<api::AddScratch>, ws::Error>),
    Value(String),
    Write,
    Written(Result<ws::Packet<api::WriteScanValue>, ws::Error>),
}

pub struct ScanResultDialog {
    pointer: String,
    value_type: String,
    pointer_error: Option<String>,
    value_type_error: Option<String>,
    added: Option<api::ScratchEntry>,
    request: ws::Request,
    value: String,
    value_error: Option<String>,
    written: Option<String>,
    write: ws::Request,
}

impl Component for ScanResultDialog {
    type Message = Msg;
    type Properties = Props;

    fn create(ctx: &Context<Self>) -> Self {
        let result = &ctx.props().result;

        Self {
            pointer: result.pointer.clone(),
            value_type: result.value_type.clone(),
            pointer_error: None,
            value_type_error: None,
            added: None,
            request: ws::Request::default(),
            value: result.current.clone(),
            value_error: None,
            written: None,
            write: ws::Request::default(),
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::Pointer(text) => {
                self.pointer = text;
                self.pointer_error = None;
            }
            Msg::ValueType(text) => {
                self.value_type = text;
                self.value_type_error = None;
            }
            Msg::Add => {
                self.added = None;
                self.request = ctx
                    .props()
                    .ws
                    .request()
                    .body(api::AddScratchRequest {
                        pointer: self.pointer.clone(),
                        value_type: self.value_type.clone(),
                    })
                    .on_packet(ctx.link().callback(Msg::Added))
                    .send();
            }
            Msg::Added(result) => match result.and_then(|p| p.decode_response()) {
                Ok(response) => {
                    self.pointer_error = response.pointer_error;
                    self.value_type_error = response.value_type_error.or(response.value_error);
                    self.added = response.entry;

                    if let Some(error) = response.error {
                        ctx.props()
                            .on_error
                            .emit(format!("Adding to scratch: {error}"));
                    }
                }
                Err(error) => ctx
                    .props()
                    .on_error
                    .emit(format!("Adding to scratch: {error}")),
            },
            Msg::Value(text) => {
                self.value = text;
                self.value_error = None;
            }
            Msg::Write => {
                self.written = None;
                self.write = ctx
                    .props()
                    .ws
                    .request()
                    .body(api::WriteScanValueRequest {
                        address: ctx.props().result.address,
                        value_type: self.value_type.clone(),
                        value: self.value.clone(),
                    })
                    .on_packet(ctx.link().callback(Msg::Written))
                    .send();
            }
            Msg::Written(result) => match result.and_then(|p| p.decode_response()) {
                Ok(response) => {
                    self.value_type_error = response.value_type_error;
                    self.value_error = response.value_error;

                    if let Some(error) = response.error {
                        ctx.props().on_error.emit(format!("Writing value: {error}"));
                    } else if self.value_error.is_none() && self.value_type_error.is_none() {
                        let value = response.current.as_deref().unwrap_or("?");
                        self.written = Some(format!("Now {value}"));
                    }
                }
                Err(error) => ctx.props().on_error.emit(format!("Writing value: {error}")),
            },
        }

        true
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let props = ctx.props();
        let link = ctx.link();
        let result = &props.result;
        let close = props.on_close.reform(|_| ());
        let changed = result.current != result.last;

        let onsubmit = link.callback(|e: SubmitEvent| {
            e.prevent_default();
            Msg::Add
        });

        let added = self.added.as_ref().map(|entry| {
            let address = entry
                .address
                .map(|a| format!("{a:#x}"))
                .unwrap_or_else(|| String::from("?"));
            let value = entry.value.as_deref().unwrap_or("?");

            html! {
                <span class={class::MUTED}>
                    {format!("Added {} at {address} = {value}", entry.pointer)}
                </span>
            }
        });

        let error = |error: &Option<String>| error.as_ref().map(|e| html! { {e.clone()} });

        let on_write_key = link.batch_callback(|e: KeyboardEvent| {
            (e.key() == "Enter").then(|| {
                e.prevent_default();
                Msg::Write
            })
        });

        let actions = html! {
            <>
                {added}
                <Button submit=true form="scan-result-form" variant={ButtonVariant::Primary}
                    busy={self.request.is_pending()}>
                    {"Add to scratch"}
                </Button>
            </>
        };

        html! {
            <Dialog open=true on_dismiss={close} title="Scan result" dismiss={DialogDismiss::NoOutside} {actions}>
                <form id="scan-result-form" class={class::FORM} {onsubmit}>
                    <FormRows>
                        <FormRow id="result-address" group=true label={html! {"Address"}}>
                            <span class={class::MONO}>{format!("{:#x}", result.address)}</span>
                        </FormRow>
                        <FormRow id="result-last" group=true label={html! {"Last value"}}>
                            <span class={class::MONO}>{&result.last}</span>
                        </FormRow>
                        <FormRow id="result-current" group=true label={html! {"Current value"}}>
                            <span class={classes!(class::MONO, changed.then_some(class::CHANGED))}>{&result.current}</span>
                        </FormRow>
                        <FormRow id="result-pointer" label={html! {"Pointer"}} error={error(&self.pointer_error)}>
                            <TextInput id="result-pointer" class={class::MONO}
                                invalid={self.pointer_error.is_some()}
                                value={self.pointer.clone()}
                                on_input={link.callback(Msg::Pointer)} />
                        </FormRow>
                        <FormRow id="result-type" label={html! {"Type"}} error={error(&self.value_type_error)}>
                            <TextInput id="result-type" class={class::MONO}
                                invalid={self.value_type_error.is_some()}
                                value={self.value_type.clone()}
                                on_input={link.callback(Msg::ValueType)} />
                        </FormRow>
                        <FormRow id="result-value" label={html! {"Value"}} error={error(&self.value_error)}
                            hint={self.written.clone().map(|w| html! { {w} })}>
                            <Cluster nowrap=true>
                                <TextInput id="result-value" class={classes!(class::MONO, sockel::class::GROW)}
                                    invalid={self.value_error.is_some()}
                                    value={self.value.clone()}
                                    on_input={link.callback(Msg::Value)}
                                    on_keydown={on_write_key} />
                                <Button title="Write the value as the type above at the address"
                                    busy={self.write.is_pending()}
                                    onclick={link.callback(|_| Msg::Write)}>
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
