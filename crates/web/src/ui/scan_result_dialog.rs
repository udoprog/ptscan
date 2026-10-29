use musli_web::web03::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::HtmlInputElement;
use yew::prelude::*;

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
        let close = props.on_close.reform(|_: MouseEvent| ());
        let keep_open = Callback::from(|e: MouseEvent| e.stop_propagation());
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
                <span class="muted">
                    {format!("Added {} at {address} = {value}", entry.pointer)}
                </span>
            }
        });

        html! {
            <div class="backdrop" onclick={close.clone()}>
                <column class="panel dialog" style="gap: 0" onclick={keep_open}>
                    <row class="toolbar center">
                        <span class="title">{"Scan result"}</span>
                        <div class="fill" />
                        <button onclick={close}>{"Close"}</button>
                    </row>
                    <form class="details" {onsubmit}>
                        <span class="field-label">{"Address"}</span>
                        <span class="mono">{format!("{:#x}", result.address)}</span>
                        <span class="field-label">{"Last value"}</span>
                        <span class="mono">{&result.last}</span>
                        <span class="field-label">{"Current value"}</span>
                        <span class={classes!("mono", changed.then_some("changed"))}>{&result.current}</span>
                        <label class="field-label" for="result-pointer">{"Pointer"}</label>
                        <column style="gap: 2px">
                            <input id="result-pointer"
                                class={classes!("mono", self.pointer_error.is_some().then_some("invalid"))}
                                value={self.pointer.clone()}
                                oninput={link.callback(|e: InputEvent| Msg::Pointer(input_value(&e)))} />
                            if let Some(error) = &self.pointer_error {
                                <span class="field-error">{error}</span>
                            }
                        </column>
                        <label class="field-label" for="result-type">{"Type"}</label>
                        <column style="gap: 2px">
                            <input id="result-type"
                                class={classes!("mono", self.value_type_error.is_some().then_some("invalid"))}
                                value={self.value_type.clone()}
                                oninput={link.callback(|e: InputEvent| Msg::ValueType(input_value(&e)))} />
                            if let Some(error) = &self.value_type_error {
                                <span class="field-error">{error}</span>
                            }
                        </column>
                        <label class="field-label" for="result-value">{"Value"}</label>
                        <column style="gap: 2px">
                            <row class="center">
                                <input id="result-value"
                                    class={classes!("mono", "fill", self.value_error.is_some().then_some("invalid"))}
                                    value={self.value.clone()}
                                    oninput={link.callback(|e: InputEvent| Msg::Value(input_value(&e)))}
                                    onkeydown={link.batch_callback(|e: KeyboardEvent| {
                                        (e.key() == "Enter").then(|| {
                                            e.prevent_default();
                                            Msg::Write
                                        })
                                    })} />
                                <button type="button" title="Write the value as the type above at the address"
                                    disabled={self.write.is_pending()}
                                    onclick={link.callback(|_| Msg::Write)}>
                                    {"Write"}
                                </button>
                            </row>
                            if let Some(error) = &self.value_error {
                                <span class="field-error">{error}</span>
                            }
                            if let Some(written) = &self.written {
                                <span class="muted">{written}</span>
                            }
                        </column>
                        <span />
                        <row class="center">
                            <button type="submit" class="primary" disabled={self.request.is_pending()}>
                                {"Add to scratch"}
                            </button>
                            {added}
                        </row>
                    </form>
                </column>
            </div>
        }
    }
}

fn input_value(e: &InputEvent) -> String {
    e.target()
        .and_then(|t| t.dyn_into::<HtmlInputElement>().ok())
        .map(|input| input.value())
        .unwrap_or_default()
}
