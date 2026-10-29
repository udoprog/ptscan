use yew::prelude::*;

use crate::error::ErrorEntry;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub errors: Vec<ErrorEntry>,
    pub on_clear: Callback<()>,
    pub on_close: Callback<()>,
}

#[function_component(ErrorLog)]
pub fn error_log(props: &Props) -> Html {
    let close = props.on_close.reform(|_: MouseEvent| ());
    let clear = props.on_clear.reform(|_: MouseEvent| ());
    let keep_open = Callback::from(|e: MouseEvent| e.stop_propagation());

    let entries = if props.errors.is_empty() {
        html! { <div class="empty muted">{"No errors"}</div> }
    } else {
        props
            .errors
            .iter()
            .rev()
            .map(|entry| {
                html! {
                    <column class="error-entry" style="gap: 2px">
                        <span class="muted">{&entry.time}</span>
                        <pre>{&entry.message}</pre>
                    </column>
                }
            })
            .collect::<Html>()
    };

    html! {
        <div class="backdrop" onclick={close.clone()}>
            <column class="panel dialog" style="gap: 0" onclick={keep_open}>
                <row class="toolbar center">
                    <span class="title">{"Errors"}</span>
                    <div class="fill" />
                    <button onclick={clear} disabled={props.errors.is_empty()}>{"Clear"}</button>
                    <button onclick={close}>{"Close"}</button>
                </row>
                <column class="list fill" style="gap: 0">{entries}</column>
            </column>
        </div>
    }
}
