use sockel::{Button, Dialog, DialogSize, EmptyState, Stack};
use yew::prelude::*;

use crate::class;
use crate::error::ErrorEntry;

#[derive(Properties, PartialEq)]
pub struct Props {
    /// The dialog is shown.
    pub open: bool,
    pub errors: Vec<ErrorEntry>,
    pub on_clear: Callback<()>,
    pub on_close: Callback<()>,
}

#[function_component(ErrorLog)]
pub fn error_log(props: &Props) -> Html {
    let close = props.on_close.reform(|_| ());
    let clear = props.on_clear.reform(|_: MouseEvent| ());

    let entries = if props.errors.is_empty() {
        html! { <EmptyState title="No errors" compact=true /> }
    } else {
        props
            .errors
            .iter()
            .rev()
            .map(|entry| {
                html! {
                    <Stack gap={sockel::Gap::Hair} class={class::ERROR_ENTRY}>
                        <span class={class::MUTED}>{&entry.time}</span>
                        <pre class={class::ERROR_MESSAGE}>{&entry.message}</pre>
                    </Stack>
                }
            })
            .collect::<Html>()
    };

    let actions = html! {
        <Button onclick={clear} disabled={props.errors.is_empty()}>{"Clear"}</Button>
    };

    html! {
        <Dialog open={props.open} on_dismiss={close} title="Errors"
            size={DialogSize::WideTall} flush=true {actions}>
            <div class={class::SCROLL}>{entries}</div>
        </Dialog>
    }
}
