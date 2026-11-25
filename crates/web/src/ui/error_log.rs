use sockel::{Dialog, DialogSize, EmptyState, Stack};
use yew::prelude::*;

use crate::class;
use crate::error::ErrorEntry;
use crate::icons::AppIcon;
use crate::ui::actions::{Action, tool};

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
        html! { <EmptyState title="No errors" hint={html! { "Failed requests are listed here." }} /> }
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

    let head_actions = tool(
        &Action::new(AppIcon::Remove, "Clear the errors", clear).disabled(props.errors.is_empty()),
    );

    html! {
        <Dialog open={props.open} on_dismiss={close} title="Errors"
            size={DialogSize::WideTall} flush=true {head_actions}>
            <div class={classes!(class::SCROLL, props.errors.is_empty().then_some(class::CENTER))}>
                {entries}
            </div>
        </Dialog>
    }
}
