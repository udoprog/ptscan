use sockel::{Toolbar, ToolbarSpacer};
use yew::prelude::*;

use crate::class;

#[derive(Properties, PartialEq)]
pub struct PaneProps {
    /// What the pane holds, for assistive technology.
    pub label: AttrValue,
    #[prop_or_default]
    pub children: Html,
}

/// One region of the main screen: a flat surface its head, bars and
/// scrolling content stack in.
#[function_component]
pub fn Pane(props: &PaneProps) -> Html {
    html! {
        <section class={class::PANE} aria-label={props.label.clone()}>
            {props.children.clone()}
        </section>
    }
}

#[derive(Properties, PartialEq)]
pub struct PaneHeadProps {
    pub title: AttrValue,
    /// Muted detail beside the title, such as a count.
    #[prop_or_default]
    pub meta: Option<AttrValue>,
    /// The pane's actions, at the head's end.
    #[prop_or_default]
    pub children: Html,
}

/// A pane's title and its primary actions.
#[function_component]
pub fn PaneHead(props: &PaneHeadProps) -> Html {
    html! {
        <Toolbar label={props.title.clone()} band=true class={class::PANE_HEAD}>
            <h2 class={class::PANE_TITLE}>{props.title.clone()}</h2>
            if let Some(meta) = &props.meta {
                <span class={classes!(class::PANE_META, sockel::class::NUMERIC, sockel::class::TRUNCATE)}>
                    {meta.clone()}
                </span>
            }
            <ToolbarSpacer />
            {props.children.clone()}
        </Toolbar>
    }
}

#[derive(Properties, PartialEq)]
pub struct PaneBarProps {
    pub label: AttrValue,
    #[prop_or_default]
    pub children: Html,
}

/// A secondary row of a pane's controls, ruled off from what follows.
#[function_component]
pub fn PaneBar(props: &PaneBarProps) -> Html {
    html! {
        <Toolbar label={props.label.clone()} band=true class={class::PANE_BAR}>
            {props.children.clone()}
        </Toolbar>
    }
}

/// How a status line reads.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum StatusTone {
    Neutral,
    Warning,
    Danger,
}

#[derive(Properties, PartialEq)]
pub struct StatusLineProps {
    #[prop_or(StatusTone::Neutral)]
    pub tone: StatusTone,
    /// Shown before the text, such as a progress bar.
    #[prop_or_default]
    pub lead: Option<Html>,
    pub text: AttrValue,
}

/// One quiet line saying how the pane's last work went or how far the
/// current work is.
#[function_component]
pub fn StatusLine(props: &StatusLineProps) -> Html {
    let tone = match props.tone {
        StatusTone::Neutral => None,
        StatusTone::Warning => Some(class::WARNING),
        StatusTone::Danger => Some(class::DANGER),
    };

    html! {
        <div class={classes!(class::STATUS_LINE, tone)} role="status">
            {for props.lead.clone()}
            <span class={classes!(class::STATUS_TEXT, sockel::class::TRUNCATE)} title={props.text.clone()}>
                {props.text.clone()}
            </span>
        </div>
    }
}
