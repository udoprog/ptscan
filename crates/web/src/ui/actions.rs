use sockel::{Button, ButtonSize, ButtonVariant, ContextMenu, Menu, MenuItem};
use yew::prelude::*;

use crate::icons::{AppIcon, icon};

/// One thing a view lets its user do, drawn as a toolbar's icon button or a
/// menu's item from the same description.
#[derive(Clone, PartialEq)]
pub struct Action {
    pub icon: AppIcon,
    pub label: AttrValue,
    pub onclick: Callback<MouseEvent>,
    pub disabled: bool,
    pub danger: bool,
}

impl Action {
    pub fn new(icon: AppIcon, label: impl Into<AttrValue>, onclick: Callback<MouseEvent>) -> Self {
        Self {
            icon,
            label: label.into(),
            onclick,
            disabled: false,
            danger: false,
        }
    }

    pub fn disabled(mut self, disabled: bool) -> Self {
        self.disabled = disabled;
        self
    }

    pub fn danger(mut self) -> Self {
        self.danger = true;
        self
    }
}

/// A toolbar's icon button: the icon alone, named and tipped by its label.
pub fn tool(action: &Action) -> Html {
    html! {
        <Button size={ButtonSize::Small} variant={ButtonVariant::Quiet}
            icon={icon(action.icon)}
            label={action.label.clone()} tip={action.label.clone()}
            disabled={action.disabled} onclick={action.onclick.clone()} />
    }
}

/// The actions as menu items.
pub fn items(actions: &[Action]) -> Html {
    actions
        .iter()
        .map(|action| {
            html! {
                <MenuItem icon={icon(action.icon)} danger={action.danger}
                    disabled={action.disabled} onclick={action.onclick.clone()}>
                    {action.label.clone()}
                </MenuItem>
            }
        })
        .collect()
}

/// A toolbar's overflow menu holding the actions it has no room for.
pub fn more(label: &'static str, actions: &[Action]) -> Html {
    html! {
        <Menu icon={icon(AppIcon::More)} label={label} tip={label}
            size={ButtonSize::Small} variant={ButtonVariant::Quiet}>
            {items(actions)}
        </Menu>
    }
}

#[derive(Properties, PartialEq)]
pub struct RowMenuProps {
    pub label: AttrValue,
    pub actions: Vec<Action>,
    #[prop_or_default]
    pub children: Html,
}

/// The actions as a menu opened by a right-click (or long press) on the
/// rows it wraps.
#[function_component]
pub fn RowMenu(props: &RowMenuProps) -> Html {
    html! {
        <ContextMenu label={props.label.clone()} items={items(&props.actions)}>
            {props.children.clone()}
        </ContextMenu>
    }
}
