use sockel::{Glyph, IconSet};
use yew::prelude::*;

/// The app's icons, drawn like sockel's own: strokes on a 16 unit grid in
/// `currentColor`.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum AppIcon {
    Scan,
    Stop,
    Undo,
    Redo,
    Reset,
    Refresh,
    Remove,
    Edit,
    Write,
    Target,
    Json,
    Options,
    Save,
    Load,
    Folder,
    File,
    Up,
    Home,
    Data,
    Drive,
    Add,
    Copy,
    Show,
    More,
    Attach,
    Detach,
    Open,
    Session,
    Process,
    Errors,
}

impl AppIcon {
    const ALL: [AppIcon; 26] = [
        AppIcon::Scan,
        AppIcon::Stop,
        AppIcon::Undo,
        AppIcon::Redo,
        AppIcon::Reset,
        AppIcon::Refresh,
        AppIcon::Remove,
        AppIcon::Edit,
        AppIcon::Write,
        AppIcon::Target,
        AppIcon::Json,
        AppIcon::Options,
        AppIcon::Attach,
        AppIcon::Detach,
        AppIcon::Open,
        AppIcon::Save,
        AppIcon::Load,
        AppIcon::Folder,
        AppIcon::File,
        AppIcon::Up,
        AppIcon::Home,
        AppIcon::Data,
        AppIcon::Drive,
        AppIcon::Session,
        AppIcon::Process,
        AppIcon::Errors,
    ];

    fn glyph(self) -> Option<Glyph> {
        match self {
            AppIcon::Add => Some(Glyph::Plus),
            AppIcon::Copy => Some(Glyph::Copy),
            AppIcon::Show => Some(Glyph::Eye),
            AppIcon::More => Some(Glyph::Ellipsis),
            _ => None,
        }
    }

    fn name(self) -> &'static str {
        match self {
            AppIcon::Scan => "scan",
            AppIcon::Stop => "stop",
            AppIcon::Undo => "undo",
            AppIcon::Redo => "redo",
            AppIcon::Reset => "reset",
            AppIcon::Refresh => "refresh",
            AppIcon::Remove => "remove",
            AppIcon::Edit => "edit",
            AppIcon::Write => "write",
            AppIcon::Target => "target",
            AppIcon::Json => "json",
            AppIcon::Options => "options",
            AppIcon::Save => "save",
            AppIcon::Load => "load",
            AppIcon::Folder => "folder",
            AppIcon::File => "file",
            AppIcon::Up => "up",
            AppIcon::Home => "home",
            AppIcon::Data => "data",
            AppIcon::Drive => "drive",
            AppIcon::Add => "add",
            AppIcon::Copy => "copy",
            AppIcon::Show => "show",
            AppIcon::More => "more",
            AppIcon::Attach => "attach",
            AppIcon::Detach => "detach",
            AppIcon::Open => "open",
            AppIcon::Session => "session",
            AppIcon::Process => "process",
            AppIcon::Errors => "errors",
        }
    }

    fn paths(self) -> Html {
        match self {
            AppIcon::Scan => html! { <path d="M5 3.5v9l7-4.5z" /> },
            AppIcon::Stop => html! { <rect x="4" y="4" width="8" height="8" rx="1.5" /> },
            AppIcon::Undo => html! { <path d="M6 3.5L3 6.5l3 3M3 6.5h6.5a3.5 3.5 0 0 1 0 7H7" /> },
            AppIcon::Redo => html! { <path d="M10 3.5l3 3-3 3M13 6.5H6.5a3.5 3.5 0 0 0 0 7H9" /> },
            AppIcon::Reset => html! { <path d="M3 8a5 5 0 1 0 1.5-3.5M3 2.5v3h3" /> },
            AppIcon::Refresh => html! { <path d="M13 8a5 5 0 1 1-1.5-3.5M13 2.5v3h-3" /> },
            AppIcon::Remove => {
                html! { <path d="M3 4.5h10M6.5 4.5V3h3v1.5M4.5 4.5l.75 8.5h5.5l.75-8.5" /> }
            }
            AppIcon::Edit => html! { <path d="M10.5 2.5l3 3-8 8h-3v-3z" /> },
            AppIcon::Write => html! { <path d="M8 2.5v8M4.5 7L8 10.5 11.5 7M3 13.5h10" /> },
            AppIcon::Target => html! {
                <><circle cx="8" cy="8" r="4.5" /><path d="M8 1.5v3M8 11.5v3M1.5 8h3M11.5 8h3" /></>
            },
            AppIcon::Json => html! {
                <path d="M6 2.5c-1.5 0-2 .5-2 2V6c0 1-.5 1.5-1.5 2 1 .5 1.5 1 1.5 2v1.5c0 1.5.5 2 2 2M10 2.5c1.5 0 2 .5 2 2V6c0 1 .5 1.5 1.5 2-1 .5-1.5 1-1.5 2v1.5c0 1.5-.5 2-2 2" />
            },
            AppIcon::Options => html! {
                <><path d="M2.5 4.5h11M2.5 11.5h11" />
                <circle cx="5.5" cy="4.5" r="1.75" fill="var(--surface)" />
                <circle cx="10.5" cy="11.5" r="1.75" fill="var(--surface)" /></>
            },
            AppIcon::Attach => html! {
                <path d="M6.5 9.5l3-3M7 4.5l1-1a2.5 2.5 0 0 1 3.5 3.5l-1 1M9 11.5l-1 1a2.5 2.5 0 0 1-3.5-3.5l1-1" />
            },
            AppIcon::Detach => html! {
                <path d="M7 4.5l1-1a2.5 2.5 0 0 1 3.5 3.5l-1 1M9 11.5l-1 1a2.5 2.5 0 0 1-3.5-3.5l1-1M2.5 2.5l2 2M13.5 13.5l-2-2" />
            },
            AppIcon::Open => html! { <path d="M9 2.5h4.5v11H9M2.5 8h7.5M7.5 5.5L10 8l-2.5 2.5" /> },
            AppIcon::Save => html! { <path d="M8 2v8M4.5 6.5L8 10l3.5-3.5M2.5 11v2.5h11V11" /> },
            AppIcon::Load => html! { <path d="M8 10V2M4.5 5.5L8 2l3.5 3.5M2.5 11v2.5h11V11" /> },
            AppIcon::Folder => html! {
                <path d="M1.5 4.5a1 1 0 0 1 1-1h3.5l1.5 1.5h6a1 1 0 0 1 1 1v6.5a1 1 0 0 1-1 1h-11a1 1 0 0 1-1-1z" />
            },
            AppIcon::File => html! {
                <path d="M4 1.5h5l3.5 3.5v9a.5.5 0 0 1-.5.5H4a.5.5 0 0 1-.5-.5V2a.5.5 0 0 1 .5-.5zM9 1.5V5h3.5" />
            },
            AppIcon::Up => html! { <path d="M8 13V3M4 7l4-4 4 4" /> },
            AppIcon::Home => html! { <path d="M2 7.5L8 2.5l6 5M3.5 6.5v7h3.5v-4h2v4h3.5v-7" /> },
            AppIcon::Data => html! {
                <><ellipse cx="8" cy="3.5" rx="5.5" ry="2" />
                <path d="M2.5 3.5v9c0 1.1 2.5 2 5.5 2s5.5-.9 5.5-2v-9M2.5 8c0 1.1 2.5 2 5.5 2s5.5-.9 5.5-2" /></>
            },
            AppIcon::Drive => {
                html! { <><rect x="1.5" y="5" width="13" height="6" rx="1" /><path d="M11 8h1" /></> }
            }
            // A stack of records: the saved sessions.
            AppIcon::Session => html! {
                <><ellipse cx="8" cy="4" rx="5" ry="1.75" />
                <path d="M3 4v8c0 1 2.25 1.75 5 1.75s5-.75 5-1.75V4M3 8c0 1 2.25 1.75 5 1.75S13 9 13 8" /></>
            },
            // A chip: the attached process.
            AppIcon::Process => html! {
                <><rect x="4" y="4" width="8" height="8" rx="1" />
                <path d="M6.5 1.5v2.5M9.5 1.5v2.5M6.5 12v2.5M9.5 12v2.5M1.5 6.5h2.5M1.5 9.5h2.5M12 6.5h2.5M12 9.5h2.5" /></>
            },
            AppIcon::Errors => html! {
                <><path d="M8 2l6 11H2z" /><path d="M8 6.5v3M8 11.25v.25" /></>
            },
            AppIcon::Add | AppIcon::Copy | AppIcon::Show | AppIcon::More => html! {},
        }
    }
}

/// The icon set the app provides at its root.
pub fn set() -> IconSet {
    AppIcon::ALL.into_iter().fold(IconSet::default(), |set, icon| {
        set.with_named(
            icon.name(),
            html! {
                <svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.5"
                    stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" focusable="false">
                    {icon.paths()}
                </svg>
            },
        )
    })
}

/// The icon, for a button's or menu item's `icon`.
pub fn icon(icon: AppIcon) -> Html {
    match icon.glyph() {
        Some(glyph) => html! { <sockel::Icon {glyph} /> },
        None => html! { <sockel::Icon name={icon.name()} /> },
    }
}
