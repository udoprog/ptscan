//! Glyphs for files and folders, drawn like sockel's own, until sockel has
//! them.

use yew::prelude::*;

/// Paths on a 16 unit grid in `currentColor`, in sockel's glyph box.
pub fn glyph(paths: Html) -> Html {
    html! {
        <span class={sockel::class::GLYPH} aria-hidden="true">
            <svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.5"
                stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" focusable="false">
                {paths}
            </svg>
        </span>
    }
}

pub fn folder_glyph() -> Html {
    html! { <path d="M1.5 4.5a1 1 0 0 1 1-1h3.5l1.5 1.5h6a1 1 0 0 1 1 1v6.5a1 1 0 0 1-1 1h-11a1 1 0 0 1-1-1z" /> }
}

pub fn file_glyph() -> Html {
    html! { <><path d="M4 1.5h5l3.5 3.5v9a.5.5 0 0 1-.5.5H4a.5.5 0 0 1-.5-.5V2a.5.5 0 0 1 .5-.5z" /><path d="M9 1.5V5h3.5" /></> }
}

pub fn save_glyph() -> Html {
    html! { <><path d="M8 2v8M4.5 6.5L8 10l3.5-3.5" /><path d="M2.5 11v2.5h11V11" /></> }
}

pub fn up_glyph() -> Html {
    html! { <path d="M8 13V3M4 7l4-4 4 4" /> }
}

pub fn home_glyph() -> Html {
    html! { <path d="M2 7.5L8 2.5l6 5M3.5 6.5v7h3.5v-4h2v4h3.5v-7" /> }
}

pub fn data_glyph() -> Html {
    html! { <><ellipse cx="8" cy="3.5" rx="5.5" ry="2" /><path d="M2.5 3.5v9c0 1.1 2.5 2 5.5 2s5.5-.9 5.5-2v-9M2.5 8c0 1.1 2.5 2 5.5 2s5.5-.9 5.5-2" /></> }
}

pub fn drive_glyph() -> Html {
    html! { <><rect x="1.5" y="5" width="13" height="6" rx="1" /><path d="M11 8h1" /></> }
}
