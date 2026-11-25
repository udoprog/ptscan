mod app;
mod clipboard;
mod error;
mod ui;

pub mod class {
    include!(concat!(env!("OUT_DIR"), "/classes.rs"));
}

const STYLESHEET: &str = include_str!(concat!(env!("OUT_DIR"), "/style.css"));

#[yew::function_component]
fn Root() -> yew::Html {
    yew::html! {
        <>
            <sockel::Styles css={STYLESHEET} />
            <sockel::OverlayHost>
                <app::App />
            </sockel::OverlayHost>
        </>
    }
}

fn main() {
    console_error_panic_hook::set_once();
    yew::Renderer::<Root>::new().render();
}
