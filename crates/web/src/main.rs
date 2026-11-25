mod app;
mod clipboard;
mod error;
mod icons;
mod ui;

pub mod class {
    include!(concat!(env!("OUT_DIR"), "/classes.rs"));
}

const STYLESHEET: &str = include_str!(concat!(env!("OUT_DIR"), "/style.css"));

#[yew::function_component]
fn Root() -> yew::Html {
    let icons = yew::use_memo((), |()| icons::set());

    yew::html! {
        <>
            <sockel::Styles css={STYLESHEET} />
            <yew::ContextProvider<sockel::IconSet> context={(*icons).clone()}>
                <sockel::OverlayHost>
                    <app::App />
                    <sockel::DataTips />
                </sockel::OverlayHost>
            </yew::ContextProvider<sockel::IconSet>>
        </>
    }
}

fn main() {
    console_error_panic_hook::set_once();
    yew::Renderer::<Root>::new().render();
}
