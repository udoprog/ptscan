use std::sync::atomic::{AtomicUsize, Ordering};

use musli_web::web03::prelude::*;
use sockel::{
    Button, ButtonSize, ButtonVariant, Checkbox, Confirm, Dialog, DialogSize, EmptyState,
    InputSize, NavItem, NavList, PickRow, TextInput,
};
use wasm_bindgen::{JsCast, JsValue};
use web_sys::HtmlInputElement;
use yew::prelude::*;

use crate::class;
use crate::icons::{AppIcon, icon};

/// What a [`FileBrowser`] picks.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FileMode {
    /// An existing file.
    Open,
    /// A file name in a directory, asking before replacing one.
    Save,
}

#[derive(Properties, PartialEq)]
pub struct Props {
    pub ws: ws::Handle,
    pub open: bool,
    pub mode: FileMode,
    pub title: AttrValue,
    /// The directory it opens in; the server's home when `None`.
    #[prop_or_default]
    pub path: Option<AttrValue>,
    /// The file name a save starts with.
    #[prop_or_default]
    pub name: AttrValue,
    /// File name endings shown, such as `.ptscan.json`; every file when
    /// empty. A save without one gets the first.
    #[prop_or_default]
    pub extensions: Vec<AttrValue>,
    /// The chosen file's absolute path.
    pub on_choose: Callback<String>,
    pub on_close: Callback<()>,
    #[prop_or_default]
    pub testid: Option<AttrValue>,
}

pub enum Msg {
    Navigate(Option<String>),
    Listed(Result<ws::Packet<api::ListDirectory>, ws::Error>),
    Click(usize),
    Activate(usize),
    Name(String),
    AllFiles(bool),
    Hidden(bool),
    Key(KeyboardEvent),
    Choose,
    Replace,
    CancelReplace,
}

/// A file on the server's machine, chosen by browsing to it: places down the
/// side, the path as crumbs, then the directory's entries.
pub struct FileBrowser {
    listing: Option<api::ListDirectoryResponse>,
    /// The active row, an index into [`FileBrowser::visible`].
    active: Option<usize>,
    /// Focus the active row once rendered.
    focus: bool,
    name: String,
    all_files: bool,
    hidden: bool,
    replacing: Option<String>,
    error: Option<String>,
    id: String,
    request: ws::Request,
}

impl FileBrowser {
    fn navigate(&mut self, ctx: &Context<Self>, path: Option<String>) {
        self.request = ctx
            .props()
            .ws
            .request()
            .body(api::ListDirectoryRequest { path })
            .on_packet(ctx.link().callback(Msg::Listed))
            .send();
    }

    fn visible<'a>(&'a self, ctx: &'a Context<Self>) -> Vec<&'a api::DirectoryEntry> {
        let extensions = &ctx.props().extensions;

        self.listing
            .iter()
            .flat_map(|listing| listing.entries.iter())
            .filter(|entry| self.hidden || !entry.name.starts_with('.'))
            .filter(|entry| {
                entry.kind == api::EntryKind::Directory
                    || self.all_files
                    || matches_extension(&entry.name, extensions)
            })
            .collect()
    }

    /// The path a save writes to, when the name is usable.
    fn save_path(&self, ctx: &Context<Self>) -> Option<String> {
        let listing = self.listing.as_ref().filter(|l| l.refusal.is_none())?;
        let name = with_extension(self.name.trim(), &ctx.props().extensions)?;
        Some(join(&listing.path, &listing.separator, &name))
    }

    /// What Open or Save would choose now.
    fn chosen(&self, ctx: &Context<Self>) -> Option<String> {
        match ctx.props().mode {
            FileMode::Open => {
                let visible = self.visible(ctx);
                let entry = visible.get(self.active?)?;
                (entry.kind == api::EntryKind::File).then(|| entry.path.clone())
            }
            FileMode::Save => self.save_path(ctx),
        }
    }

    /// Choose `path`, asking first when a save would replace a file.
    fn choose(&mut self, ctx: &Context<Self>, path: String) {
        let props = ctx.props();

        let existing = self
            .listing
            .iter()
            .flat_map(|listing| listing.entries.iter())
            .find(|entry| entry.path == path);

        match (props.mode, existing.map(|entry| entry.kind)) {
            (FileMode::Save, Some(api::EntryKind::Directory)) => {
                self.error = Some("A folder has that name".to_owned());
            }
            (FileMode::Save, Some(api::EntryKind::File)) => {
                self.replacing = Some(path);
            }
            _ => {
                props.on_choose.emit(path);
                props.on_close.emit(());
            }
        }
    }

    fn activate(&mut self, ctx: &Context<Self>, index: usize) {
        let Some((kind, path, name)) = self
            .visible(ctx)
            .get(index)
            .map(|e| (e.kind, e.path.clone(), e.name.clone()))
        else {
            return;
        };

        match (kind, ctx.props().mode) {
            (api::EntryKind::Directory, _) => self.navigate(ctx, Some(path)),
            (api::EntryKind::File, FileMode::Open) => self.choose(ctx, path),
            (api::EntryKind::File, FileMode::Save) => {
                self.name = name;
                self.choose(ctx, path);
            }
        }
    }

    fn key(&mut self, ctx: &Context<Self>, e: KeyboardEvent) -> bool {
        let in_input = e
            .target()
            .is_some_and(|target| target.dyn_into::<HtmlInputElement>().is_ok());

        if in_input {
            if e.key() == "Enter" {
                e.prevent_default();

                if let Some(path) = self.chosen(ctx) {
                    self.choose(ctx, path);
                }

                return true;
            }

            return false;
        }

        let count = self.visible(ctx).len();

        let step = |by: isize| match self.active {
            Some(at) => at.saturating_add_signed(by).min(count.saturating_sub(1)),
            None => 0,
        };

        let active = match e.key().as_str() {
            "ArrowDown" => step(1),
            "ArrowUp" => step(-1),
            "Home" => 0,
            "End" => count.saturating_sub(1),
            "Enter" => {
                if let Some(at) = self.active {
                    e.prevent_default();
                    self.activate(ctx, at);
                }

                return true;
            }
            "Backspace" => {
                e.prevent_default();
                let parent = self.listing.as_ref().and_then(|l| l.parent.clone());

                if parent.is_some() {
                    self.navigate(ctx, parent);
                }

                return false;
            }
            _ => return false,
        };

        if count == 0 {
            return false;
        }

        e.prevent_default();
        self.active = Some(active);
        self.focus = true;
        true
    }

    fn row_id(&self, index: usize) -> String {
        format!("{}-{index}", self.id)
    }

    fn crumbs(&self, ctx: &Context<Self>) -> Html {
        let link = ctx.link();

        let Some(listing) = &self.listing else {
            return html! {};
        };

        let up = listing.parent.clone();
        let disabled = up.is_none();

        let crumbs = listing.crumbs.iter().enumerate().map(|(n, crumb)| {
            let path = crumb.path.clone();

            html! {
                <>
                    if n > 0 {
                        <span class={class::FILE_BROWSER_CRUMB_SEPARATOR} aria-hidden="true">{"›"}</span>
                    }
                    <Button size={ButtonSize::Small} variant={ButtonVariant::Quiet}
                        onclick={link.callback(move |_| Msg::Navigate(Some(path.clone())))}>
                        {crumb.name.clone()}
                    </Button>
                </>
            }
        });

        html! {
            <nav class={class::FILE_BROWSER_CRUMBS} aria-label="Path">
                <Button size={ButtonSize::Small} variant={ButtonVariant::Quiet}
                    icon={icon(AppIcon::Up)} {disabled} title="Parent folder"
                    onclick={link.callback(move |_| Msg::Navigate(up.clone()))} />
                {for crumbs}
            </nav>
        }
    }

    fn places(&self, ctx: &Context<Self>) -> Html {
        let Some(listing) = &self.listing else {
            return html! {};
        };

        let items = listing
            .places
            .iter()
            .map(|place| {
                let kind = match place.kind {
                    api::PlaceKind::Home => AppIcon::Home,
                    api::PlaceKind::Data => AppIcon::Data,
                    api::PlaceKind::Root => AppIcon::Drive,
                };

                NavItem::new(place.path.clone(), place.name.clone()).icon(icon(kind))
            })
            .collect::<Vec<_>>();

        let value = listing
            .places
            .iter()
            .find(|place| place.path == listing.path)
            .map(|place| AttrValue::from(place.path.clone()));

        let on_select = ctx
            .link()
            .callback(|path: AttrValue| Msg::Navigate(Some(path.to_string())));

        html! {
            <NavList class={class::FILE_BROWSER_PLACES} label="Places" {items} {value} {on_select} />
        }
    }

    fn entries(&self, ctx: &Context<Self>) -> Html {
        let link = ctx.link();

        let Some(listing) = &self.listing else {
            return html! { <EmptyState title="Reading…" compact=true /> };
        };

        if let Some(refusal) = listing.refusal {
            let title = match refusal {
                api::ListDirectoryRefusal::Relative => "Not an absolute path",
                api::ListDirectoryRefusal::Missing => "This folder does not exist",
                api::ListDirectoryRefusal::NotADirectory => "Not a folder",
                api::ListDirectoryRefusal::Unreadable => "This folder cannot be read",
            };

            return html! { <EmptyState {title} hint={html! { {listing.path.clone()} }} compact=true /> };
        }

        let visible = self.visible(ctx);

        if visible.is_empty() {
            let title = if listing.entries.is_empty() {
                "Empty folder"
            } else {
                "No matching files"
            };

            return html! { <EmptyState {title} compact=true /> };
        }

        visible
            .iter()
            .enumerate()
            .map(|(index, entry)| {
                let directory = entry.kind == api::EntryKind::Directory;
                let mark = icon(if directory { AppIcon::Folder } else { AppIcon::File });

                let detail = (!directory).then(|| {
                    let size = entry.size.map(format_size).unwrap_or_default();
                    let modified = entry.modified.map(format_time).unwrap_or_default();
                    html! { <>{size}{" · "}{modified}</> }
                });

                html! {
                    <div id={self.row_id(index)} key={entry.path.clone()}
                        ondblclick={(!directory).then(|| link.callback(move |_| Msg::Activate(index)))}>
                        <PickRow class={class::FILE_BROWSER_ENTRY}
                            name={html! { {entry.name.clone()} }} {detail} mark={Some(mark)}
                            current={self.active == Some(index)}
                            onclick={link.callback(move |_| Msg::Click(index))} />
                    </div>
                }
            })
            .collect()
    }
}

static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

impl Component for FileBrowser {
    type Message = Msg;
    type Properties = Props;

    fn create(ctx: &Context<Self>) -> Self {
        let mut this = Self {
            listing: None,
            active: None,
            focus: false,
            name: ctx.props().name.to_string(),
            all_files: false,
            hidden: false,
            replacing: None,
            error: None,
            id: format!("file-browser-{}", NEXT_ID.fetch_add(1, Ordering::Relaxed)),
            request: ws::Request::default(),
        };

        if ctx.props().open {
            this.navigate(ctx, ctx.props().path.as_ref().map(|p| p.to_string()));
        }

        this
    }

    fn changed(&mut self, ctx: &Context<Self>, old: &Self::Properties) -> bool {
        let props = ctx.props();

        if props.open && !old.open {
            self.name = props.name.to_string();
            self.replacing = None;
            self.error = None;
            self.navigate(ctx, props.path.as_ref().map(|p| p.to_string()));
        }

        true
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::Navigate(path) => {
                self.navigate(ctx, path);
                false
            }
            Msg::Listed(result) => {
                match result.and_then(|packet| packet.decode_response()) {
                    Ok(listing) => {
                        self.listing = Some(listing);
                        self.active = None;
                        self.error = None;
                    }
                    Err(error) => self.error = Some(format!("Listing the folder: {error}")),
                }

                true
            }
            Msg::Click(index) => {
                self.active = Some(index);

                if let Some(entry) = self.visible(ctx).get(index) {
                    match (entry.kind, ctx.props().mode) {
                        (api::EntryKind::Directory, _) => {
                            let path = entry.path.clone();
                            self.navigate(ctx, Some(path));
                        }
                        (api::EntryKind::File, FileMode::Save) => {
                            self.name = entry.name.clone();
                        }
                        (api::EntryKind::File, FileMode::Open) => {}
                    }
                }

                true
            }
            Msg::Activate(index) => {
                self.activate(ctx, index);
                true
            }
            Msg::Name(name) => {
                self.name = name;
                self.error = None;
                true
            }
            Msg::AllFiles(all_files) => {
                self.all_files = all_files;
                self.active = None;
                true
            }
            Msg::Hidden(hidden) => {
                self.hidden = hidden;
                self.active = None;
                true
            }
            Msg::Key(e) => self.key(ctx, e),
            Msg::Choose => {
                if let Some(path) = self.chosen(ctx) {
                    self.choose(ctx, path);
                }

                true
            }
            Msg::Replace => {
                if let Some(path) = self.replacing.take() {
                    ctx.props().on_choose.emit(path);
                    ctx.props().on_close.emit(());
                }

                true
            }
            Msg::CancelReplace => {
                self.replacing = None;
                true
            }
        }
    }

    fn rendered(&mut self, _: &Context<Self>, _: bool) {
        if !std::mem::take(&mut self.focus) {
            return;
        }

        let row = self.active.map(|at| self.row_id(at)).and_then(|id| {
            web_sys::window()?
                .document()?
                .get_element_by_id(&id)?
                .first_element_child()?
                .dyn_into::<web_sys::HtmlElement>()
                .ok()
        });

        if let Some(row) = row {
            _ = row.focus();
            row.scroll_into_view_with_bool(false);
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let props = ctx.props();
        let link = ctx.link();
        let testid = props.testid.clone();

        let chosen = self.chosen(ctx);

        let (accept, mode_icon) = match props.mode {
            FileMode::Open => ("Open", AppIcon::File),
            FileMode::Save => ("Save", AppIcon::Save),
        };

        let filtered = !props.extensions.is_empty();

        let actions_start = html! {
            <>
                if filtered {
                    <Checkbox checked={self.all_files}
                        on_change={link.callback(Msg::AllFiles)}>{"All files"}</Checkbox>
                }
                <Checkbox checked={self.hidden}
                    on_change={link.callback(Msg::Hidden)}>{"Hidden"}</Checkbox>
            </>
        };

        let actions = html! {
            <>
                <Button onclick={props.on_close.reform(|_| ())}>{"Cancel"}</Button>
                <Button variant={ButtonVariant::Primary} disabled={chosen.is_none()}
                    title={chosen.clone().map(AttrValue::from)}
                    onclick={link.callback(|_| Msg::Choose)}>{accept}</Button>
            </>
        };

        let save = (props.mode == FileMode::Save).then(|| {
            html! {
                <div class={class::FILE_BROWSER_NAME}>
                    <TextInput size={InputSize::Small} label="File name" placeholder="File name"
                        value={self.name.clone()} invalid={self.error.is_some()}
                        on_input={link.callback(Msg::Name)} />
                </div>
            }
        });

        let error = self.error.as_ref().map(|error| {
            html! { <p class={class::FILE_BROWSER_ERROR} role="alert">{error.clone()}</p> }
        });

        let replace_name = self
            .replacing
            .as_deref()
            .map(|path| file_name(path).to_owned())
            .unwrap_or_default();

        html! {
            <>
                <Dialog open={props.open} on_dismiss={props.on_close.reform(|_| ())}
                    title={props.title.clone()} icon={icon(mode_icon)} {testid}
                    size={DialogSize::WideTall} flush=true {actions} {actions_start}
                    onkeydown={link.callback(Msg::Key)}>
                    <div class={class::FILE_BROWSER}>
                        {self.places(ctx)}
                        <div class={class::FILE_BROWSER_MAIN}>
                            {self.crumbs(ctx)}
                            <div class={class::FILE_BROWSER_LIST} role="listbox" aria-label="Entries">
                                {self.entries(ctx)}
                            </div>
                            {save}
                            {error}
                        </div>
                    </div>
                </Dialog>
                <Confirm open={self.replacing.is_some()} title="Replace the file?"
                    explanation={format!("{replace_name} already exists. Saving replaces it.")}
                    accept="Replace" danger=true
                    on_accept={link.callback(|()| Msg::Replace)}
                    on_cancel={link.callback(|()| Msg::CancelReplace)} />
            </>
        }
    }
}

fn matches_extension(name: &str, extensions: &[AttrValue]) -> bool {
    if extensions.is_empty() {
        return true;
    }

    let name = name.to_lowercase();

    extensions
        .iter()
        .any(|extension| name.ends_with(&extension.to_lowercase()))
}

/// The name a save writes, with the first extension when it has none of
/// them, or `None` when it cannot name a file in the directory.
fn with_extension(name: &str, extensions: &[AttrValue]) -> Option<String> {
    if name.is_empty() || name == "." || name == ".." || name.contains(['/', '\\']) {
        return None;
    }

    match extensions.first() {
        Some(first) if !matches_extension(name, extensions) => Some(format!("{name}{first}")),
        _ => Some(name.to_owned()),
    }
}

fn join(directory: &str, separator: &str, name: &str) -> String {
    if directory.ends_with(separator) {
        format!("{directory}{name}")
    } else {
        format!("{directory}{separator}{name}")
    }
}

fn file_name(path: &str) -> &str {
    path.rsplit(['/', '\\']).next().unwrap_or(path)
}

fn format_size(bytes: u64) -> String {
    const UNITS: [&str; 4] = ["KB", "MB", "GB", "TB"];

    if bytes < 1024 {
        return format!("{bytes} B");
    }

    let mut value = bytes as f64 / 1024.0;
    let mut unit = "KB";

    for next in UNITS.iter().skip(1) {
        if value < 1024.0 {
            break;
        }

        value /= 1024.0;
        unit = next;
    }

    format!("{value:.1} {unit}")
}

fn format_time(seconds: u64) -> String {
    let date = js_sys::Date::new(&JsValue::from_f64(seconds as f64 * 1000.0));
    String::from(date.to_locale_string("default", &JsValue::UNDEFINED))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn exts(list: &[&'static str]) -> Vec<AttrValue> {
        list.iter().map(|e| AttrValue::Static(e)).collect()
    }

    #[test]
    fn filters_by_extension_case_insensitively() {
        let json = exts(&[".ptscan.json"]);
        assert!(matches_extension("a.ptscan.json", &json));
        assert!(matches_extension("A.PTSCAN.JSON", &json));
        assert!(!matches_extension("a.json", &json));
        assert!(matches_extension("anything", &[]));
    }

    #[test]
    fn save_names_get_the_first_extension() {
        let json = exts(&[".ptscan.json", ".json"]);
        assert_eq!(
            with_extension("list", &json).as_deref(),
            Some("list.ptscan.json")
        );
        assert_eq!(
            with_extension("list.json", &json).as_deref(),
            Some("list.json")
        );
        assert_eq!(with_extension("list", &[]).as_deref(), Some("list"));
        assert_eq!(with_extension("", &json), None);
        assert_eq!(with_extension("..", &json), None);
        assert_eq!(with_extension("a/b", &json), None);
        assert_eq!(with_extension("a\\b", &json), None);
    }

    #[test]
    fn joins_onto_the_directory() {
        assert_eq!(join("/home/me", "/", "a"), "/home/me/a");
        assert_eq!(join("/", "/", "a"), "/a");
        assert_eq!(join("C:\\", "\\", "a"), "C:\\a");
        assert_eq!(join("C:\\Users", "\\", "a"), "C:\\Users\\a");
    }

    #[test]
    fn names_the_file_of_a_path() {
        assert_eq!(file_name("/home/me/a.json"), "a.json");
        assert_eq!(file_name("C:\\x\\b.json"), "b.json");
        assert_eq!(file_name("c"), "c");
    }

    #[test]
    fn formats_sizes() {
        assert_eq!(format_size(0), "0 B");
        assert_eq!(format_size(1023), "1023 B");
        assert_eq!(format_size(1536), "1.5 KB");
        assert_eq!(format_size(5 * 1024 * 1024), "5.0 MB");
    }
}
