use std::cell::RefCell;
use std::rc::Rc;

use musli_web::web03::prelude::*;
use sockel::{EntryKind, FileBrowserMode, ListRequest, Listing, ListingEntry, Place, PlaceKind};
use yew::prelude::*;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub ws: ws::Handle,
    pub open: bool,
    pub mode: FileBrowserMode,
    pub title: AttrValue,
    /// The file or directory it opens at; the server's home when empty.
    #[prop_or_default]
    pub path: AttrValue,
    /// The file name a save in the home directory starts with.
    #[prop_or_default]
    pub name: AttrValue,
    #[prop_or_default]
    pub extensions: Vec<AttrValue>,
    /// The chosen file's absolute path.
    pub on_choose: Callback<String>,
    pub on_close: Callback<()>,
    #[prop_or_default]
    pub testid: Option<AttrValue>,
}

/// A file on the server's machine, chosen by browsing the directories it
/// lists.
#[function_component]
pub fn FileBrowser(props: &Props) -> Html {
    let request = use_mut_ref(ws::Request::default);

    let name = (props.mode == FileBrowserMode::Save && !props.name.is_empty())
        .then(|| props.name.to_string());

    let list = list(props.ws.clone(), request, name);

    let on_choose = {
        let on_choose = props.on_choose.clone();
        let on_close = props.on_close.clone();

        Callback::from(move |path: String| {
            on_choose.emit(path);
            on_close.emit(());
        })
    };

    html! {
        <sockel::FileBrowser open={props.open} mode={props.mode} title={props.title.clone()}
            path={props.path.clone()} extensions={props.extensions.clone()} {list} {on_choose}
            on_dismiss={props.on_close.clone()} testid={props.testid.clone()} />
    }
}

/// Answers each request through the server; `name` is the file a listing of
/// the home directory names.
fn list(
    ws: ws::Handle,
    request: Rc<RefCell<ws::Request>>,
    name: Option<String>,
) -> Callback<ListRequest> {
    Callback::from(move |list: ListRequest| {
        let home = list.path.is_empty();
        let name = name.clone().filter(|_| home);
        let reply = list.reply;

        let body = api::ListDirectoryRequest {
            path: (!home).then_some(list.path),
            nearest: list.nearest,
        };

        let on_packet = Callback::from(
            move |result: Result<ws::Packet<api::ListDirectory>, ws::Error>| {
                let listed = result
                    .and_then(|packet| packet.decode_response())
                    .map_err(|error| format!("Listing the folder: {error}"))
                    .and_then(|response| listing(response, name.clone()));

                reply.emit(listed);
            },
        );

        *request.borrow_mut() = ws.request().body(body).on_packet(on_packet).send();
    })
}

fn listing(response: api::ListDirectoryResponse, name: Option<String>) -> Result<Listing, String> {
    if let Some(refusal) = response.refusal {
        let reason = match refusal {
            api::ListDirectoryRefusal::Relative => "Not an absolute path",
            api::ListDirectoryRefusal::Missing => "This folder does not exist",
            api::ListDirectoryRefusal::NotADirectory => "Not a folder",
            api::ListDirectoryRefusal::Unreadable => "This folder cannot be read",
        };

        return Err(format!("{reason}: {}", response.path));
    }

    let entries = response
        .entries
        .into_iter()
        .map(|entry| ListingEntry {
            name: entry.name,
            kind: match entry.kind {
                api::EntryKind::Directory => EntryKind::Directory,
                api::EntryKind::File => EntryKind::File,
            },
            size: entry.size,
        })
        .collect();

    let places = response
        .places
        .into_iter()
        .filter_map(|place| {
            let kind = match place.kind {
                api::PlaceKind::Home => PlaceKind::Home,
                api::PlaceKind::Root => PlaceKind::Root,
                api::PlaceKind::Data => return None,
            };

            Some(Place::new(kind, place.path))
        })
        .collect();

    Ok(Listing {
        path: response.path,
        parent: response.parent,
        separator: response.separator,
        entries,
        file: response.file.or(name),
        places,
        notice: None,
    })
}

pub(crate) fn file_name(path: &str) -> &str {
    path.rsplit(['/', '\\']).next().unwrap_or(path)
}

/// Why a file of `kind`, such as "a scratch list", was not saved or loaded.
pub(crate) fn refused(
    what: &str,
    name: &str,
    kind: &str,
    refusal: api::FileRefusal,
    error: Option<String>,
) -> String {
    let other;

    let reason = match refusal {
        api::FileRefusal::Relative => "not an absolute path",
        api::FileRefusal::Missing => "it does not exist",
        api::FileRefusal::Directory => "it is a folder",
        api::FileRefusal::Denied => "permission denied",
        api::FileRefusal::TooLarge => "the file is too large",
        api::FileRefusal::Malformed => "not a ptscan file, or cut short",
        api::FileRefusal::WrongKind => {
            other = format!("it holds something other than {kind}");
            other.as_str()
        }
        api::FileRefusal::UnsupportedVersion => "written by a newer ptscan",
        api::FileRefusal::Failed => "it failed",
    };

    match error {
        Some(error) => format!("Could not {what} {name}: {reason} ({error})"),
        None => format!("Could not {what} {name}: {reason}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn response() -> api::ListDirectoryResponse {
        api::ListDirectoryResponse {
            path: "/home/me".into(),
            parent: Some("/home".into()),
            entries: vec![api::DirectoryEntry {
                name: "a.ptscan.json".into(),
                kind: api::EntryKind::File,
                size: Some(2),
            }],
            file: None,
            places: vec![
                api::Place {
                    kind: api::PlaceKind::Home,
                    name: "Home".into(),
                    path: "/home/me".into(),
                },
                api::Place {
                    kind: api::PlaceKind::Data,
                    name: "ptscan data".into(),
                    path: "/home/me/.local/share/ptscan".into(),
                },
            ],
            separator: "/".into(),
            refusal: None,
        }
    }

    #[test]
    fn lists_the_response() {
        let listed = listing(response(), Some("new.ptscan.json".into())).unwrap();
        assert_eq!(listed.path, "/home/me");
        assert_eq!(listed.file.as_deref(), Some("new.ptscan.json"));
        assert_eq!(listed.entries[0].kind, EntryKind::File);
        let kinds = listed.places.iter().map(|p| p.kind).collect::<Vec<_>>();
        assert_eq!(kinds, [PlaceKind::Home]);
    }

    #[test]
    fn the_server_names_the_file_first() {
        let mut response = response();
        response.file = Some("a.ptscan.json".into());
        let listed = listing(response, Some("new.ptscan.json".into())).unwrap();
        assert_eq!(listed.file.as_deref(), Some("a.ptscan.json"));
    }

    #[test]
    fn refusals_are_errors() {
        let mut response = response();
        response.refusal = Some(api::ListDirectoryRefusal::Missing);
        assert_eq!(
            listing(response, None),
            Err("This folder does not exist: /home/me".to_owned())
        );
    }

    #[test]
    fn names_the_file_of_a_path() {
        assert_eq!(file_name("/home/me/a.json"), "a.json");
        assert_eq!(file_name("C:\\x\\b.json"), "b.json");
        assert_eq!(file_name("c"), "c");
    }
}
