use self::Orientation::*;
use crate::{prelude::*, ConnectDialog};

pub struct MainMenu;

impl MainMenu {
    pub fn new<A>(
        window: &ApplicationWindow,
        accel_group: &AccelGroup,
        attach: A,
        error: impl ErrorHandler,
    ) -> MenuBar
    where
        A: Fn(ptscan::ProcessId) + 'static,
    {
        let connect_window = ConnectDialog::new(attach, error);

        connect_window.hide();

        let weak_window = window.downgrade();

        let connect_window = connect_window.downgrade();

        let file_item = cascade! {
            MenuItem::new();
            ..add(&cascade! {
                gtk::Box::new(Horizontal, 0);
                ..pack_start(&Image::new_from_icon_name(Some("document-open"), IconSize::Menu), false, false, 0);
                ..pack_start(&Label::new(Some("Attach")), true, true, 0);
            });
            ..connect_activate(clone!(connect_window => move |_| {
                let connect_window = upgrade_weak!(connect_window);
                connect_window.show();
            }));
        };

        let about_item = cascade! {
            MenuItem::new_with_label("About");
            ..connect_activate(clone!(weak_window => move |_| {
                let window = upgrade_weak!(weak_window);
                cascade! {
                    AboutDialog::new();
                    ..set_website_label(Some("ptscan GitHub"));
                    ..set_website(Some("https://github.com/udoprog/ptscan"));
                    ..set_authors(&["John-John Tedro <udoprog@tedro.se>"]);
                    ..set_title("About ptscan");
                    ..set_transient_for(Some(&window));
                    ..run();
                    ..destroy();
                };
            }));
        };

        // `Primary` is `Ctrl` on Windows and Linux, and `command` on macOS
        // It isn't available directly through gdk::ModifierType, since it has
        // different values on different platforms.
        let (key, modifier) = gtk::accelerator_parse("<Primary>Q");

        let menu_bar = cascade! {
            MenuBar::new();
            ..append(&cascade! {
                MenuItem::new_with_label("File");
                ..set_submenu(Some(&cascade! {
                    Menu::new();
                    ..append(&file_item);
                    ..append(&about_item);
                    ..append(&cascade! {
                        MenuItem::new_with_label("Quit");
                        ..connect_activate(clone!(weak_window => move |_| {
                            let window = upgrade_weak!(weak_window);
                            window.destroy();
                        }));
                        ..add_accelerator("activate", accel_group, key, modifier, AccelFlags::VISIBLE);
                    });
                }));
            });
        };

        menu_bar
    }
}
