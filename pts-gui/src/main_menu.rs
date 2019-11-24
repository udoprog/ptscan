use self::Orientation::*;
use crate::prelude::*;

pub struct MainMenu;

impl MainMenu {
    pub fn new(
        window: glib::WeakRef<ApplicationWindow>,
        accel_group: &AccelGroup,
        connect_dialog_window: glib::WeakRef<Window>,
        error_dialog_window: glib::WeakRef<Window>,
    ) -> MenuBar {
        let file_item = cascade! {
            MenuItem::new();
            ..add(&cascade! {
                gtk::Box::new(Horizontal, 0);
                ..pack_start(&Image::new_from_icon_name(Some("document-open"), IconSize::Menu), false, false, 0);
                ..pack_start(&Label::new(Some("Attach")), true, true, 0);
            });
            ..connect_activate(clone!(connect_dialog_window => move |_| {
                let connect_dialog_window = upgrade_weak!(connect_dialog_window);
                connect_dialog_window.show();
                connect_dialog_window.present();
            }));
        };

        let error_dialog_item = cascade! {
            MenuItem::new();
            ..add(&cascade! {
                gtk::Box::new(Horizontal, 0);
                ..pack_start(&Image::new_from_icon_name(Some("document-open"), IconSize::Menu), false, false, 0);
                ..pack_start(&Label::new(Some("Show Errors")), true, true, 0);
            });
            ..connect_activate(clone!(error_dialog_window => move |_| {
                let error_dialog_window = upgrade_weak!(error_dialog_window);
                error_dialog_window.show();
                error_dialog_window.present();
            }));
        };

        let about_item = cascade! {
            MenuItem::new_with_label("About");
            ..connect_activate(clone!(window => move |_| {
                let window = upgrade_weak!(window);
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
                        ..connect_activate(clone!(window => move |_| {
                            let window = upgrade_weak!(window);
                            window.destroy();
                        }));
                        ..add_accelerator("activate", accel_group, key, modifier, AccelFlags::VISIBLE);
                    });
                }));
            });
            ..append(&cascade! {
                MenuItem::new_with_label("Help");
                ..set_submenu(Some(&cascade! {
                    Menu::new();
                    ..append(&error_dialog_item);
                }));
            });
            ..show_all();
        };

        menu_bar
    }
}
