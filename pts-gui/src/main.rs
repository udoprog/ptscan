use gio::prelude::*;
use pts_gui::MainWindow;

#[derive(Clone)]
struct ErrorHandler;

impl pts_gui::ErrorHandler for ErrorHandler {
    fn report<E>(&self, error: E)
    where
        E: Into<anyhow::Error>,
    {
        if true {
            return;
        }

        use std::io::Write as _;

        let stdout = std::io::stdout();
        let mut stdout = stdout.lock();

        let error: anyhow::Error = error.into();
        writeln!(stdout, "{}", error).expect("failed to write to stdout");

        for e in error.chain() {
            writeln!(stdout, "caused by: {}", e).expect("failed to write to stdout");
        }
    }
}

fn main() -> anyhow::Result<()> {
    let application = gtk::Application::new(Some("com.github.udoprog.ptscan"), Default::default())?;

    application.connect_activate(|app| {
        let error = ErrorHandler;
        MainWindow::build(app, error);
    });

    application.run(&std::env::args().collect::<Vec<_>>());
    Ok(())
}
