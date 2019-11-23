use gio::prelude::*;
use pts_gui::MainWindow;
use std::sync::Arc;

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
    let thread_pool = Arc::new(
        rayon::ThreadPoolBuilder::new()
            .num_threads(usize::max(num_cpus::get(), 4))
            .build()?,
    );

    let application = gtk::Application::new(Some("com.github.udoprog.ptscan"), Default::default())?;

    application.connect_activate(move |app| {
        let error = ErrorHandler;
        MainWindow::build(thread_pool.clone(), app, error);
    });

    application.run(&std::env::args().collect::<Vec<_>>());
    Ok(())
}
