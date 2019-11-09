use crate::{scan::ScanProgress, Size, Token};

pub(crate) struct ProgressReporter<'token, 'err, P> {
    progress: P,
    /// Current progress.
    current: usize,
    /// Last percentage encountered.
    last_percentage: usize,
    /// Total.
    total: usize,
    /// Whether to report progress or not.
    token: &'token Token,
    /// Last error captured from the progress.
    last_err: &'err mut Option<anyhow::Error>,
}

impl<'token, 'err, P> ProgressReporter<'token, 'err, P> {
    /// Create a new reporter.
    pub(crate) fn new(
        progress: P,
        total: usize,
        token: &'token Token,
        last_err: &'err mut Option<anyhow::Error>,
    ) -> ProgressReporter<'token, 'err, P> {
        Self {
            progress,
            current: 0,
            last_percentage: 0,
            total,
            token,
            last_err,
        }
    }

    /// Evaluate the given result.
    pub(crate) fn eval<T>(&mut self, result: anyhow::Result<T>) -> Option<T> {
        match result {
            Ok(value) => Some(value),
            Err(e) => {
                self.token.set();
                *self.last_err = Some(e);
                None
            }
        }
    }

    /// Report a number of bytes.
    pub(crate) fn report_bytes(&mut self, bytes: Size)
    where
        P: ScanProgress,
    {
        if let Err(e) = self.progress.report_bytes(bytes) {
            *self.last_err = Some(e);
            self.token.set();
        }
    }

    /// Tick a single task.
    pub(crate) fn tick(&mut self, results: u64)
    where
        P: ScanProgress,
    {
        self.tick_n(1, results);
    }

    /// Tick a single task.
    pub(crate) fn tick_n(&mut self, count: usize, results: u64)
    where
        P: ScanProgress,
    {
        self.current += count;
        let p = (self.current * 100) / self.total;

        if p > self.last_percentage {
            if let Err(e) = self.progress.report(p, results) {
                *self.last_err = Some(e);
                self.token.set();
            }

            self.last_percentage = p;
        }
    }
}
