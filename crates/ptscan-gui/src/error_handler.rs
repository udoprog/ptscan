pub trait ErrorHandler: 'static + Clone + Send + Sync {
    /// Report that the given error happened, but don't deal with it in any particular way.
    fn report<E>(&self, error: E)
    where
        E: Into<anyhow::Error>;
}
