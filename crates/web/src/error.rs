/// An error reported to the user, kept in the error log.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrorEntry {
    pub time: String,
    pub message: String,
}

impl ErrorEntry {
    pub fn now(message: String) -> Self {
        let time = js_sys::Date::new_0()
            .to_locale_time_string("default")
            .as_string()
            .unwrap_or_default();

        Self { time, message }
    }
}
