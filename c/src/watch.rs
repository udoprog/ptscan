use crate::string::pts_string_t;

/// An address being watched.
pub struct pts_watch_t(pub(crate) ptscan::watch::Watch);

/// Access a human-readable version of the watch.
#[no_mangle]
pub extern "C" fn pts_watch_display<'a>(watch: *const pts_watch_t, out: *mut pts_string_t) {
    let pts_watch_t(ref watch) = *null_ck!(&'a watch);
    let out = null_ck!(&'a mut out);
    *out = pts_string_t::new(format!("{:?}", watch));
}

/// Free the watch.
#[no_mangle]
pub extern "C" fn pts_watch_free(watch: *mut pts_watch_t) {
    free!(watch);
}
