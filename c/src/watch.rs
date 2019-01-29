use crate::{pointer::pts_pointer_t, string::pts_string_t};

/// An address being watched.
pub struct pts_watch_t(pub(crate) ptscan::watch::Watch);

/// Access a human-readable version of the pointer.
#[no_mangle]
pub extern "C" fn pts_watch_display_pointer<'a>(watch: *const pts_watch_t, out: *mut pts_string_t) {
    let pts_watch_t(ref watch) = *null_ck!(&'a watch);
    let out = null_ck!(&'a mut out);
    *out = pts_string_t::new(format!("{}", watch.pointer));
}

/// Set the pointer of the watch using a clone of the provided pointer.
#[no_mangle]
pub extern "C" fn pts_watch_set_pointer<'a>(
    watch: *mut pts_watch_t,
    pointer: *const pts_pointer_t,
) {
    let pts_watch_t(ref mut watch) = *null_ck!(&'a mut watch);
    let pts_pointer_t(ref pointer) = *null_ck!(&'a pointer);
    watch.pointer = pointer.clone();
}

/// Get a clone of the pointer used by the watch.
#[no_mangle]
pub extern "C" fn pts_watch_get_pointer<'a>(watch: *mut pts_watch_t) -> *mut pts_pointer_t {
    let pts_watch_t(ref mut watch) = *null_ck!(&'a mut watch);
    into_ptr!(pts_pointer_t(watch.pointer.clone()))
}

/// Access a human-readable version of the watch value.
#[no_mangle]
pub extern "C" fn pts_watch_display_value<'a>(watch: *const pts_watch_t, out: *mut pts_string_t) {
    let pts_watch_t(ref watch) = *null_ck!(&'a watch);
    let out = null_ck!(&'a mut out);
    *out = pts_string_t::new(format!("{}", watch.value));
}

/// Access a human-readable version of the watch type.
#[no_mangle]
pub extern "C" fn pts_watch_display_type<'a>(watch: *const pts_watch_t, out: *mut pts_string_t) {
    let pts_watch_t(ref watch) = *null_ck!(&'a watch);
    let out = null_ck!(&'a mut out);
    *out = pts_string_t::new(format!("{}", watch.ty));
}

/// Free the watch.
#[no_mangle]
pub extern "C" fn pts_watch_free(watch: *mut pts_watch_t) {
    free!(watch);
}
