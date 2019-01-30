use crate::{pointer::Pointer, string::StringT};

/// An address being watched.
pub struct Watch(pub(crate) ptscan::watch::Watch);

/// Access a human-readable version of the pointer.
#[no_mangle]
pub extern "C" fn pts_watch_display_pointer<'a>(watch: *const Watch, out: *mut StringT) {
    let Watch(ref watch) = *null_ck!(&'a watch);
    let out = null_ck!(&'a mut out);
    *out = StringT::new(format!("{}", watch.pointer));
}

/// Set the pointer of the watch using a clone of the provided pointer.
#[no_mangle]
pub extern "C" fn pts_watch_set_pointer<'a>(watch: *mut Watch, pointer: *const Pointer) {
    let Watch(ref mut watch) = *null_ck!(&'a mut watch);
    let Pointer(ref pointer) = *null_ck!(&'a pointer);
    watch.pointer = pointer.clone();
}

/// Get a clone of the pointer used by the watch.
#[no_mangle]
pub extern "C" fn pts_watch_get_pointer<'a>(watch: *mut Watch) -> *mut Pointer {
    let Watch(ref mut watch) = *null_ck!(&'a mut watch);
    into_ptr!(Pointer(watch.pointer.clone()))
}

/// Access a human-readable version of the watch value.
#[no_mangle]
pub extern "C" fn pts_watch_display_value<'a>(watch: *const Watch, out: *mut StringT) {
    let Watch(ref watch) = *null_ck!(&'a watch);
    let out = null_ck!(&'a mut out);
    *out = StringT::new(format!("{}", watch.value));
}

/// Access a human-readable version of the watch type.
#[no_mangle]
pub extern "C" fn pts_watch_display_type<'a>(watch: *const Watch, out: *mut StringT) {
    let Watch(ref watch) = *null_ck!(&'a watch);
    let out = null_ck!(&'a mut out);
    *out = StringT::new(format!("{}", watch.ty));
}

/// Free the watch.
#[no_mangle]
pub extern "C" fn pts_watch_free(watch: *mut Watch) {
    free!(watch);
}
