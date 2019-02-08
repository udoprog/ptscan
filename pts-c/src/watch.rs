use crate::{pointer::Pointer, string::StringT, ty::Type, value::Value};

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

/// Set the type of the pointer.
#[no_mangle]
pub extern "C" fn pts_watch_set_type<'a>(watch: *mut Watch, ty: Type) {
    let Watch(ref mut watch) = *null_ck!(&'a mut watch);
    watch.ty = from_immediate!(ptscan::Type, ty);
}

/// Get a clone of the pointer used by the watch.
#[no_mangle]
pub extern "C" fn pts_watch_get_pointer<'a>(watch: *mut Watch) -> *mut Pointer {
    let Watch(ref mut watch) = *null_ck!(&'a mut watch);
    into_ptr!(Pointer(watch.pointer.clone()))
}

/// Access a copy of the value.
#[no_mangle]
pub extern "C" fn pts_watch_value<'a>(watch: *const Watch) -> Value {
    let Watch(ref watch) = *null_ck!(&'a watch);
    into_immediate!(watch.value.clone())
}

/// Access a human-readable version of the watch type.
#[no_mangle]
pub extern "C" fn pts_watch_type<'a>(watch: *const Watch) -> Type {
    let Watch(ref watch) = *null_ck!(&'a watch);
    into_immediate!(watch.ty)
}

/// Free the watch.
#[no_mangle]
pub extern "C" fn pts_watch_free(watch: *mut Watch) {
    free!(watch);
}
