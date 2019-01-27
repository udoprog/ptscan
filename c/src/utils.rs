pub(crate) fn constrain<'a, T>(value: *const T) -> &'a T {
    unsafe { &*value }
}

pub(crate) fn constrain_mut<'a, T>(value: *mut T) -> &'a mut T {
    unsafe { &mut *value }
}

/// NULL check the given argument and convert into a reference with a bounded lifetime.
macro_rules! null_ck {
    (&$l:lifetime mut $expr:expr) => {{
        null_ck!(@test $expr, null_mut);
        $crate::utils::constrain_mut::<$l, _>($expr)
    }};

    (&$l:lifetime $expr:expr) => {{
        null_ck!(@test $expr, null);
        $crate::utils::constrain::<$l, _>($expr)
    }};

    (@test $expr:expr, $m:ident) => {
        if $expr == std::ptr::$m() {
            panic!(concat!("unexpected null pointer in `", stringify!($expr), "`"));
        }
    };
}

/// Convert the given expression into a pointer.
macro_rules! into_ptr {
    ($expr:expr) => {
        Box::into_raw(Box::new($expr))
    };
}

/// Free the given expression by taking ownership of it, causing it to drop at the end of the scope.
macro_rules! free {
    ($expr:expr) => {
        // FIXME: should we panic on NULL?
        if $expr != ptr::null_mut() {
            unsafe {
                Box::from_raw($expr);
            }
        }
    };
}

/// Helper macro to handle errors.
///
/// Any raised error will be stored in thread-local memory and can be accessed using the corresponding pts_error_*
/// functions.
macro_rules! try_last {
    ($expr:expr, $err:expr) => {
        match $expr {
            Ok(value) => value,
            Err(e) => {
                $crate::error::LAST_ERROR.with(|last_error| {
                    *last_error.borrow_mut() = Some(failure::Error::from(e));
                });

                return $err;
            }
        }
    };
}
