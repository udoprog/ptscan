// make moving clones into closures more convenient
#[macro_export]
macro_rules! clone {
    ($($n:ident),+ => move || $body:expr) => {{
        $( let $n = $n.clone(); )+
        move || $body
    }};

    ($($n:ident),+ => move |$($p:tt),*| $body:expr) => {{
        $( let $n = $n.clone(); )+
        move |$($p,)+| $body
    }};
}

// Handle optional expressions.
#[macro_export]
macro_rules! optional {
    ($x:expr, $r:expr) => {
        match $x {
            Some(e) => e,
            None => return $r,
        }
    };
    ($e:expr) => {
        optional!($e, ())
    };
}

// upgrade weak reference or return
#[macro_export]
macro_rules! upgrade {
    ($x:expr, $r:expr) => {
        optional!($x.upgrade(), $r)
    };
    ($x:expr) => {
        upgrade!($x, ())
    };
}
