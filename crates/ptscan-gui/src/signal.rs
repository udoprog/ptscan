use glib::{signal::SignalHandlerId, IsA, Object, ObjectExt as _};
use std::ops;

/// Trait for blocking and unblocking signals.
pub trait Signals {
    fn block<T>(&self, object: &T)
    where
        T: IsA<Object>;

    fn unblock<T>(&self, object: &T)
    where
        T: IsA<Object>;
}

impl<'a> Signals for &'a SignalHandlerId {
    fn block<T>(&self, object: &T)
    where
        T: IsA<Object>,
    {
        object.block_signal(*self);
    }

    fn unblock<T>(&self, object: &T)
    where
        T: IsA<Object>,
    {
        object.unblock_signal(*self);
    }
}

macro_rules! array_impl {
    ($($n:expr),*) => {
        $(
        impl<'a> Signals for [&'a SignalHandlerId; $n] {
            fn block<T>(&self, object: &T)
            where
                T: IsA<Object>,
            {
                for id in self {
                    object.block_signal(id);
                }
            }

            fn unblock<T>(&self, object: &T)
            where
                T: IsA<Object>,
            {
                for id in self {
                    object.unblock_signal(id);
                }
            }
        }
        )*
    };
}

array_impl!(1, 2, 3, 4, 5, 6, 7, 8);

/// Scoped handler for a single blocked signal.
pub struct Block<T, S>(T, S)
where
    T: IsA<Object>,
    S: Signals;

impl<T, S> Block<T, S>
where
    T: IsA<Object>,
    S: Signals,
{
    /// Block the specified signal until this block is dropped.
    pub fn new(object: T, signals: S) -> Self {
        signals.block(&object);
        Self(object, signals)
    }
}

impl<T, S> Drop for Block<T, S>
where
    T: IsA<Object>,
    S: Signals,
{
    fn drop(&mut self) {
        self.1.unblock(&self.0);
    }
}

impl<T, S> ops::Deref for Block<T, S>
where
    T: IsA<Object>,
    S: Signals,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T, S> ops::DerefMut for Block<T, S>
where
    T: IsA<Object>,
    S: Signals,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Extension trait to provide the `block` function.
pub trait BlockExt {
    fn block<S>(self, signals: S) -> Block<Self, S>
    where
        Self: IsA<Object>,
        S: Signals,
    {
        Block::new(self, signals)
    }
}

impl<T> BlockExt for T where T: IsA<Object> {}
