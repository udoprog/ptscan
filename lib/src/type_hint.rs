use crate::Type;

#[derive(Debug, Clone, Copy)]
pub enum TypeHint {
    NoHint,
    Implicit(Type),
    Explicit(Type),
}

impl TypeHint {
    /// Either extract the value or generate an error type.
    pub fn ok_or_else<T, E>(self, fallback: T) -> Result<Type, E>
    where
        T: FnOnce() -> E,
    {
        match self {
            Self::NoHint => Err(fallback()),
            Self::Implicit(ty) | Self::Explicit(ty) => Ok(ty),
        }
    }

    /// Make into an Option.
    pub fn option(self) -> Option<Type> {
        match self {
            Self::NoHint => None,
            Self::Implicit(ty) | Self::Explicit(ty) => Some(ty),
        }
    }

    /// Unwrap into a default type if this is None.
    pub fn unwrap_or(self, fallback: TypeHint) -> TypeHint {
        match self {
            Self::NoHint => fallback,
            other => other,
        }
    }
}
