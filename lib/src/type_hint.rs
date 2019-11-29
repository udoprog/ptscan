use crate::Type;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[error("can solve between two conflicting types {0} and {1}")]
pub struct TypeSolveError(pub Type, pub Type);

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

    /// Convert hint into an implicit hint.
    pub fn into_implicit(self) -> Self {
        match self {
            Self::Explicit(ty) | Self::Implicit(ty) => Self::Implicit(ty),
            Self::NoHint => Self::NoHint,
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

    pub fn or_implicit(self, ty: Type) -> TypeHint {
        match self {
            Self::NoHint => Self::Implicit(ty),
            other => other,
        }
    }

    /// Try to solve two type hints into one.
    pub fn solve(self, other: TypeHint) -> Result<TypeHint, TypeSolveError> {
        Ok(match (self, other) {
            (Self::Explicit(lhs), Self::Explicit(rhs)) => {
                if lhs != rhs {
                    return Err(TypeSolveError(lhs, rhs));
                }

                TypeHint::Explicit(lhs)
            }
            // NB: explicit type hint takes precedence.
            (Self::Explicit(ty), _) | (_, Self::Explicit(ty)) => TypeHint::Explicit(ty),
            (Self::Implicit(ty), _) | (_, Self::Implicit(ty)) => TypeHint::Implicit(ty),
            _ => TypeHint::NoHint,
        })
    }
}
