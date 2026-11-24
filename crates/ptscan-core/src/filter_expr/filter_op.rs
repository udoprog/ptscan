use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FilterOp {
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
    StartsWith,
}

impl fmt::Display for FilterOp {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Eq => "==".fmt(fmt),
            Self::Neq => "!=".fmt(fmt),
            Self::Gt => ">".fmt(fmt),
            Self::Gte => ">=".fmt(fmt),
            Self::Lt => "<".fmt(fmt),
            Self::Lte => "<=".fmt(fmt),
            Self::StartsWith => "^".fmt(fmt),
        }
    }
}
