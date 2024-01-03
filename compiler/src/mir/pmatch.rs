use crate::{db::DefId, hir};

#[derive(Debug)]
pub enum Decision {
    /// A successful pattern match
    Ok(Body),

    /// A pattern is missing
    Err,

    /// Check if a value matches any of the given patterns
    Switch { cond: DefId, cases: Vec<Case>, fallback: Option<Box<Decision>> },
}

#[derive(Debug)]
pub struct Body {
    pub bindings: Vec<DefId>,
    pub expr: hir::Expr,
}

#[derive(Debug)]
pub struct Case {
    /// The constructor to test the given value against
    pub ctor: Constructor,

    /// Bindings to introduce to the body of this case.
    pub args: Vec<DefId>,

    /// The subtree of this case
    pub body: Decision,
}

/// A type constructor.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constructor {
    // True,
    // False,
    // Int(i64),
    // Pair(TypeId, TypeId),
    // Variant(TypeId, usize),
}

impl Constructor {
    // /// Returns the index of this constructor relative to its type.
    // fn index(&self) -> usize {
    //     match self {
    //         Self::False
    //         | Self::Int(_)
    //         | Self::Pair(_, _)
    //         | Self::Range(_, _) => 0,
    //         Self::True => 1,
    //         Self::Variant(_, index) => *index,
    //     }
    // }
}
