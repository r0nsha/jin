use rustc_hash::FxHashSet;

use crate::{
    db::{Db, DefId},
    diagnostics::Diagnostic,
    hir,
    mir::{BlockId, ValueId},
};

pub fn compile(db: &Db, rows: Vec<Row>) -> (Decision, Vec<Diagnostic>) {
    Compiler::new(db).compile(rows)
}

#[derive(Debug)]
pub struct Column {
    /// The value in question
    pub value: ValueId,

    /// The pattern that `value` is being matched against
    pub pat: hir::MatchPat,
}

#[derive(Debug)]
pub struct Row {
    pub columns: Vec<Column>,
    pub body: Body,
}

#[derive(Debug)]
struct Compiler<'db> {
    db: &'db Db,
    missing: bool,
    reachable: FxHashSet<BlockId>,
}

impl<'db> Compiler<'db> {
    fn new(db: &'db Db) -> Self {
        Self { db, missing: false, reachable: FxHashSet::default() }
    }

    fn compile(mut self, rows: Vec<Row>) -> (Decision, Vec<Diagnostic>) {
        match self.compile_rows(rows) {
            Decision::Err => {
                // TODO: create diagnostics here
                (Decision::Err, vec![])
            }
            decision => (decision, vec![]),
        }
    }

    fn compile_rows(&mut self, rows: Vec<Row>) -> Decision {
        if rows.is_empty() {
            self.missing = true;
            return Decision::Err;
        }

        todo!()
    }
}

#[derive(Debug)]
struct Diagnostics {
    missing: bool,
    // reachable: Vec<usize>
}

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
struct Body {
    pub bindings: Vec<DefId>,
    pub block_id: BlockId,
}

#[derive(Debug)]
struct Case {
    /// The constructor to test the given value against
    ctor: Constructor,

    /// Bindings to introduce to the body of this case.
    args: Vec<DefId>,

    /// The subtree of this case
    body: Decision,
}

// TODO: this should be in hir?
/// A type constructor.
#[derive(Debug, Clone, Eq, PartialEq)]
enum Constructor {
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
