use ena::snapshot_vec::VecLike;
/// This implementation is inspired by [yorickpeterse's implementation](https://github.com/yorickpeterse/pattern-matching-in-rust)
use rustc_hash::{FxHashMap, FxHashSet};

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

impl Column {
    pub fn new(value: ValueId, pat: hir::MatchPat) -> Self {
        Self { value, pat }
    }
}

#[derive(Debug)]
pub struct Row {
    pub columns: Vec<Column>,
    pub body: Body,
}

impl Row {
    pub fn new(columns: Vec<Column>, body: Body) -> Self {
        Self { columns, body }
    }
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

    fn compile_rows(&mut self, mut rows: Vec<Row>) -> Decision {
        if rows.is_empty() {
            self.missing = true;
            return Decision::Err;
        }

        for row in &mut rows {
            Self::move_name_pats(row);
        }

        // If the first row has no columns, we don't need to continue,
        // since they'll never be reachable anyways
        if rows.first().map_or(false, |c| c.columns.is_empty()) {
            let row = rows.swap_remove(0);
            self.reachable.insert(row.body.block_id);
            return Decision::Ok(row.body);
        }

        let branch_value = Self::branch_value(&rows);

        dbg!(branch_value);

        todo!()
    }

    fn move_name_pats(row: &mut Row) {
        row.columns.retain(|col| match &col.pat {
            hir::MatchPat::Name(id) => {
                row.body.bindings.push((*id, col.value));
                false
            }
            _ => true,
        })
    }

    fn branch_value(rows: &[Row]) -> ValueId {
        let mut counts = FxHashMap::default();

        for row in rows {
            for col in &row.columns {
                *counts.entry(&col.value).or_insert(0_usize) += 1;
            }
        }

        rows[0]
            .columns
            .iter()
            .map(|col| col.value)
            .max_by_key(|var| counts[var])
            .unwrap()
    }
}

#[derive(Debug)]
pub enum Decision {
    /// A successful pattern match
    Ok(Body),

    /// A pattern is missing
    Err,

    // /// Check if a value matches any of the given patterns
    // Switch { cond: ValueId, cases: Vec<Case>, fallback: Option<Box<Decision>> },
}

#[derive(Debug)]
pub struct Body {
    pub bindings: Vec<(DefId, ValueId)>,
    pub block_id: BlockId,
}

impl Body {
    pub fn new(block_id: BlockId) -> Self {
        Self { bindings: vec![], block_id }
    }
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
