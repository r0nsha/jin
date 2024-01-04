/// This implementation is inspired by [yorickpeterse's implementation](https://github.com/yorickpeterse/pattern-matching-in-rust)
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    db::{Db, DefId},
    diagnostics::{Diagnostic, Label},
    hir,
    mir::{BlockId, ValueId},
    span::{Span, Spanned},
};

pub fn compile(db: &Db, rows: Vec<Row>) -> (Decision, Vec<Diagnostic>) {
    let mut body_pat_spans = FxHashMap::default();

    for row in &rows {
        let first_span = row.columns[0].pat.span();
        let pat_span = if row.columns.len() > 1 {
            first_span.merge(row.columns.last().unwrap().pat.span())
        } else {
            first_span
        };

        body_pat_spans.insert(row.body.block_id, pat_span);
    }

    Compiler::new(db, body_pat_spans).compile(rows)
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

    /// Whether there's a pattern the user is missing
    missing: bool,

    /// The set of reachable body blocks
    reachable: FxHashSet<BlockId>,

    /// Associated body blocks with their span, used for reachability diagnostics
    body_pat_spans: FxHashMap<BlockId, Span>,
}

impl<'db> Compiler<'db> {
    fn new(db: &'db Db, body_pat_spans: FxHashMap<BlockId, Span>) -> Self {
        Self {
            db,
            missing: false,
            reachable: FxHashSet::default(),
            body_pat_spans,
        }
    }

    fn compile(mut self, rows: Vec<Row>) -> (Decision, Vec<Diagnostic>) {
        let mut diagnostics = vec![];
        let all_blocks: Vec<_> = rows.iter().map(|r| r.body.block_id).collect();

        let decision = self.compile_rows(rows);

        if self.reachable.len() != all_blocks.len() {
            self.reachability_diagnostics(&all_blocks, &mut diagnostics);
        }

        // TODO: report missing cases

        (decision, diagnostics)
    }

    fn reachability_diagnostics(
        &self,
        all_blocks: &[BlockId],
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        diagnostics.extend(
            all_blocks.iter().filter(|b| !self.reachable.contains(b)).map(
                |blk| {
                    let span = self.body_pat_spans[blk];

                    Diagnostic::warning()
                        .with_message("unreachable pattern")
                        .with_label(
                            Label::primary(span)
                                .with_message("unreachable pattern"),
                        )
                },
            ),
        );
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
            hir::MatchPat::Name(id, _) => {
                row.body.bindings.push(Binding::Name(*id, col.value));
                false
            }
            _ => true,
        });
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
    pub bindings: Bindings,
    pub block_id: BlockId,
    pub span: Span,
}

pub type Bindings = Vec<Binding>;

#[derive(Debug)]
pub enum Binding {
    Name(DefId, ValueId),
    Discard(ValueId),
}

impl Body {
    pub fn new(block_id: BlockId, span: Span) -> Self {
        Self { bindings: vec![], block_id, span }
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
