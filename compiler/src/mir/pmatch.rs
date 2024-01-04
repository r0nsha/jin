/// This implementation is inspired by [yorickpeterse's implementation](https://github.com/yorickpeterse/pattern-matching-in-rust)
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    db::DefId,
    diagnostics::{Diagnostic, Label},
    hir,
    mir::{lower::LowerBody, BlockId, ValueId},
    span::{Span, Spanned},
    ty::{Ty, TyKind},
};

pub fn compile(
    cx: &mut LowerBody<'_, '_>,
    rows: Vec<Row>,
) -> (Decision, Vec<Diagnostic>) {
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

    Compiler::new(cx, body_pat_spans).compile(rows)
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
    pub body: DecisionBody,
}

impl Row {
    pub fn new(columns: Vec<Column>, body: DecisionBody) -> Self {
        Self { columns, body }
    }
}

#[derive(Debug)]
struct Compiler<'a, 'cx, 'db> {
    cx: &'a mut LowerBody<'cx, 'db>,

    /// Whether there's a pattern the user is missing
    missing: bool,

    /// The set of reachable body blocks
    reachable: FxHashSet<BlockId>,

    /// Associated body blocks with their span, used for reachability diagnostics
    body_pat_spans: FxHashMap<BlockId, Span>,
}

impl<'a, 'cx, 'db> Compiler<'a, 'cx, 'db> {
    fn new(
        cx: &'a mut LowerBody<'cx, 'db>,
        body_pat_spans: FxHashMap<BlockId, Span>,
    ) -> Self {
        Self {
            cx,
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
            Self::move_binding_pats(row);
        }

        // If the first row has no columns, we don't need to continue,
        // since they'll never be reachable anyways
        if rows.first().map_or(false, |c| c.columns.is_empty()) {
            let row = rows.swap_remove(0);
            self.reachable.insert(row.body.block_id);
            return Decision::Ok(row.body);
        }

        let branch_value = Self::branch_value(&rows);

        let ty = Type::from_ty(self.cx.ty_of(branch_value));

        match ty {
            Type::Finite(_) => todo!(),
        }
    }

    fn move_binding_pats(row: &mut Row) {
        row.columns.retain(|col| match &col.pat {
            hir::MatchPat::Name(id, span) => {
                row.body.bindings.push(Binding::Name(*id, col.value, *span));
                false
            }
            hir::MatchPat::Wildcard(span) => {
                row.body.bindings.push(Binding::Discard(col.value, *span));
                false
            }
            hir::MatchPat::Bool(..) => true,
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
    Ok(DecisionBody),

    /// A pattern is missing
    Err,
    // /// Check if a value matches any of the given patterns
    // Switch { cond: ValueId, cases: Vec<Case>, fallback: Option<Box<Decision>> },
}

#[derive(Debug)]
pub struct DecisionBody {
    pub bindings: Bindings,
    pub block_id: BlockId,
    pub span: Span,
}

pub type Bindings = Vec<Binding>;

#[derive(Debug)]
pub enum Binding {
    Name(DefId, ValueId, Span),
    Discard(ValueId, Span),
}

impl DecisionBody {
    pub fn new(block_id: BlockId, span: Span) -> Self {
        Self { bindings: vec![], block_id, span }
    }
}

#[derive(Debug)]
struct Case {
    // /// The constructor to test the given value against
    // ctor: Constructor,

    // /// Bindings to introduce to the body of this case.
    // args: Vec<DefId>,

    // /// The subtree of this case
    // body: Decision,
}

/// A simplified version of `Ty`, makes it easier to work with.
#[derive(Debug)]
enum Type {
    // Int,
    // Str,
    Finite(Vec<FiniteType>),
}

/// A type which represents a set of constructors
#[derive(Debug)]
struct FiniteType {
    /// The matched constructor
    ctor: Ctor,

    /// The values which are exposed to the constructor's sub tree
    values: Vec<ValueId>,

    /// Rows used for building the sub tree
    rows: Vec<Row>,
}

impl FiniteType {
    fn new(ctor: Ctor, values: Vec<ValueId>) -> Self {
        Self { ctor, values, rows: vec![] }
    }
}

impl Type {
    fn from_ty(ty: Ty) -> Self {
        match ty.kind() {
            TyKind::Bool => Self::Finite(vec![
                FiniteType::new(Ctor::True, vec![]),
                FiniteType::new(Ctor::False, vec![]),
            ]),
            TyKind::Fn(_)
            | TyKind::Adt(_, _)
            | TyKind::Ref(_, _)
            | TyKind::RawPtr(_)
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::Str
            | TyKind::Unit
            | TyKind::Never
            | TyKind::Param(_)
            | TyKind::Infer(_)
            | TyKind::Type(_)
            | TyKind::Module(_)
            | TyKind::Unknown => unreachable!(),
        }
    }
}

// A constructor for a given `Type`
#[derive(Debug, Clone, Eq, PartialEq)]
enum Ctor {
    True,
    False,
    // Int(i64),
    // Pair(TypeId, TypeId),
    // Variant(TypeId, usize),
}

impl Ctor {
    // /// Returns the index of this constructor relative to its type.
    fn index(&self) -> usize {
        match self {
            Self::False => 0,
            Self::True => 1,
            // Self::Variant(_, index) => *index,
        }
    }
}
