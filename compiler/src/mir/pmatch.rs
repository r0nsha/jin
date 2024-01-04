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
        let first_span = row.cols[0].pat.span();
        let pat_span = if row.cols.len() > 1 {
            first_span.merge(row.cols.last().unwrap().pat.span())
        } else {
            first_span
        };

        body_pat_spans.insert(row.body.block_id, pat_span);
    }

    Compiler::new(cx, body_pat_spans).compile(rows)
}

#[derive(Debug, Clone)]
pub struct Col {
    /// The value to branch on
    pub value: ValueId,

    /// The pattern that `value` is being matched against
    pub pat: Pat,
}

impl Col {
    pub fn new(value: ValueId, pat: Pat) -> Self {
        Self { value, pat }
    }
}

#[derive(Debug, Clone)]
pub struct Row {
    pub cols: Vec<Col>,
    pub body: DecisionBody,
}

impl Row {
    pub fn new(cols: Vec<Col>, body: DecisionBody) -> Self {
        Self { cols, body }
    }

    fn remove_col(&mut self, value: ValueId) -> Option<Col> {
        self.cols
            .iter()
            .position(|c| c.value == value)
            .map(|idx| self.cols.remove(idx))
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
        if rows.first().map_or(false, |c| c.cols.is_empty()) {
            let row = rows.swap_remove(0);
            self.reachable.insert(row.body.block_id);
            return Decision::Ok(row.body);
        }

        let cond = Self::cond(&rows);
        let ty = Type::from_ty(self.cx.ty_of(cond));

        match ty {
            Type::Finite(cases) => Decision::Switch {
                cond,
                cases: self.compile_ctor_cases(rows, cond, cases),
                fallback: None,
            },
        }
    }

    fn move_binding_pats(row: &mut Row) {
        row.cols.retain(|col| match &col.pat {
            Pat::Name(id, span) => {
                row.body.bindings.push(Binding::Name(*id, col.value, *span));
                false
            }
            Pat::Wildcard(span) => {
                row.body.bindings.push(Binding::Discard(col.value, *span));
                false
            }
            Pat::Ctor(..) => true,
        });
    }

    fn cond(rows: &[Row]) -> ValueId {
        let mut counts = FxHashMap::default();

        for row in rows {
            for col in &row.cols {
                *counts.entry(&col.value).or_insert(0_usize) += 1;
            }
        }

        rows[0]
            .cols
            .iter()
            .map(|col| col.value)
            .max_by_key(|var| counts[var])
            .unwrap()
    }

    fn compile_ctor_cases(
        &mut self,
        rows: Vec<Row>,
        cond: ValueId,
        mut cases: Vec<TypeCase>,
    ) -> Vec<Case> {
        for mut row in rows {
            if let Some(col) = row.remove_col(cond) {
                for (pat, row) in col.pat.flatten_or(row) {
                    if let Pat::Ctor(ctor, args, _) = pat {
                        let idx = ctor.index();
                        let mut cols = row.cols;
                        let case = &mut cases[idx];

                        // TODO: if args != case.values : missing fields in pattern

                        cols.extend(
                            case.values
                                .iter()
                                .zip(args.into_iter())
                                .map(|(value, pat)| Col::new(*value, pat)),
                        );

                        case.rows.push(Row::new(cols, row.body));
                    }
                }
            } else {
                for case in &mut cases {
                    case.rows.push(row.clone());
                }
            }
        }

        cases
            .into_iter()
            .map(|case| {
                Case::new(case.ctor, case.values, self.compile_rows(case.rows))
            })
            .collect()
    }
}

#[derive(Debug)]
pub(super) enum Decision {
    /// A successful pattern match
    Ok(DecisionBody),

    /// A pattern is missing
    Err,

    /// Check if a value matches any of the given patterns
    Switch { cond: ValueId, cases: Vec<Case>, fallback: Option<Box<Decision>> },
}

#[derive(Debug, Clone)]
pub struct DecisionBody {
    pub bindings: Bindings,
    pub block_id: BlockId,
    pub span: Span,
}

pub type Bindings = Vec<Binding>;

#[derive(Debug, Clone)]
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
pub(super) struct Case {
    /// The constructor to test the given value against
    pub(super) ctor: Ctor,

    /// Values to introduce to the body of this case.
    pub(super) values: Vec<ValueId>,

    /// The subtree of this case
    pub(super) decision: Decision,
}

impl Case {
    pub(super) fn new(
        ctor: Ctor,
        values: Vec<ValueId>,
        body: Decision,
    ) -> Self {
        Self { ctor, values, decision: body }
    }
}

/// A simplified version of `Ty`, makes it easier to work with.
#[derive(Debug)]
enum Type {
    // Int,
    // Str,
    Finite(Vec<TypeCase>),
}

/// A type which represents a set of constructors
#[derive(Debug)]
struct TypeCase {
    /// The matched constructor
    ctor: Ctor,

    /// The values which are exposed to the constructor's sub tree
    values: Vec<ValueId>,

    /// Rows used for building the sub tree
    rows: Vec<Row>,
}

impl TypeCase {
    fn new(ctor: Ctor, values: Vec<ValueId>) -> Self {
        Self { ctor, values, rows: vec![] }
    }
}

impl Type {
    fn from_ty(ty: Ty) -> Self {
        match ty.kind() {
            TyKind::Bool => Self::Finite(vec![
                TypeCase::new(Ctor::True, vec![]),
                TypeCase::new(Ctor::False, vec![]),
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
pub(super) enum Ctor {
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

// A constructor for a given `Type`
#[derive(Debug, Clone, Eq, PartialEq)]
pub(super) enum Pat {
    Ctor(Ctor, Vec<Pat>, Span),
    Name(DefId, Span),
    Wildcard(Span),
}

impl Pat {
    pub(super) fn flatten_or(self, row: Row) -> Vec<(Self, Row)> {
        // if let Self::Or(pats, _) = self {
        //     pats.into_iter().map(|p| (p, row.clone())).collect()
        // } else {
        vec![(self, row)]
        // }
    }

    pub(super) fn from_hir(pat: &hir::MatchPat) -> Self {
        match pat {
            hir::MatchPat::Name(id, span) => Self::Name(*id, *span),
            hir::MatchPat::Wildcard(span) => Self::Wildcard(*span),
            hir::MatchPat::Bool(true, span) => {
                Self::Ctor(Ctor::True, vec![], *span)
            }
            hir::MatchPat::Bool(false, span) => {
                Self::Ctor(Ctor::False, vec![], *span)
            }
        }
    }
}

impl Spanned for Pat {
    fn span(&self) -> Span {
        match self {
            Self::Ctor(_, _, span)
            | Self::Name(_, span)
            | Self::Wildcard(span) => *span,
        }
    }
}
