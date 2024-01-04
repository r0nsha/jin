use indexmap::IndexSet;
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
    span: Span,
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

    Compiler::new(cx, body_pat_spans).compile(rows, span)
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

    fn compile(
        mut self,
        rows: Vec<Row>,
        span: Span,
    ) -> (Decision, Vec<Diagnostic>) {
        let mut diagnostics = vec![];
        let all_blocks: Vec<_> = rows.iter().map(|r| r.body.block_id).collect();

        let decision = self.compile_rows(rows);

        if self.reachable.len() != all_blocks.len() {
            self.report_unreachable_pats(&all_blocks, &mut diagnostics);
        }

        if self.missing {
            let pats = self.collect_missing_pats(&decision);
            diagnostics.push(Self::missing_pats_diagnostic(pats, span));
        }

        (decision, diagnostics)
    }

    fn report_unreachable_pats(
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

    fn missing_pats_diagnostic(
        pats: IndexSet<String>,
        span: Span,
    ) -> Diagnostic {
        const LIMIT: usize = 3;

        let pat_len = pats.len();

        let (missing, verb) = if pat_len > 1 {
            let mut missing = String::new();

            let overflow = pat_len.checked_sub(LIMIT).unwrap_or_default();
            let last_idx = (pat_len - 1).min(LIMIT - 1);

            for (idx, pat) in pats.into_iter().enumerate().take(LIMIT) {
                missing.push_str(&format!("`{pat}`"));

                if overflow == 0 && idx == last_idx - 1 {
                    missing.push_str(" and ");
                } else if idx < last_idx {
                    missing.push_str(", ");
                }
            }

            if overflow > 0 {
                missing.push_str(&format!(" and {overflow} more"));
            }

            (missing, "are")
        } else {
            (format!("`{}`", pats[0]), "is")
        };

        Diagnostic::error()
            .with_message(format!(
                "missing match arms: {missing} {verb} not covered"
            ))
            .with_label(
                Label::primary(span).with_message("match is not exhaustive"),
            )
    }

    fn collect_missing_pats(&self, decision: &Decision) -> IndexSet<String> {
        let mut case_infos = vec![];
        let mut missing = IndexSet::default();
        collect_missing_pats(self.cx, decision, &mut case_infos, &mut missing);
        missing
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

        match Type::from_ty(self.cx.ty_of(cond)) {
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

fn collect_missing_pats(
    cx: &LowerBody<'_, '_>,
    decision: &Decision,
    case_infos: &mut Vec<CaseInfo>,
    missing: &mut IndexSet<String>,
) {
    match decision {
        Decision::Ok(_) => (),
        Decision::Err => {
            let value_to_idx: FxHashMap<_, _> = case_infos
                .iter()
                .enumerate()
                .map(|(idx, case_info)| (case_info.value, idx))
                .collect();

            let full_name = case_infos.first().map_or_else(
                || "_".to_string(),
                |case_info| case_info.pat_name(case_infos, &value_to_idx),
            );

            missing.insert(full_name);
        }
        Decision::Switch { cond, cases, fallback } => {
            for case in cases {
                match &case.ctor {
                    Ctor::True => case_infos.push(CaseInfo::new(
                        *cond,
                        "true".to_string(),
                        vec![],
                    )),
                    Ctor::False => case_infos.push(CaseInfo::new(
                        *cond,
                        "false".to_string(),
                        vec![],
                    )),
                }

                collect_missing_pats(cx, &case.decision, case_infos, missing);
                case_infos.pop();
            }

            if let Some(fallback) = fallback {
                collect_missing_pats(cx, fallback, case_infos, missing);
            }
        }
    }
}

#[derive(Debug)]
struct CaseInfo {
    value: ValueId,
    name: String,
    args: Vec<ValueId>,
}

impl CaseInfo {
    fn new(value: ValueId, name: String, args: Vec<ValueId>) -> Self {
        Self { value, name, args }
    }

    fn pat_name(
        &self,
        case_infos: &[CaseInfo],
        value_to_idx: &FxHashMap<ValueId, usize>,
    ) -> String {
        if self.args.is_empty() {
            return self.name.to_string();
        }

        let args = self
            .args
            .iter()
            .map(|arg| {
                value_to_idx.get(arg).map_or_else(
                    || "_".to_string(),
                    |&idx| case_infos[idx].pat_name(case_infos, value_to_idx),
                )
            })
            .collect::<Vec<_>>()
            .join(", ");

        format!("{}({})", self.name, args)
    }
}
