/// This implementation is inspired by [yorickpeterse's implementation](https://github.com/yorickpeterse/pattern-matching-in-rust)
use indexmap::IndexSet;
use rustc_hash::{FxHashMap, FxHashSet};
use ustr::Ustr;

use crate::{
    db::{AdtId, AdtKind, DefId},
    diagnostics::{Diagnostic, Label, Severity},
    hir,
    mir::{lower::LowerBody, BlockId, Const, ValueId},
    span::{Span, Spanned},
    ty::{fold::TyFolder as _, Ty, TyKind},
};

pub fn compile(
    cx: &mut LowerBody<'_, '_>,
    rows: Vec<Row>,
    span: Span,
) -> Result<Decision, ()> {
    let mut body_pat_spans = FxHashMap::default();

    for row in &rows {
        let first_span = row.cols[0].pat.span();
        let pat_span = if row.cols.len() > 1 {
            first_span.merge(row.cols.last().unwrap().pat.span())
        } else {
            first_span
        };

        body_pat_spans.insert(row.body.block, pat_span);
    }

    let (decision, diagnostics) =
        Compiler::new(cx, body_pat_spans).compile(rows, span);

    if diagnostics.iter().any(|d| d.severity() == Severity::Error) {
        cx.cx.db.diagnostics.emit_many(diagnostics);
        Err(())
    } else {
        cx.cx.db.diagnostics.emit_many(diagnostics);
        Ok(decision)
    }
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
    pub guard: Option<BlockId>,
    pub body: DecisionBody,
}

impl Row {
    pub fn new(
        cols: Vec<Col>,
        guard: Option<BlockId>,
        body: DecisionBody,
    ) -> Self {
        Self { cols, guard, body }
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

    diagnostics: Vec<Diagnostic>,
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
            diagnostics: vec![],
        }
    }

    fn compile(
        mut self,
        rows: Vec<Row>,
        span: Span,
    ) -> (Decision, Vec<Diagnostic>) {
        let all_blocks: Vec<_> = rows.iter().map(|r| r.body.block).collect();
        let decision = self.compile_rows(rows);

        if self.reachable.len() != all_blocks.len() {
            self.report_unreachable_pats(&all_blocks);
        }

        if self.missing {
            let pats = self.collect_missing_pats(&decision);
            self.diagnostics.push(Self::missing_pats_diagnostic(pats, span));
        }

        (decision, self.diagnostics)
    }

    fn report_unreachable_pats(&mut self, all_blocks: &[BlockId]) {
        self.diagnostics.extend(
            all_blocks.iter().filter(|b| !self.reachable.contains(b)).map(
                |block| {
                    let span = self.body_pat_spans[block];

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
            self.reachable.insert(row.body.block);
            return if let Some(guard) = row.guard {
                Decision::Guard {
                    guard,
                    body: row.body,
                    fallback: Box::new(self.compile_rows(rows)),
                }
            } else {
                Decision::Ok(row.body)
            };
        }

        let cond = Self::cond_value(&rows);

        match Type::from_cond(self.cx, cond, self.cx.ty_of(cond)) {
            Type::Int => self.compile_int_decision(rows, cond),
            Type::Str => self.compile_str_decision(rows, cond),
            Type::Finite(cases) => {
                self.compile_finite_decision(rows, cond, cases)
            }
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
            _ => true,
        });
    }

    fn cond_value(rows: &[Row]) -> ValueId {
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

    fn compile_int_decision(
        &mut self,
        rows: Vec<Row>,
        cond: ValueId,
    ) -> Decision {
        self.compile_lit_decision(
            rows,
            cond,
            |p| if let Pat::Int(k, _) = p { k } else { unreachable!() },
            Ctor::Int,
        )
    }

    fn compile_str_decision(
        &mut self,
        rows: Vec<Row>,
        cond: ValueId,
    ) -> Decision {
        self.compile_lit_decision(
            rows,
            cond,
            |p| if let Pat::Str(k, _) = p { k } else { unreachable!() },
            Ctor::Str,
        )
    }

    fn compile_lit_decision<K: Eq + core::hash::Hash + Copy>(
        &mut self,
        rows: Vec<Row>,
        cond: ValueId,
        get_key: impl FnOnce(Pat) -> K + Copy,
        get_ctor: impl FnOnce(K) -> Ctor + Copy,
    ) -> Decision {
        let mut type_cases = Vec::<TypeCase>::new();
        let mut fallback_rows = Vec::<Row>::new();
        let mut indices = FxHashMap::<K, usize>::default();

        for mut row in rows {
            if let Some(col) = row.remove_col(cond) {
                for (pat, row) in col.pat.flatten_or(row) {
                    let key = get_key(pat);

                    if let Some(idx) = indices.get(&key) {
                        type_cases[*idx].rows.push(row);
                        continue;
                    }

                    indices.insert(key, type_cases.len());
                    type_cases.push(TypeCase::new_with_rows(
                        get_ctor(key),
                        vec![],
                        vec![row],
                    ));
                }
            } else {
                fallback_rows.push(row);
            }
        }

        self.compile_lit_cases(cond, type_cases, fallback_rows)
    }

    fn compile_lit_cases(
        &mut self,
        cond: ValueId,
        mut type_cases: Vec<TypeCase>,
        fallback_rows: Vec<Row>,
    ) -> Decision {
        for case in &mut type_cases {
            case.rows.extend(fallback_rows.clone());
        }

        let cases = type_cases
            .into_iter()
            .map(|case| {
                Case::new(case.ctor, case.values, self.compile_rows(case.rows))
            })
            .collect();

        let fallback = self.compile_rows(fallback_rows);

        Decision::Switch { cond, cases, fallback: Some(Box::new(fallback)) }
    }

    fn compile_finite_decision(
        &mut self,
        rows: Vec<Row>,
        cond: ValueId,
        mut cases: Vec<TypeCase>,
    ) -> Decision {
        for mut row in rows {
            if let Some(col) = row.remove_col(cond) {
                for (pat, row) in col.pat.flatten_or(row) {
                    if let Pat::Ctor(ctor, args, _) = pat {
                        let mut cols = row.cols;
                        let case = &mut cases[ctor.index()];

                        cols.extend(
                            case.values
                                .iter()
                                .zip(args.into_iter())
                                .map(|(value, pat)| Col::new(*value, pat)),
                        );

                        case.rows.push(Row::new(cols, row.guard, row.body));
                    }
                }
            } else {
                for case in &mut cases {
                    case.rows.push(row.clone());
                }
            }
        }

        let cases = cases
            .into_iter()
            .map(|case| {
                Case::new(case.ctor, case.values, self.compile_rows(case.rows))
            })
            .collect();

        Decision::Switch { cond, cases, fallback: None }
    }
}

#[derive(Debug)]
pub(super) enum Decision {
    /// A successful pattern match
    Ok(DecisionBody),

    /// A pattern is missing
    Err,

    /// Evaluates the given `guard` block. If it's evaluated to true, evaluates `body`, otherwise, evaluates
    /// `fallback`.
    Guard { guard: BlockId, body: DecisionBody, fallback: Box<Decision> },

    /// Check if a value matches any of the given patterns
    Switch { cond: ValueId, cases: Vec<Case>, fallback: Option<Box<Decision>> },
}

#[derive(Debug, Clone)]
pub struct DecisionBody {
    pub bindings: Bindings,
    pub block: BlockId,
    pub span: Span,
}

pub type Bindings = Vec<Binding>;

#[derive(Debug, Clone)]
pub enum Binding {
    Name(DefId, ValueId, Span),
    Discard(ValueId, Span),
}

impl DecisionBody {
    pub fn new(block: BlockId, span: Span) -> Self {
        Self { bindings: vec![], block, span }
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
    Int,
    Str,
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
        Self::new_with_rows(ctor, values, vec![])
    }

    fn new_with_rows(ctor: Ctor, values: Vec<ValueId>, rows: Vec<Row>) -> Self {
        Self { ctor, values, rows }
    }
}

impl Type {
    fn from_cond(
        cx: &mut LowerBody<'_, '_>,
        cond: ValueId,
        matched_ty: Ty,
    ) -> Self {
        match matched_ty.kind() {
            TyKind::Unit => {
                Self::Finite(vec![TypeCase::new(Ctor::Unit, vec![])])
            }
            TyKind::Bool => Self::Finite(vec![
                TypeCase::new(Ctor::False, vec![]),
                TypeCase::new(Ctor::True, vec![]),
            ]),
            TyKind::Int(_) | TyKind::Uint(_) => Self::Int,
            TyKind::Str => Self::Str,
            TyKind::Adt(adt_id, targs) => {
                let adt = &cx.cx.db[*adt_id];
                let instantiation = adt.instantiation(targs);
                let mut folder = instantiation.folder();

                match &adt.kind {
                    AdtKind::Struct(struct_def) => {
                        let fields_to_create: Vec<(Ustr, Ty)> = struct_def
                            .fields
                            .iter()
                            .map(|f| (f.name.name(), folder.fold(f.ty)))
                            .collect();

                        Self::Finite(vec![TypeCase::new(
                            Ctor::Struct(*adt_id),
                            fields_to_create
                                .into_iter()
                                .map(|(name, ty)| {
                                    cx.field_or_create(cond, name, ty)
                                })
                                .collect(),
                        )])
                    }
                }
            }
            TyKind::Ref(ty, _) => Self::from_cond(cx, cond, *ty),
            TyKind::Fn(_)
            | TyKind::RawPtr(_)
            | TyKind::Float(_)
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
    Unit,
    True,
    False,
    Int(i128),
    Str(Ustr),
    Struct(AdtId),
}

impl Ctor {
    /// Returns the index of this constructor relative to its type.
    /// The index must match the order which constructor are defined in `Type::from_ty`
    fn index(&self) -> usize {
        match self {
            Self::Unit
            | Self::False
            | Self::Int(_)
            | Self::Str(_)
            | Self::Struct(_) => 0,
            Self::True => 1,
        }
    }
}

impl From<Const> for Ctor {
    fn from(value: Const) -> Self {
        match value {
            Const::Unit => Self::Unit,
            Const::Bool(true) => Self::True,
            Const::Bool(false) => Self::False,
            Const::Int(v) => Self::Int(v),
            Const::Str(v) => Self::Str(v),
            Const::Float(_) => panic!("unexpected const value {value:?}"),
        }
    }
}

impl From<Ctor> for Const {
    fn from(value: Ctor) -> Self {
        match value {
            Ctor::Unit => Const::Unit,
            Ctor::True => Const::Bool(true),
            Ctor::False => Const::Bool(false),
            Ctor::Int(v) => Const::Int(v),
            Ctor::Str(v) => Const::Str(v),
            Ctor::Struct(_) => panic!("unexpected ctor value {value:?}"),
        }
    }
}

// A constructor for a given `Type`
#[derive(Debug, Clone, Eq, PartialEq)]
pub(super) enum Pat {
    Int(i128, Span),
    Str(Ustr, Span),
    Ctor(Ctor, Vec<Pat>, Span),
    Name(DefId, Span),
    Wildcard(Span),
    Or(Vec<Pat>, Span),
}

impl Spanned for Pat {
    fn span(&self) -> Span {
        match self {
            Self::Int(_, span)
            | Self::Str(_, span)
            | Self::Ctor(_, _, span)
            | Self::Name(_, span)
            | Self::Wildcard(span)
            | Self::Or(_, span) => *span,
        }
    }
}

impl Pat {
    pub(super) fn flatten_or(self, row: Row) -> Vec<(Self, Row)> {
        if let Self::Or(pats, _) = self {
            pats.into_iter().map(|p| (p, row.clone())).collect()
        } else {
            vec![(self, row)]
        }
    }

    pub(super) fn from_hir(pat: &hir::MatchPat) -> Self {
        match pat {
            hir::MatchPat::Name(id, span) => Self::Name(*id, *span),
            hir::MatchPat::Wildcard(span) => Self::Wildcard(*span),
            hir::MatchPat::Unit(span) => Self::Ctor(Ctor::Unit, vec![], *span),
            hir::MatchPat::Bool(true, span) => {
                Self::Ctor(Ctor::True, vec![], *span)
            }
            hir::MatchPat::Bool(false, span) => {
                Self::Ctor(Ctor::False, vec![], *span)
            }
            hir::MatchPat::Int(value, span) => Self::Int(*value, *span),
            hir::MatchPat::Str(value, span) => Self::Str(*value, *span),
            hir::MatchPat::Adt(adt_id, pats, span) => {
                let args: Vec<_> = pats.iter().map(Self::from_hir).collect();
                Self::Ctor(Ctor::Struct(*adt_id), args, *span)
            }
            hir::MatchPat::Or(pats, span) => {
                Self::Or(pats.iter().map(Self::from_hir).collect(), *span)
            }
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
        Decision::Guard { fallback, .. } => {
            collect_missing_pats(cx, fallback, case_infos, missing);
        }
        Decision::Switch { cond, cases, fallback } => {
            for case in cases {
                match &case.ctor {
                    Ctor::Unit => case_infos.push(CaseInfo::new(
                        *cond,
                        "{}".to_string(),
                        vec![],
                    )),
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
                    Ctor::Int(_) | Ctor::Str(_) => case_infos
                        .push(CaseInfo::new(*cond, "_".to_string(), vec![])),
                    Ctor::Struct(adt_id) => case_infos.push(CaseInfo::new(
                        *cond,
                        cx.cx.db[*adt_id].name.to_string(),
                        case.values.clone(),
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
