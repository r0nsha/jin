use std::{fmt, mem};

use data_structures::index_vec::Key;
use indexmap::IndexSet;
use itertools::Itertools as _;
use ustr::{ustr, Ustr};

use crate::{
    db::{AdtKind, Db, DefId, DefKind, StructKind},
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    hir,
    hir::{FnKind, Hir},
    macros::create_bool_enum,
    mangle,
    middle::{BinOp, CmpOp, Mutability, NamePat, Pat, Vis},
    mir::{
        pmatch, AdtId, Block, BlockId, Body, Const, Fn, FnParam, FnSig,
        FnSigId, FxHashMap, FxHashSet, Global, GlobalId, GlobalKind, Inst, Mir,
        Span, StaticGlobal, UnOp, ValueId, ValueKind,
    },
    span::Spanned,
    ty::{
        coerce::{CoercionKind, Coercions},
        fold::TyFolder,
        Instantiation, Ty, TyKind,
    },
};

pub fn lower(db: &mut Db, hir: &Hir) -> Mir {
    Lower::new(db, hir).lower_all()
}

#[derive(Debug)]
pub(super) struct Lower<'db> {
    pub(super) db: &'db mut Db,
    pub(super) hir: &'db Hir,
    pub(super) mir: Mir,
    pub(super) id_to_fn_sig: FxHashMap<DefId, FnSigId>,
    pub(super) id_to_global: FxHashMap<DefId, GlobalId>,
    pub(super) struct_ctors: FxHashMap<AdtId, FnSigId>,
}

impl<'db> Lower<'db> {
    fn new(db: &'db mut Db, hir: &'db Hir) -> Self {
        Self {
            db,
            hir,
            mir: Mir::new(),
            id_to_fn_sig: FxHashMap::default(),
            id_to_global: FxHashMap::default(),
            struct_ctors: FxHashMap::default(),
        }
    }

    fn lower_all(mut self) -> Mir {
        for fun in &self.hir.fns {
            let def = &self.db[fun.def_id];
            let is_extern = fun.kind.is_extern();
            let name = if is_extern {
                def.name
            } else {
                mangle::mangle_fn_name(self.db, fun)
            };
            let sig = self.lower_fn_sig(&fun.sig, &fun.kind, name);
            self.id_to_fn_sig.insert(fun.def_id, sig);
        }

        for let_ in &self.hir.lets {
            if !let_.pat.any(|name| self.id_to_global.get(&name.id).is_some()) {
                self.lower_global_let(let_);
            }
        }

        for let_ in &self.hir.extern_lets {
            let id = self.mir.globals.insert_with_key(|id| Global {
                id,
                def_id: let_.id,
                name: let_.word.name(),
                ty: self.db[let_.id].ty,
                kind: GlobalKind::Extern,
            });

            self.id_to_global.insert(let_.id, id);
        }

        for f in self
            .hir
            .fns
            .iter()
            .filter(|f| matches!(f.kind, FnKind::Bare { .. }))
        {
            let sig = self.id_to_fn_sig[&f.def_id];
            self.lower_fn_body(sig, f);
        }

        self.mir
    }

    fn lower_global(&mut self, def_id: DefId) -> GlobalId {
        if let Some(target_id) = self.id_to_global.get(&def_id).copied() {
            return target_id;
        }

        let let_ = self.hir.lets.iter().find(|let_| match &let_.pat {
            Pat::Name(n) => n.id == def_id,
            Pat::Discard(_) => false,
        });

        if let Some(let_) = let_ {
            self.lower_global_let(let_).expect("to output a GlobalId")
        } else {
            panic!(
                "global let {} not found in hir.lets",
                self.db[def_id].qpath
            );
        }
    }

    fn lower_global_let(&mut self, let_: &hir::Let) -> Option<GlobalId> {
        LowerBody::new(self).lower_global_let(let_)
    }

    fn get_or_create_struct_ctor(&mut self, adt_id: AdtId) -> FnSigId {
        if let Some(sig_id) = self.struct_ctors.get(&adt_id) {
            return *sig_id;
        }

        // TODO: doesn't work for polymorphic structs...
        let sig_id = self.create_struct_ctor(adt_id);
        self.struct_ctors.insert(adt_id, sig_id);

        sig_id
    }

    fn create_struct_ctor(&mut self, adt_id: AdtId) -> FnSigId {
        let adt = &self.db[adt_id];
        let def = &self.db[adt.def_id];
        let struct_def = adt.as_struct().unwrap();

        let name = ustr(&def.qpath.clone().child(ustr("ctor")).join_with("_"));

        let params: Vec<_> = struct_def
            .fields
            .iter()
            .map(|field| FnParam {
                pat: Pat::Name(NamePat {
                    id: DefId::null(),
                    word: field.name,
                    vis: Vis::Private,
                    mutability: Mutability::Imm,
                    ty: field.ty,
                }),
                ty: field.ty,
            })
            .collect();

        let sig = self.mir.fn_sigs.insert_with_key(|id| FnSig {
            id,
            name,
            params,
            ty: struct_def.ctor_ty,
            is_extern: false,
            is_c_variadic: false,
            span: adt.name.span(),
        });

        let mut body = Body::new();
        let start_block = body.create_block("start");

        // Initialize the `this` value based on the struct kind
        let this = match struct_def.kind {
            StructKind::Ref => {
                let value =
                    body.create_value(adt.ty(), ValueKind::Register(None));
                body.block_mut(start_block).push_inst(Inst::Alloc { value });
                value
            }
            StructKind::Extern => {
                let value =
                    body.create_value(adt.ty(), ValueKind::Register(None));
                body.block_mut(start_block)
                    .push_inst(Inst::StackAlloc { value, init: None });
                value
            }
        };

        // Initialize all of the struct's fields
        for field in &struct_def.fields {
            let name = field.name.name();
            let ty = field.ty;

            let field_value =
                body.create_value(ty, ValueKind::Field(this, name));
            let param = body.create_value(ty, ValueKind::UniqueName(name));
            body.block_mut(start_block)
                .push_inst(Inst::Store { value: param, target: field_value });
        }

        // Return the struct
        body.block_mut(start_block).push_inst(Inst::Return { value: this });

        self.mir.fns.insert(sig, Fn { sig, body });

        sig
    }

    fn lower_fn_sig(
        &mut self,
        sig: &hir::FnSig,
        kind: &hir::FnKind,
        name: Ustr,
    ) -> FnSigId {
        let (is_extern, is_c_variadic) = match kind {
            FnKind::Bare { .. } => (false, false),
            FnKind::Extern { is_c_variadic } => (true, *is_c_variadic),
        };

        self.mir.fn_sigs.insert_with_key(|id| FnSig {
            id,
            name,
            params: sig
                .params
                .iter()
                .map(|p| FnParam { pat: p.pat.clone(), ty: p.ty })
                .collect(),
            ty: sig.ty,
            is_extern,
            is_c_variadic,
            span: sig.word.span(),
        })
    }

    fn lower_fn_body(&mut self, sig: FnSigId, f: &hir::Fn) {
        LowerBody::new(self).lower_fn(sig, f);
    }
}

#[derive(Debug)]
pub(super) struct LowerBody<'cx, 'db> {
    pub(super) cx: &'cx mut Lower<'db>,
    pub(super) body: Body,
    pub(super) value_states: ValueStates,
    pub(super) scopes: Vec<Scope>,
    pub(super) current_block: BlockId,
    pub(super) locals: FxHashMap<DefId, ValueId>,
    pub(super) fields: FxHashMap<ValueId, FxHashMap<Ustr, ValueId>>,
}

impl<'cx, 'db> LowerBody<'cx, 'db> {
    fn new(cx: &'cx mut Lower<'db>) -> Self {
        Self {
            cx,
            body: Body::new(),
            value_states: ValueStates::new(),
            scopes: vec![],
            current_block: BlockId::start(),
            locals: FxHashMap::default(),
            fields: FxHashMap::default(),
        }
    }

    fn lower_fn(mut self, sig: FnSigId, fun: &hir::Fn) {
        match &fun.kind {
            FnKind::Bare { body } => {
                if self.cx.db.main_function_id() == Some(fun.def_id) {
                    self.cx.mir.main_fn = Some(sig);
                }

                self.enter_scope(ScopeKind::Block, body.span);
                let start_block = self.body.create_block("start");
                self.position_at(start_block);

                for (idx, param) in fun.sig.params.iter().enumerate() {
                    match &param.pat {
                        Pat::Name(name) => {
                            let id = name.id;
                            let value = self
                                .create_value(param.ty, ValueKind::Local(id));
                            self.create_destroy_flag(value);
                            self.locals.insert(id, value);
                        }
                        Pat::Discard(span) => {
                            let value = self.create_untracked_value(
                                param.ty,
                                ValueKind::UniqueName(ustr(&format!("_{idx}"))),
                            );
                            self.destroy_value_entirely(value, *span);
                        }
                    }
                }

                let last_value = self.lower_expr(body);

                if !self.body.last_inst_is_return() {
                    // If the body isn't terminating, we must push a return instruction at the
                    // for the function's last value.
                    self.push_return(last_value, body.span.tail());
                }

                // println!("fn `{}`", fun.sig.word);
                // println!("{}", self.value_states);
                // for block in self.body.blocks() {
                //     println!(
                //         "{} {}: {:?}",
                //         block.id, block.name, block.successors
                //     );
                // }
                // println!("---------------------");

                self.exit_scope();

                self.body.cleanup();
                self.cx.mir.fns.insert(sig, Fn { sig, body: self.body });
            }
            FnKind::Extern { .. } => unreachable!(),
        }
    }

    fn lower_global_let(mut self, let_: &hir::Let) -> Option<GlobalId> {
        match &let_.pat {
            Pat::Name(name) => {
                let full_name = self.cx.db[name.id].qpath.join_with("_");
                let ty = name.ty;

                self.enter_scope(ScopeKind::Block, let_.value.span);
                let start_block = self.body.create_block("start");
                self.position_at(start_block);

                let value = self.lower_expr(&let_.value);
                self.try_move(value, let_.value.span);
                self.exit_scope();

                self.body.cleanup();

                let id = self.cx.mir.globals.insert_with_key(|id| Global {
                    id,
                    def_id: name.id,
                    name: full_name.into(),
                    ty,
                    kind: GlobalKind::Static(StaticGlobal {
                        body: self.body,
                        result: value,
                    }),
                });
                self.cx.id_to_global.insert(name.id, id);

                Some(id)
            }
            Pat::Discard(_) => None,
        }
    }

    fn lower_expr(&mut self, expr: &hir::Expr) -> ValueId {
        let value = self.lower_expr_inner(expr);
        self.apply_coercions_to_expr(expr, value)
    }

    #[allow(clippy::too_many_lines)]
    fn lower_expr_inner(&mut self, expr: &hir::Expr) -> ValueId {
        match &expr.kind {
            hir::ExprKind::Let(let_) => {
                let init = self.lower_expr(&let_.value);

                match &let_.pat {
                    Pat::Name(name) => {
                        let value = self.push_inst_with(
                            let_.ty,
                            ValueKind::Local(name.id),
                            |value| Inst::StackAlloc {
                                value,
                                init: Some(init),
                            },
                        );
                        self.locals.insert(name.id, value);
                        self.create_destroy_flag(value);
                    }
                    Pat::Discard(span) => {
                        self.destroy_value_entirely(init, *span);
                    }
                }

                self.try_move(init, let_.value.span);

                self.const_unit()
            }
            hir::ExprKind::Assign(assign) => {
                let lhs = self.lower_expr(&assign.lhs);
                self.try_use(lhs, assign.lhs.span);
                self.check_assign_mutability(lhs, expr.span);

                let rhs = self.lower_expr(&assign.rhs);
                self.try_move(rhs, assign.rhs.span);

                let rhs = if let Some(op) = assign.op {
                    self.push_inst_with_register(assign.lhs.ty, |value| {
                        Inst::Binary { value, lhs, rhs, op, span: expr.span }
                    })
                } else {
                    rhs
                };

                // NOTE: The lhs needs to be destroyed before it's assigned to
                self.destroy_value_entirely(lhs, assign.lhs.span);
                self.push_inst(Inst::Store { value: rhs, target: lhs });

                self.const_unit()
            }
            hir::ExprKind::Match(match_) => {
                let output = self.push_inst_with_register(expr.ty, |value| {
                    Inst::StackAlloc { value, init: None }
                });

                let value = self.lower_expr(&match_.expr);
                self.try_use(value, match_.expr.span);

                let mut rows = vec![];
                let mut state = DecisionState::new(output, expr.span);

                for arm in &match_.arms {
                    let guard = arm.guard.as_ref().map(|expr| {
                        let block = self.body.create_block("guard");
                        state.guards.insert(block, expr);
                        block
                    });

                    let block = self.body.create_block("arm");
                    let pat = pmatch::Pat::from_hir(&arm.pat);
                    let col = pmatch::Col::new(value, pat);
                    let body = pmatch::DecisionBody::new(block, arm.pat.span());

                    state.bodies.insert(block, &arm.expr);
                    rows.push(pmatch::Row::new(vec![col], guard, body));
                }

                state.join_block = self.body.create_block("match_join");

                if let Ok((decision, new_guards)) =
                    pmatch::compile(self, rows, expr.span)
                {
                    for (new_guard, old_guard) in new_guards {
                        state
                            .guards
                            .insert(new_guard, state.guards[&old_guard]);
                    }

                    self.lower_decision(
                        &mut state,
                        decision,
                        self.current_block,
                        vec![],
                    );
                }

                self.position_at(state.join_block);

                output
            }
            hir::ExprKind::Loop(loop_) => {
                let start_block = self.body.create_block("loop_start");
                let end_block = self.body.create_block("loop_end");

                self.enter_scope(
                    ScopeKind::Loop(LoopScope::new(end_block)),
                    expr.span,
                );

                self.push_br(start_block);
                self.position_at(start_block);

                if let Some(cond_expr) = &loop_.cond {
                    let cond = self.lower_expr(cond_expr);
                    self.try_move(cond, cond_expr.span);

                    let not_cond = self.push_inst_with_register(
                        self.cx.db.types.bool,
                        |value| Inst::Unary {
                            value,
                            inner: cond,
                            op: UnOp::Not,
                        },
                    );

                    self.push_brif(not_cond, end_block, None);
                }

                self.lower_expr(&loop_.expr);
                self.push_br(start_block);

                if self.in_connected_block() {
                    self.check_loop_moves();
                }

                self.exit_scope();

                self.position_at(end_block);
                self.const_unit()
            }
            hir::ExprKind::Break => {
                self.destroy_loop_values(expr.span);

                let end_block = self
                    .closest_loop_scope()
                    .expect("to be inside a loop block")
                    .end_block;
                self.push_br(end_block);

                self.const_unit()
            }
            hir::ExprKind::Block(block) => {
                let mut result: Option<ValueId> = None;

                self.enter_scope(ScopeKind::Block, expr.span);

                for expr in &block.exprs {
                    result = Some(self.lower_expr(expr));
                }

                // NOTE: If the block ty is `unit`, we must always return a `unit` value.
                // A situation where we don't return a `unit` value can occur
                // when the expected type of the block is unit, but the last expression doesn't
                // return `unit`.
                let result = if expr.ty.is_unit() {
                    self.const_unit()
                } else {
                    result.unwrap_or_else(|| self.const_unit())
                };

                self.move_out(
                    result,
                    block.exprs.last().map_or(expr.span, |e| e.span),
                );
                self.exit_scope();

                result
            }
            hir::ExprKind::Return(ret) => {
                let value = self.lower_expr(&ret.expr);
                self.push_return(value, expr.span);
                self.const_unit()
            }
            hir::ExprKind::Call(call) => {
                // NOTE: We evaluate args in passing order, and then sort them to the actual
                // required parameter order
                let mut args = vec![];

                for arg in &call.args {
                    let idx = arg.index.expect("arg index to be resolved");
                    let value = self.lower_expr(&arg.expr);
                    self.try_move(value, arg.expr.span);
                    args.push((idx, value));
                }

                args.sort_by_key(|(idx, _)| *idx);

                let callee = self.lower_expr(&call.callee);
                self.try_move(callee, call.callee.span);

                self.push_inst_with_register(expr.ty, |value| Inst::Call {
                    value,
                    callee,
                    args: args.into_iter().map(|(_, arg)| arg).collect(),
                })
            }
            hir::ExprKind::Unary(un) => {
                let inner = self.lower_expr(&un.expr);

                match un.op {
                    UnOp::Neg | UnOp::Not => {
                        self.try_move(inner, un.expr.span);
                        self.push_inst_with_register(expr.ty, |value| {
                            Inst::Unary { value, inner, op: un.op }
                        })
                    }
                    UnOp::Ref(mutability) => {
                        if mutability.is_mut() {
                            self.check_ref_mutability(inner, expr.span);
                        }

                        self.create_ref(inner, expr.ty)
                    }
                }
            }
            hir::ExprKind::Binary(bin) => {
                let lhs = self.lower_expr(&bin.lhs);
                self.try_move(lhs, bin.lhs.span);

                let rhs = self.lower_expr(&bin.rhs);
                self.try_move(rhs, bin.rhs.span);

                self.push_inst_with_register(expr.ty, |value| Inst::Binary {
                    value,
                    lhs,
                    rhs,
                    op: bin.op,
                    span: expr.span,
                })
            }
            hir::ExprKind::Cast(cast) => {
                let inner = self.lower_expr(&cast.expr);
                self.try_move(inner, cast.expr.span);

                self.push_inst_with_register(cast.target, |value| Inst::Cast {
                    value,
                    inner,
                    target: cast.target,
                    span: expr.span,
                })
            }
            hir::ExprKind::Field(access) => {
                let name = access.field.name();
                let value = self.lower_expr(&access.expr);
                self.field_or_create(value, name, expr.ty)
            }
            hir::ExprKind::Name(name) => {
                self.lower_name(name.id, &name.instantiation)
            }
            hir::ExprKind::Lit(lit) => {
                let value = match lit {
                    hir::Lit::Str(lit) => Const::from(*lit),
                    #[allow(clippy::cast_possible_wrap)]
                    hir::Lit::Int(lit) => Const::from(*lit as i128),
                    hir::Lit::Float(lit) => Const::from(*lit),
                    hir::Lit::Bool(lit) => Const::from(*lit),
                };

                self.lower_const(&value, expr.ty)
            }
        }
    }

    fn lower_decision(
        &mut self,
        state: &mut DecisionState,
        decision: pmatch::Decision,
        parent_block: BlockId,
        values: Vec<ValueId>,
    ) -> BlockId {
        match decision {
            pmatch::Decision::Ok(body) => {
                self.body.create_edge(parent_block, body.block);
                self.lower_decision_bindings(state, body.block, body.bindings);
                self.lower_decision_body(state, self.current_block, body.block);
                body.block
            }
            pmatch::Decision::Err => unreachable!(),
            pmatch::Decision::Guard { guard, body, fallback } => self
                .lower_decision_guard(
                    state,
                    guard,
                    body,
                    *fallback,
                    parent_block,
                    values,
                ),
            pmatch::Decision::Switch { cond, cases, fallback } => {
                match cases[0].ctor {
                    pmatch::Ctor::Unit => self.lower_decision_unit(
                        state,
                        cases,
                        parent_block,
                        values,
                    ),
                    pmatch::Ctor::True | pmatch::Ctor::False => self
                        .lower_decision_bool(
                            state,
                            cond,
                            cases,
                            parent_block,
                            values,
                        ),
                    pmatch::Ctor::Int(_) | pmatch::Ctor::Str(_) => self
                        .lower_decision_lit(
                            state,
                            cond,
                            cases,
                            *fallback.unwrap(),
                            parent_block,
                            values,
                        ),
                    pmatch::Ctor::Struct(_) => self.lower_decision_struct(
                        state,
                        cond,
                        cases,
                        parent_block,
                        values,
                    ),
                }
            }
        }
    }

    fn lower_decision_unit(
        &mut self,
        state: &mut DecisionState,
        mut cases: Vec<pmatch::Case>,
        parent_block: BlockId,
        values: Vec<ValueId>,
    ) -> BlockId {
        assert!(cases.len() == 1, "unit can only have a single case");
        let case = cases.remove(0);
        self.lower_decision(state, case.decision, parent_block, values)
    }

    #[allow(clippy::needless_pass_by_value)]
    fn lower_decision_bool(
        &mut self,
        state: &mut DecisionState,
        cond: ValueId,
        cases: Vec<pmatch::Case>,
        parent_block: BlockId,
        values: Vec<ValueId>,
    ) -> BlockId {
        let block = self.body.create_block("case");

        self.body.create_edge(parent_block, block);

        let blocks: Vec<_> = cases
            .into_iter()
            .map(|case| {
                self.lower_decision(state, case.decision, block, values.clone())
            })
            .collect();

        self.position_at(block);
        self.push_brif(cond, blocks[1], Some(blocks[0]));

        block
    }

    fn lower_decision_lit(
        &mut self,
        state: &mut DecisionState,
        cond: ValueId,
        cases: Vec<pmatch::Case>,
        fallback: pmatch::Decision,
        parent_block: BlockId,
        mut values: Vec<ValueId>,
    ) -> BlockId {
        let blocks = self.body.create_blocks("case", cases.len());

        self.body.create_edge(parent_block, blocks[0]);
        self.body.connect_blocks(&blocks);
        values.push(cond);

        let fallback_block = self.lower_decision(
            state,
            fallback,
            *blocks.last().unwrap(),
            values.clone(),
        );

        for (idx, case) in cases.into_iter().enumerate() {
            let test_block = blocks[idx];

            let else_block =
                blocks.get(idx + 1).copied().unwrap_or(fallback_block);
            self.body.create_edge(test_block, else_block);

            let result_value = self.create_untracked_value(
                self.cx.db.types.bool,
                ValueKind::Register(None),
            );

            self.position_at(test_block);
            let lit_value = self.create_untracked_value(
                self.ty_of(cond),
                ValueKind::Const(case.ctor.into()),
            );

            self.push_inst(Inst::Binary {
                value: result_value,
                lhs: cond,
                rhs: lit_value,
                op: BinOp::Cmp(CmpOp::Eq),
                span: state.span,
            });

            let then_block = self.lower_decision(
                state,
                case.decision,
                test_block,
                values.clone(),
            );

            self.position_at(test_block);

            self.push_brif(result_value, then_block, Some(else_block));
        }

        blocks[0]
    }

    fn lower_decision_struct(
        &mut self,
        state: &mut DecisionState,
        cond: ValueId,
        mut cases: Vec<pmatch::Case>,
        parent_block: BlockId,
        mut values: Vec<ValueId>,
    ) -> BlockId {
        let case = cases.pop().unwrap();
        values.push(cond);
        self.lower_decision(state, case.decision, parent_block, values)
    }

    fn lower_decision_bindings(
        &mut self,
        state: &DecisionState,
        block: BlockId,
        bindings: pmatch::Bindings,
    ) {
        self.position_at(block);
        self.enter_scope(ScopeKind::Block, state.span);

        for binding in bindings {
            match binding {
                pmatch::Binding::Name(id, source, binding_ty, span) => {
                    // let binding_ty = self.cx.db[id].ty;

                    let binding_value = if binding_ty.is_ref() {
                        self.create_ref(source, binding_ty)
                    } else {
                        self.try_move(source, span);
                        self.push_inst_with(
                            binding_ty,
                            ValueKind::Local(id),
                            |value| Inst::StackAlloc {
                                value,
                                init: Some(source),
                            },
                        )
                    };

                    self.locals.insert(id, binding_value);
                    self.create_destroy_flag(binding_value);
                }
                pmatch::Binding::Discard(..) => {}
            }
        }
    }

    fn lower_decision_body(
        &mut self,
        state: &mut DecisionState,
        parent_block: BlockId,
        start_block: BlockId,
    ) -> BlockId {
        self.body.create_edge(parent_block, start_block);

        // Removing the expression makes sure that the code for a given block is only compiled
        // once.
        let Some(expr) = state.bodies.remove(&start_block) else {
            self.exit_scope();
            return start_block;
        };

        self.position_at(start_block);
        let value = self.lower_expr(expr);
        self.try_move(value, expr.span);
        self.exit_scope();

        if self.in_connected_block() {
            self.push_inst(Inst::Store { value, target: state.output });
            self.push_br(state.join_block);
        }

        start_block
    }

    fn lower_name(
        &mut self,
        id: DefId,
        instantiation: &Instantiation,
    ) -> ValueId {
        let value = match self.cx.db[id].kind.as_ref() {
            DefKind::Fn(_) => {
                let id = self.cx.id_to_fn_sig[&id];

                self.create_value(self.cx.mir.fn_sigs[id].ty, ValueKind::Fn(id))
            }
            DefKind::ExternGlobal | DefKind::Global => {
                let id = self.cx.lower_global(id);
                self.create_value(
                    self.cx.mir.globals[id].ty,
                    ValueKind::Global(id),
                )
            }
            DefKind::Variable => self.locals[&id],
            DefKind::Adt(adt_id) => {
                let id = self.cx.get_or_create_struct_ctor(*adt_id);
                self.create_value(self.cx.mir.fn_sigs[id].ty, ValueKind::Fn(id))
            }
            DefKind::Ty(_) => unreachable!("{:?}", &self.cx.db[id]),
        };

        if !instantiation.is_empty() {
            self.body.create_instantation(value, instantiation.clone());
        }

        value
    }

    fn lower_decision_guard(
        &mut self,
        state: &mut DecisionState,
        guard: BlockId,
        body: pmatch::DecisionBody,
        fallback: pmatch::Decision,
        parent_block: BlockId,
        values: Vec<ValueId>,
    ) -> BlockId {
        self.body.create_edge(parent_block, guard);

        // This makes sure we don't compile the same guard expression twice
        let Some(guard_expr) = state.guards.remove(&guard) else {
            return guard;
        };

        self.enter_scope(ScopeKind::Block, guard_expr.span);

        let mut restore_bindings = vec![];

        for bind in &body.bindings {
            if let pmatch::Binding::Name(id, new_value, _, _) = bind {
                if let Some(old_value) = self.locals.insert(*id, *new_value) {
                    restore_bindings.push((*id, old_value, *new_value));
                }
            }
        }

        self.position_at(guard);
        let cond = self.lower_expr(guard_expr);
        self.exit_scope();

        for (id, old_value, new_value) in restore_bindings {
            let new_state = self.value_state(new_value);
            self.set_value_state(old_value, new_state);
            self.locals.insert(id, old_value);
        }

        let guard_join = self.current_block;
        let fallback_block =
            self.lower_decision(state, fallback, guard_join, values);

        self.position_at(guard_join);
        self.push_brif(cond, body.block, Some(fallback_block));

        self.lower_decision_bindings(state, body.block, body.bindings);
        self.lower_decision_body(state, self.current_block, body.block);

        guard
    }

    pub fn lower_const(&mut self, value: &Const, ty: Ty) -> ValueId {
        match value {
            Const::Str(lit) => self.push_inst_with_register(ty, |value| {
                Inst::StrLit { value, lit: *lit }
            }),
            Const::Int(value) => {
                self.create_value(ty, ValueKind::Const(Const::from(*value)))
            }
            Const::Float(value) => {
                self.create_value(ty, ValueKind::Const(Const::from(*value)))
            }
            Const::Bool(value) => self.const_bool(*value),
            Const::Unit => self.const_unit(),
        }
    }

    pub fn const_unit(&mut self) -> ValueId {
        self.create_value(self.cx.db.types.unit, ValueKind::Const(Const::Unit))
    }

    pub fn const_bool(&mut self, value: bool) -> ValueId {
        self.create_value(
            self.cx.db.types.bool,
            ValueKind::Const(Const::Bool(value)),
        )
    }

    pub fn push_inst_with_register(
        &mut self,
        value_ty: Ty,
        f: impl FnOnce(ValueId) -> Inst,
    ) -> ValueId {
        self.push_inst_with(value_ty, ValueKind::Register(None), f)
    }

    pub fn push_inst_with_named_register(
        &mut self,
        value_ty: Ty,
        name: Ustr,
        f: impl FnOnce(ValueId) -> Inst,
    ) -> ValueId {
        self.push_inst_with(value_ty, ValueKind::Register(Some(name)), f)
    }

    pub fn push_inst_with(
        &mut self,
        value_ty: Ty,
        kind: ValueKind,
        f: impl FnOnce(ValueId) -> Inst,
    ) -> ValueId {
        let value = self.create_value(value_ty, kind);
        self.push_inst(f(value));
        value
    }

    fn push_return(&mut self, value: ValueId, span: Span) {
        self.try_move(value, span);
        self.destroy_all_values(span);
        self.push_inst(Inst::Return { value });
    }

    pub fn push_br(&mut self, target: BlockId) {
        self.create_edge(target);
        self.push_inst(Inst::Br { target });
    }

    pub fn push_brif(
        &mut self,
        cond: ValueId,
        then: BlockId,
        otherwise: Option<BlockId>,
    ) {
        self.create_edge(then);

        if let Some(otherwise) = otherwise {
            self.create_edge(otherwise);
        }

        self.push_inst(Inst::BrIf { cond, then, otherwise });
    }

    pub fn create_edge(&mut self, target: BlockId) {
        self.body.create_edge(self.current_block, target);
    }

    pub fn push_inst(&mut self, inst: Inst) {
        self.current_block_mut().push_inst(inst);
    }

    pub fn create_untracked_value(
        &mut self,
        ty: Ty,
        kind: ValueKind,
    ) -> ValueId {
        let value = self.body.create_value(ty, kind);
        self.set_owned(value);
        value
    }

    pub fn create_value(&mut self, ty: Ty, kind: ValueKind) -> ValueId {
        let value = self.body.create_value(ty, kind);
        self.set_owned(value);
        self.scope_mut().created_values.insert(value);
        self.create_value_fields(value);
        value
    }

    #[allow(unused)]
    pub fn clone_value(&mut self, value: ValueId) -> ValueId {
        let value = self.body.value(value);
        self.create_value(value.ty, value.kind.clone())
    }

    pub fn create_ref(&mut self, to_clone: ValueId, ty: Ty) -> ValueId {
        let value = self.push_inst_with_register(ty, |value| {
            Inst::StackAlloc { value, init: Some(to_clone) }
        });
        self.push_inst(Inst::IncRef { value });
        value
    }

    pub fn create_value_fields(&mut self, value: ValueId) {
        let value = self.body.value(value);

        if let TyKind::Adt(adt_id, targs) = value.ty.kind() {
            let adt = &self.cx.db[*adt_id];
            let instantiation = adt.instantiation(targs);
            let mut folder = instantiation.folder();

            match &adt.kind {
                AdtKind::Struct(struct_def) => {
                    let value = value.id;

                    let fields_to_create: Vec<(Ustr, Ty)> = struct_def
                        .fields
                        .iter()
                        .filter(|f| f.ty.is_move(self.cx.db) || f.ty.is_ref())
                        .map(|f| (f.name.name(), folder.fold(f.ty)))
                        .collect();

                    let fields: FxHashMap<_, _> = fields_to_create
                        .into_iter()
                        .map(|(name, ty)| {
                            let value = self.create_value(
                                ty,
                                ValueKind::Field(value, name),
                            );
                            self.create_destroy_flag(value);
                            (name, value)
                        })
                        .collect();

                    self.fields.insert(value, fields);
                }
                AdtKind::Union(_) => todo!(),
            }
        }
    }

    fn walk_fields(
        &mut self,
        value: ValueId,
        mut f: impl FnMut(&mut Self, ValueId) -> DiagnosticResult<()>,
    ) -> DiagnosticResult<()> {
        self.walk_fields_aux(value, &mut f)
    }

    fn walk_fields_aux(
        &mut self,
        value: ValueId,
        f: &mut impl FnMut(&mut Self, ValueId) -> DiagnosticResult<()>,
    ) -> DiagnosticResult<()> {
        if let Some(fields) = self.fields.get(&value).cloned() {
            for field in fields.values().copied() {
                f(self, field)?;
                self.walk_fields_aux(field, f)?;
            }
        }

        Ok(())
    }

    fn walk_parents<E>(
        &mut self,
        value: ValueId,
        mut f: impl FnMut(&mut Self, ValueId, ValueId) -> Result<(), E>,
    ) -> Result<(), E> {
        self.walk_parents_aux(value, &mut f)
    }

    fn walk_parents_aux<E>(
        &mut self,
        value: ValueId,
        f: &mut impl FnMut(&mut Self, ValueId, ValueId) -> Result<(), E>,
    ) -> Result<(), E> {
        if let &ValueKind::Field(parent, _) = &self.body.value(value).kind {
            f(self, parent, value)?;
            self.walk_parents_aux(parent, f)
        } else {
            Ok(())
        }
    }

    pub fn move_out(&mut self, value: ValueId, moved_to: Span) {
        let result = self.move_out_aux(value, moved_to);
        self.emit_result(result);
    }

    pub fn move_out_aux(
        &mut self,
        value: ValueId,
        moved_to: Span,
    ) -> DiagnosticResult<()> {
        self.check_if_moved(value, moved_to)?;

        let scope = self.scope_mut();
        scope.created_values.remove(&value);
        scope.moved_out.insert(value);

        self.walk_fields(value, |this, field| {
            this.move_out_aux(field, moved_to)
        })
    }

    pub fn try_move(&mut self, value: ValueId, moved_to: Span) {
        let result = self.try_move_inner(value, moved_to);
        self.emit_result(result);
    }

    pub fn try_move_inner(
        &mut self,
        value: ValueId,
        moved_to: Span,
    ) -> DiagnosticResult<()> {
        // If the value is copy, we don't need to move it.
        // Just check that its parents can be used.
        if !self.value_is_move(value) {
            self.walk_parents(value, |this, parent, _| {
                this.check_if_moved(parent, moved_to)
            })?;

            // When a reference is moved, its refcount is incremented.
            if self.value_is_ref(value) {
                self.create_ref(value, self.ty_of(value));
            }

            self.set_moved(value, moved_to);

            return Ok(());
        }

        self.check_if_moved(value, moved_to)?;

        // Mark the value and its fields as moved.
        // Mark its parents (if any) as partially moved
        self.set_moved(value, moved_to);
        self.walk_fields(value, |this, field| {
            this.set_moved(field, moved_to);
            Ok(())
        })
        .unwrap();
        self.walk_parents(
            value,
            |this, parent, child| -> DiagnosticResult<()> {
                this.check_move_out_of_ref(parent, child, moved_to)?;
                this.set_partially_moved(parent, moved_to);
                Ok(())
            },
        )?;

        self.insert_loop_move(value, moved_to);
        self.check_move_out_of_global(value, moved_to)?;

        self.set_destroy_flag(value);

        Ok(())
    }

    pub fn try_use(&mut self, value: ValueId, moved_to: Span) {
        let result = self.check_if_moved(value, moved_to);
        self.emit_result(result);
    }

    pub fn check_if_moved(
        &mut self,
        value: ValueId,
        moved_to: Span,
    ) -> DiagnosticResult<()> {
        if !self.value_is_move(value) {
            return Ok(());
        }

        match self.value_state(value) {
            ValueState::Owned => Ok(()),
            ValueState::Moved(already_moved_to)
            | ValueState::MaybeMoved(already_moved_to) => Err(self
                .use_after_move_err(
                    value,
                    moved_to,
                    already_moved_to,
                    "move",
                    "moved",
                )),
            ValueState::PartiallyMoved(already_moved_to) => Err(self
                .use_after_move_err(
                    value,
                    moved_to,
                    already_moved_to,
                    "partial move",
                    "partially moved",
                )),
        }
    }

    fn use_after_move_err(
        &self,
        value: ValueId,
        moved_to: Span,
        already_moved_to: Span,
        move_kind: &str,
        past_move_kind: &str,
    ) -> Diagnostic {
        let name = self.value_name(value);

        Diagnostic::error()
            .with_message(format!("use of {past_move_kind} {name}"))
            .with_label(
                Label::primary(moved_to).with_message(format!(
                    "{name} used here after {move_kind}"
                )),
            )
            .with_label(
                Label::secondary(already_moved_to).with_message(format!(
                    "{name} already {past_move_kind} here"
                )),
            )
    }

    pub fn check_move_out_of_global(
        &self,
        value: ValueId,
        moved_to: Span,
    ) -> DiagnosticResult<()> {
        if let ValueKind::Global(id) = self.body.value(value).kind {
            let global = &self.cx.mir.globals[id];
            let def = &self.cx.db[global.def_id];

            Err(Diagnostic::error()
                .with_message(format!(
                    "cannot move out of global item `{}`",
                    def.qpath
                ))
                .with_label(
                    Label::primary(moved_to)
                        .with_message("global item moved here"),
                ))
        } else {
            Ok(())
        }
    }

    pub fn check_move_out_of_ref(
        &self,
        parent: ValueId,
        field: ValueId,
        moved_to: Span,
    ) -> DiagnosticResult<()> {
        let parent_ty = self.ty_of(parent);

        if parent_ty.is_ref() {
            Err(Diagnostic::error()
                .with_message(format!(
                    "cannot move {} out of reference `{}`",
                    self.value_name(field),
                    parent_ty.display(self.cx.db)
                ))
                .with_label(Label::primary(moved_to).with_message(format!(
                    "cannot move out of {}",
                    self.value_name(parent)
                ))))
        } else {
            Ok(())
        }
    }

    pub fn insert_loop_move(&mut self, value: ValueId, span: Span) {
        let scope = self.scope();

        if scope.loop_depth == 0 || self.value_depth(value) >= scope.loop_depth
        {
            return;
        }

        if let Some(loop_scope) = self.closest_loop_scope_mut() {
            loop_scope.moved_in.insert(value, span);
        }
    }

    fn value_depth(&self, value: ValueId) -> usize {
        self.scopes
            .iter()
            .find(|s| s.created_values.contains(&value))
            .map_or(0, |s| s.depth)
    }

    pub fn check_loop_moves(&mut self) {
        let Some(loop_scope) = self.scope_mut().kind.as_loop_mut() else {
            return;
        };

        let moved_in_loop = mem::take(&mut loop_scope.moved_in);

        for (value, moved_to) in moved_in_loop {
            if !matches!(self.value_state(value), ValueState::Owned) {
                let name = self.value_name(value);

                self.cx.db.diagnostics.emit(
                    Diagnostic::error()
                        .with_message(format!("use of moved {name}"))
                        .with_label(Label::primary(moved_to).with_message(
                            format!(
                                "{name} moved here, in the previous loop \
                                 iteration"
                            ),
                        ))
                        .with_label(
                            Label::secondary(self.scope().span)
                                .with_message("inside this loop"),
                        ),
                );
            }
        }
    }

    pub(super) fn value_state(&mut self, value: ValueId) -> ValueState {
        if let Some(state) =
            self.value_states.get(self.current_block, value).cloned()
        {
            return state;
        }

        let state = self.solve_value_state(value);
        self.value_states.insert(self.current_block, value, state.clone());
        state
    }

    pub(super) fn solve_value_state(&self, value: ValueId) -> ValueState {
        let block = self.current_block;

        let mut work: Vec<BlockId> =
            self.body.block(block).predecessors.iter().copied().collect();
        let mut visited = FxHashSet::from_iter([block]);
        let mut result_state = ValueState::Owned;
        let mut last_move_span: Option<Span> = None;
        let mut is_initial_state = true;

        while let Some(block) = work.pop() {
            visited.insert(block);

            if let Some(state) = self.value_states.get(block, value).cloned() {
                match &state {
                    ValueState::Owned => (),
                    ValueState::Moved(moved_to)
                    | ValueState::MaybeMoved(moved_to)
                    | ValueState::PartiallyMoved(moved_to) => {
                        last_move_span = Some(*moved_to);
                    }
                };

                match (&result_state, &state) {
                    (ValueState::Owned, _) if is_initial_state => {
                        result_state = state;
                        is_initial_state = false;
                    }
                    (ValueState::MaybeMoved(_), _) => break,
                    _ => {
                        if result_state != state {
                            result_state = ValueState::MaybeMoved(
                                last_move_span
                                    .expect("to have been moved somewhere"),
                            );
                        }
                    }
                }
            } else {
                // Add this block's predecessors, since we need to
                // calculate the value's state for those blocks too
                work.extend(
                    self.body
                        .block(block)
                        .predecessors
                        .iter()
                        .filter(|b| !visited.contains(b)),
                );
            }
        }

        debug_assert!(
            !is_initial_state,
            "value v{} aka {} (type: {}) is missing a state in block {:?}. \
             value states:\n{}",
            value.0,
            self.value_name(value),
            self.ty_of(value).display(self.cx.db),
            block,
            self.value_states
        );

        result_state
    }

    fn value_is_move(&self, value: ValueId) -> bool {
        self.ty_of(value).is_move(self.cx.db)
    }

    fn value_is_ref(&self, value: ValueId) -> bool {
        self.ty_of(value).is_ref()
    }

    pub fn set_owned(&mut self, value: ValueId) {
        self.set_value_state(value, ValueState::Owned);
    }

    pub fn set_moved(&mut self, value: ValueId, moved_to: Span) {
        self.set_value_state(value, ValueState::Moved(moved_to));
    }

    pub fn set_partially_moved(&mut self, value: ValueId, moved_to: Span) {
        self.set_value_state(value, ValueState::PartiallyMoved(moved_to));
    }

    pub fn set_value_state(&mut self, value: ValueId, state: ValueState) {
        self.value_states.insert(self.current_block, value, state);
    }

    #[inline]
    pub fn position_at(&mut self, id: BlockId) {
        self.current_block = id;
    }

    pub fn apply_coercions_to_expr(
        &mut self,
        expr: &hir::Expr,
        value: ValueId,
    ) -> ValueId {
        if let Some(coercions) = self.cx.hir.coercions.get(&expr.id) {
            self.apply_coercions(&coercions.clone(), value, expr.span)
        } else {
            value
        }
    }

    pub fn apply_coercions(
        &mut self,
        coercions: &Coercions,
        mut coerced_value: ValueId,
        span: Span,
    ) -> ValueId {
        for coercion in coercions.iter() {
            coerced_value = match coercion.kind {
                CoercionKind::NeverToAny | CoercionKind::MutRefToImm => {
                    coerced_value
                }
                CoercionKind::AnyToUnit => self.const_unit(),
                CoercionKind::IntPromotion => {
                    self.push_inst_with_register(coercion.target, |value| {
                        Inst::Cast {
                            value,
                            inner: coerced_value,
                            target: coercion.target,
                            span,
                        }
                    })
                }
                CoercionKind::OwnedToRef => {
                    self.create_ref(coerced_value, coercion.target)
                }
            };

            self.body.value_mut(coerced_value).ty = coercion.target;
        }

        coerced_value
    }

    #[track_caller]
    fn scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    #[track_caller]
    fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn enter_scope(&mut self, kind: ScopeKind, span: Span) {
        self.scopes.push(Scope {
            kind,
            depth: self.scopes.len(),
            loop_depth: self
                .scopes
                .last()
                .map(|s| s.loop_depth + 1)
                .unwrap_or_default(),
            span,
            created_values: IndexSet::default(),
            moved_out: IndexSet::default(),
        });
    }

    fn exit_scope(&mut self) -> Scope {
        let scope = self.scopes.pop().expect("cannot exit the root scope");

        self.destroy_scope_values(&scope);

        if let Some(curr_scope) = self.scopes.last_mut() {
            curr_scope.created_values.extend(&scope.moved_out);
        }

        scope
    }

    fn destroy_scope_values(&mut self, scope: &Scope) {
        if !self.in_connected_block() {
            return;
        }

        let span = scope.span.tail();

        for &value in scope.created_values.iter().rev() {
            self.destroy_value(value, span);
        }
    }

    fn destroy_loop_values(&mut self, span: Span) {
        let values_to_destroy = self
            .scopes
            .iter()
            .rev()
            .take_while_inclusive(|s| !matches!(s.kind, ScopeKind::Loop(_)))
            .flat_map(|s| s.created_values.iter().rev())
            .copied()
            .collect::<Vec<_>>();

        for value in values_to_destroy {
            self.destroy_value(value, span);
        }
    }

    fn destroy_all_values(&mut self, span: Span) {
        let values_to_destroy = self
            .scopes
            .iter()
            .rev()
            .flat_map(|s| s.created_values.iter().rev())
            .copied()
            .collect::<Vec<_>>();

        for value in values_to_destroy {
            self.destroy_value(value, span);
        }
    }

    fn closest_loop_scope(&self) -> Option<&LoopScope> {
        self.scopes.iter().rev().find_map(|s| s.kind.as_loop())
    }

    fn closest_loop_scope_mut(&mut self) -> Option<&mut LoopScope> {
        self.scopes.iter_mut().rev().find_map(|s| s.kind.as_loop_mut())
    }

    fn destroy_value(&mut self, value: ValueId, span: Span) {
        if !self.value_is_move(value) && !self.value_is_ref(value) {
            return;
        }

        match self.value_state(value) {
            ValueState::Moved(_) => {
                // Value has been moved, don't destroy
            }
            ValueState::MaybeMoved(_) => {
                // let destroy_block = self.body.create_block("destroy");
                // let no_destroy_block = self.body.create_block("no_destroy");
                //
                // self.push_brif(destroy_flag, destroy_block, Some(no_destroy_block));
                //
                // self.position_at(destroy_block);
                // self.push_inst(Inst::Destroy { value });
                //
                // // Now that the value is destroyed, it has definitely been moved...
                // self.set_value_as_moved(value, moved_to);
                //
                // self.position_at(no_destroy_block);

                // Conditional destroy
                self.push_inst(Inst::Free {
                    value,
                    destroy_flag: Some(self.body.destroy_flags[&value]),
                    span,
                });
            }
            ValueState::PartiallyMoved { .. } | ValueState::Owned => {
                // Unconditional destroy
                self.push_inst(Inst::Free { value, destroy_flag: None, span });
            }
        }
    }

    fn destroy_fields(&mut self, value: ValueId, span: Span) {
        self.walk_fields(value, |this, field| {
            this.destroy_value(field, span);
            Ok(())
        })
        .unwrap();
    }

    fn destroy_value_entirely(&mut self, value: ValueId, span: Span) {
        self.destroy_fields(value, span);
        self.destroy_value(value, span);
    }

    pub(super) fn create_destroy_flag(&mut self, value: ValueId) {
        if !self.value_is_move(value) {
            return;
        }

        let init = self.const_bool(true);
        let flag = self.push_inst_with_named_register(
            self.cx.db.types.bool,
            ustr("destroy_flag"),
            |value| Inst::StackAlloc { value, init: Some(init) },
        );
        self.body.destroy_flags.insert(value, flag);
    }

    fn set_destroy_flag(&mut self, value: ValueId) {
        if let Some(destroy_flag) = self.body.destroy_flags.get(&value).copied()
        {
            let const_false = self.const_bool(false);
            self.push_inst(Inst::Store {
                value: const_false,
                target: destroy_flag,
            });
            self.walk_fields(value, |this, field| {
                this.set_destroy_flag(field);
                Ok(())
            })
            .unwrap();
        }
    }

    fn value_name(&self, value: ValueId) -> String {
        match &self.body.value(value).kind {
            ValueKind::Register(_) | ValueKind::Const(_) => {
                "temporary value".to_string()
            }
            _ => format!("`{}`", self.value_name_aux(value)),
        }
    }

    fn value_name_aux(&self, value: ValueId) -> String {
        match &self.body.value(value).kind {
            ValueKind::Local(id) => self.cx.db[*id].name.to_string(),
            ValueKind::Global(id) => self.cx.mir.globals[*id].name.to_string(),
            ValueKind::Fn(id) => self.cx.mir.fn_sigs[*id].name.to_string(),
            ValueKind::Field(parent, field) => {
                format!("{}.{}", self.value_name_aux(*parent), field)
            }
            ValueKind::UniqueName(name) => name.to_string(),
            ValueKind::Register(_) | ValueKind::Const(_) => "_".to_string(),
        }
    }

    fn current_block(&self) -> &Block {
        self.body.block(self.current_block)
    }

    fn current_block_mut(&mut self) -> &mut Block {
        self.body.block_mut(self.current_block)
    }

    pub fn in_connected_block(&self) -> bool {
        self.current_block().is_connected()
    }

    fn check_assign_mutability(&mut self, lhs: ValueId, span: Span) {
        if let Err(root) = self.value_imm_root(lhs, BreakOnMutRef::Yes) {
            self.cx.db.diagnostics.emit(self.imm_root_err(
                "cannot assign",
                lhs,
                root,
                span,
            ));
        }
    }

    fn check_ref_mutability(&mut self, value: ValueId, span: Span) {
        if let Err(root) = self
            .value_ty_imm_root(value)
            .and_then(|()| self.value_imm_root(value, BreakOnMutRef::No))
        {
            self.cx.db.diagnostics.emit(self.imm_root_err(
                "cannot take &mut",
                value,
                root,
                span,
            ));
        }
    }

    fn imm_root_err(
        &self,
        prefix: &str,
        value: ValueId,
        root: ImmutableRoot,
        span: Span,
    ) -> Diagnostic {
        match root {
            ImmutableRoot::Def(root) => {
                let root_name = self.value_name(root);

                let message = if root == value {
                    format!("{prefix} to immutable value {root_name}")
                } else {
                    format!(
                        "{} to {}, as {} is not declared as mutable",
                        prefix,
                        self.value_name(value),
                        root_name
                    )
                };

                Diagnostic::error().with_message(message).with_label(
                    Label::primary(span)
                        .with_message(format!("{prefix} to immutable value")),
                )
            }
            ImmutableRoot::Ref(root) => {
                let root_name = self.value_name(root);

                let message = format!(
                    "{} to {}, as {} is behind a `&` reference",
                    prefix,
                    self.value_name(value),
                    root_name,
                );

                Diagnostic::error()
                    .with_message(message)
                    .with_label(Label::primary(span).with_message(format!(
                        "{prefix} to immutable reference"
                    )))
                    .with_note(format!(
                        "{} is of type `{}`, which is immutable",
                        root_name,
                        self.ty_of(root).display(self.cx.db)
                    ))
            }
        }
    }

    fn value_imm_root(
        &self,
        value: ValueId,
        break_on_mut_ty: BreakOnMutRef,
    ) -> Result<(), ImmutableRoot> {
        match &self.body.value(value).kind {
            ValueKind::Local(id) => {
                if self.def_is_imm(*id) {
                    Err(ImmutableRoot::Def(value))
                } else {
                    Ok(())
                }
            }
            ValueKind::Global(id) => {
                if self.def_is_imm(self.cx.mir.globals[*id].def_id) {
                    Err(ImmutableRoot::Def(value))
                } else {
                    Ok(())
                }
            }
            ValueKind::Field(parent, _) => {
                match (self.value_ty_imm_root(*parent), break_on_mut_ty) {
                    (Ok(()), BreakOnMutRef::Yes) => Ok(()),
                    (Ok(()), BreakOnMutRef::No) => {
                        self.value_imm_root(*parent, break_on_mut_ty)
                    }
                    (Err(err), _) => Err(err),
                }
            }
            ValueKind::Fn(_)
            | ValueKind::Const(_)
            | ValueKind::Register(_)
            | ValueKind::UniqueName(_) => Ok(()),
        }
    }

    fn value_ty_imm_root(&self, value: ValueId) -> Result<(), ImmutableRoot> {
        if self.ty_of(value).is_imm_ref() {
            Err(ImmutableRoot::Ref(value))
        } else {
            Ok(())
        }
    }

    fn def_is_imm(&self, id: DefId) -> bool {
        self.cx.db[id].mutability.is_imm()
    }

    pub fn emit_result(&mut self, result: DiagnosticResult<()>) {
        if let Err(diagnostic) = result {
            self.cx.db.diagnostics.emit(diagnostic);
        }
    }

    pub fn ty_of(&self, value: ValueId) -> Ty {
        self.body.value(value).ty
    }

    pub fn field_or_create(
        &mut self,
        of: ValueId,
        name: Ustr,
        ty: Ty,
    ) -> ValueId {
        self.field(of, name).unwrap_or_else(|| {
            self.create_untracked_value(ty, ValueKind::Field(of, name))
        })
    }

    pub fn field(&self, of: ValueId, name: Ustr) -> Option<ValueId> {
        self.fields.get(&of).and_then(|fields| fields.get(&name)).copied()
    }
}

#[derive(Debug)]
pub(super) struct ValueStates(FxHashMap<BlockId, BlockState>);

impl ValueStates {
    fn new() -> Self {
        Self(FxHashMap::default())
    }

    fn get(&self, block: BlockId, value: ValueId) -> Option<&ValueState> {
        self.0.get(&block).and_then(|b| b.states.get(&value))
    }

    #[allow(unused)]
    fn get_mut(
        &mut self,
        block: BlockId,
        value: ValueId,
    ) -> Option<&mut ValueState> {
        self.0.get_mut(&block).and_then(|b| b.states.get_mut(&value))
    }

    fn insert(&mut self, block: BlockId, value: ValueId, state: ValueState) {
        self.0.entry(block).or_default().states.insert(value, state);
    }
}

impl Default for ValueStates {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
struct BlockState {
    states: FxHashMap<ValueId, ValueState>,
}

impl BlockState {
    fn new() -> Self {
        Self { states: FxHashMap::default() }
    }
}

impl Default for BlockState {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for ValueStates {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (block, block_state) in &self.0 {
            writeln!(f, "b{}:\n{}", block.0, block_state)?;
        }

        Ok(())
    }
}

impl fmt::Display for BlockState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (value, state) in &self.states {
            writeln!(
                f,
                "- v{} : {}",
                value.0,
                match state {
                    ValueState::Owned => "owned",
                    ValueState::Moved(_) => "moved",
                    ValueState::MaybeMoved(_) => "maybe moved",
                    ValueState::PartiallyMoved(_) => "partially moved",
                }
            )?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) enum ValueState {
    /// The value is owned, and should be dropped at the end of its scope
    Owned,

    /// The value has been moved. It shouldn't be dropped in its scope.
    Moved(Span),

    /// The value has been moved in one branch, but is still
    /// owned in another branch. This value should be dropped conditionally at the end of its scope
    MaybeMoved(Span),

    // Some of this value's fields have been moved, and the parent value is considered as moved.
    // The parent value should be destroyed in its scope.
    PartiallyMoved(Span),
}

#[derive(Debug, Clone)]
pub(super) struct Scope {
    kind: ScopeKind,
    depth: usize,
    loop_depth: usize,
    span: Span,

    // Values that were created in this scope
    created_values: IndexSet<ValueId>,
    moved_out: IndexSet<ValueId>,
}

#[derive(Debug, Clone)]
enum ScopeKind {
    Block,
    Loop(LoopScope),
}

impl ScopeKind {
    #[must_use]
    fn as_loop(&self) -> Option<&LoopScope> {
        if let Self::Loop(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    fn as_loop_mut(&mut self) -> Option<&mut LoopScope> {
        if let Self::Loop(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
struct LoopScope {
    end_block: BlockId,
    moved_in: FxHashMap<ValueId, Span>,
}

impl LoopScope {
    fn new(end_block: BlockId) -> Self {
        Self { end_block, moved_in: FxHashMap::default() }
    }
}

#[derive(Debug, Clone, Copy)]
enum ImmutableRoot {
    Def(ValueId),
    Ref(ValueId),
}

create_bool_enum!(BreakOnMutRef);

#[derive(Debug)]
struct DecisionState<'a> {
    /// The value the pmatch result is written to
    output: ValueId,

    /// The block to join into at the end of each decision body
    join_block: BlockId,

    /// A mapping of arm bodies, and their associated concrete expression
    bodies: FxHashMap<BlockId, &'a hir::Expr>,

    /// A mapping of arm guards, and their associated concrete guard expression
    guards: FxHashMap<BlockId, &'a hir::Expr>,

    /// The match expression's span
    span: Span,
}

impl<'a> DecisionState<'a> {
    fn new(output: ValueId, span: Span) -> Self {
        Self {
            output,
            join_block: BlockId::null(),
            bodies: FxHashMap::default(),
            guards: FxHashMap::default(),
            span,
        }
    }
}
