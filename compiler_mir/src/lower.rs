use compiler_core::middle::Vis;
use compiler_data_structures::index_vec::Key;
use indexmap::IndexSet;
use ustr::{ustr, Ustr};

use compiler_core::{
    db::{AdtField, AdtKind, Builtin, Db, DefId, DefKind, StructKind, UnionKind, VariantId},
    diagnostics::{Diagnostic, DiagnosticResult},
    hir,
    hir::{FnKind, Hir},
    mangle,
    middle::{BinOp, CmpOp, Mutability, NamePat, Pat},
    span::Spanned,
    sym,
    ty::{
        coerce::{CoercionKind, Coercions},
        fold::TyFolder,
        Instantiation, Ty, TyKind,
    },
};

use crate::{
    builder::InstBuilder,
    ownck::{CannotMove, ValueState, ValueStates},
    pmatch, AdtId, Block, BlockId, Body, Const, Fn, FnParam, FnSig, FnSigId, FxHashMap, Global,
    GlobalId, GlobalKind, Inst, Mir, RtCallKind, Span, StaticGlobal, UnOp, ValueId, ValueKind,
};

#[allow(clippy::similar_names)]
pub fn lower(db: &mut Db, hir: &Hir) -> Mir {
    let mut cx = Lower::new(db, hir);
    cx.run();
    let mir = cx.mir;
    db.diagnostics.extend(cx.diagnostics);
    mir
}

#[derive(Debug)]
pub(super) struct Lower<'db> {
    pub(super) db: &'db Db,
    pub(super) hir: &'db Hir,
    pub(super) mir: Mir,
    pub(super) diagnostics: Vec<Diagnostic>,
    pub(super) id_to_fn_sig: FxHashMap<DefId, FnSigId>,
    pub(super) id_to_global: FxHashMap<DefId, GlobalId>,
    pub(super) struct_ctors: FxHashMap<AdtId, FnSigId>,
    pub(super) variant_ctors: FxHashMap<VariantId, FnSigId>,
}

impl<'db> Lower<'db> {
    fn new(db: &'db Db, hir: &'db Hir) -> Self {
        Self {
            db,
            hir,
            mir: Mir::new(),
            diagnostics: vec![],
            id_to_fn_sig: FxHashMap::default(),
            id_to_global: FxHashMap::default(),
            struct_ctors: FxHashMap::default(),
            variant_ctors: FxHashMap::default(),
        }
    }

    fn run(&mut self) {
        for fun in &self.hir.fns {
            let sig = self.lower_fn_sig(fun);
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
                ty: let_.ty,
                kind: GlobalKind::Extern,
            });

            self.id_to_global.insert(let_.id, id);
        }

        for f in self.hir.fns.iter().filter(|f| matches!(f.kind, FnKind::Bare { .. })) {
            let sig = self.id_to_fn_sig[&f.def_id];
            self.lower_fn_body(sig, f);
        }
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
            panic!("global let {} not found in hir.lets", self.db[def_id].qpath);
        }
    }

    fn lower_global_let(&mut self, let_: &hir::Let) -> Option<GlobalId> {
        LowerBody::new(self).lower_global_let(let_)
    }

    fn get_or_create_struct_ctor(&mut self, adt_id: AdtId) -> FnSigId {
        if let Some(sig_id) = self.struct_ctors.get(&adt_id) {
            return *sig_id;
        }

        let sig_id = self.create_struct_ctor(adt_id);
        self.struct_ctors.insert(adt_id, sig_id);

        sig_id
    }

    fn create_struct_ctor(&mut self, adt_id: AdtId) -> FnSigId {
        let adt = &self.db[adt_id];
        let def = &self.db[adt.def_id];
        let struct_def = adt.as_struct().unwrap();

        let mangled_name = ustr(&format!("{}_ctor", def.qpath.join_with("_")));
        let display_name = ustr(&def.qpath.join());

        let params = Self::adt_fields_to_fn_params(&struct_def.fields);
        let sig = self.mir.fn_sigs.insert_with_key(|id| FnSig {
            id,
            def_id: adt.def_id,
            mangled_name,
            display_name,
            params,
            ty: struct_def.ctor_ty,
            is_inline: false,
            span: adt.name.span(),
        });

        let mut body = Body::new();
        let start_block = body.create_block("start");

        // Initialize the `this` value based on the struct kind
        let this = body.create_register(adt.ty());
        match struct_def.kind {
            StructKind::Ref => body.ins(start_block).alloc(this),
            StructKind::Value => body.ins(start_block).stackalloc_uninit(this),
        };

        Self::ctor_init_adt_fields(&mut body, start_block, this, &struct_def.fields);

        // Return the struct
        body.ins(start_block).ret(this);

        self.mir.fns.insert(sig, Fn { sig, body });

        sig
    }

    fn get_or_create_variant_ctor(&mut self, variant_id: VariantId) -> FnSigId {
        if let Some(sig_id) = self.variant_ctors.get(&variant_id) {
            return *sig_id;
        }

        let sig_id = self.create_variant_ctor(variant_id);
        self.variant_ctors.insert(variant_id, sig_id);

        sig_id
    }

    fn create_variant_ctor(&mut self, variant_id: VariantId) -> FnSigId {
        let variant = &self.db[variant_id];
        let adt = &self.db[variant.adt_id];
        let def = &self.db[adt.def_id];
        let union_def = adt.as_union().unwrap();

        let mangled_name = ustr(&format!("{}_{}", def.qpath.join_with("_"), variant.name));
        let display_name = ustr(&format!("{}_{}", def.qpath.join(), variant.name));

        let params = Self::adt_fields_to_fn_params(&variant.fields);
        let sig = self.mir.fn_sigs.insert_with_key(|id| FnSig {
            id,
            def_id: adt.def_id,
            mangled_name,
            display_name,
            params,
            ty: variant.ctor_ty,
            is_inline: false,
            span: variant.name.span(),
        });

        let mut body = Body::new();
        let start_block = body.create_block("start");

        // Initialize the `this` value based on the union kind
        let this = body.create_register(adt.ty());
        match union_def.kind {
            UnionKind::Ref => body.ins(start_block).alloc(this),
            UnionKind::Value => body.ins(start_block).stackalloc_uninit(this),
        };

        // Set the tag to this variant
        let uint = self.db.types.uint;
        let tag_field = body.create_value(uint, ValueKind::Field(this, ustr("tag")));
        let tag_value =
            body.create_value(uint, ValueKind::Const(Const::Int(variant.index as i128)));
        body.ins(start_block).store(tag_value, tag_field);

        let this_variant = body.create_value(adt.ty(), ValueKind::Variant(this, variant.id));

        Self::ctor_init_adt_fields(&mut body, start_block, this_variant, &variant.fields);

        // Return the struct
        body.ins(start_block).ret(this);

        self.mir.fns.insert(sig, Fn { sig, body });

        sig
    }

    fn adt_fields_to_fn_params(fields: &[AdtField]) -> Vec<FnParam> {
        fields
            .iter()
            .map(|field| FnParam {
                pat: Pat::Name(NamePat {
                    id: DefId::null(),
                    word: field.name,
                    mutability: Mutability::Imm,
                    vis: Vis::Public,
                    ty: field.ty,
                }),
                ty: field.ty,
            })
            .collect()
    }

    fn ctor_init_adt_fields(body: &mut Body, block: BlockId, this: ValueId, fields: &[AdtField]) {
        for (idx, field) in fields.iter().enumerate() {
            let name = field.name.name();
            let ty = field.ty;
            let field_value = body.create_value(ty, ValueKind::Field(this, name));
            let param = body.create_value(ty, ValueKind::Param(DefId::null(), idx));
            body.ins(block).store(param, field_value);
        }
    }

    fn lower_fn_sig(&mut self, fun: &hir::Fn) -> FnSigId {
        let mangled_name = if fun.kind.is_extern() {
            fun.sig.word.name()
        } else {
            ustr(&mangle::fn_name(self.db, fun))
        };

        let display_name = ustr(&self.db[fun.def_id].qpath.join());

        self.mir.fn_sigs.insert_with_key(|id| FnSig {
            id,
            def_id: fun.def_id,
            mangled_name,
            display_name,
            params: fun
                .sig
                .params
                .iter()
                .map(|p| FnParam { pat: p.pat.clone(), ty: p.ty })
                .collect(),
            ty: fun.sig.ty,
            is_inline: false,
            span: fun.sig.word.span(),
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
    pub(super) value_roots: ValueRoots,
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
            value_roots: ValueRoots::new(),
        }
    }

    fn lower_fn(mut self, sig: FnSigId, fun: &hir::Fn) {
        match &fun.kind {
            FnKind::Bare { body } => {
                // println!("fn `{}`", fun.sig.word);

                if self.cx.hir.main_fn.unwrap() == fun.id {
                    self.cx.mir.main_fn = Some(sig);
                }

                self.enter_scope(ScopeKind::Block, body.span);
                let start_block = self.body.create_block("start");
                self.position_at(start_block);

                for (idx, param) in fun.sig.params.iter().enumerate() {
                    match &param.pat {
                        Pat::Name(name) => {
                            let id = name.id;
                            let value = self.create_value(param.ty, ValueKind::Param(id, idx));
                            self.create_destroy_flag(value);
                            self.locals.insert(id, value);
                        }
                        Pat::Discard(span) => {
                            let value = self.create_untracked_value(
                                param.ty,
                                ValueKind::Param(DefId::null(), idx),
                            );
                            self.destroy_value_entirely(value, *span);
                        }
                    }
                }

                let result = self.lower_input_expr(body);
                let result = self.copy_value_before_destroy(result);

                self.exit_scope();

                if !self.body.last_inst_is_return() {
                    // If the body isn't terminating, we must push a return instruction at the
                    // for the function's last value.
                    self.ins(self.current_block).ret(result);
                }

                // println!("{}", self.value_states);
                // println!("---------------------");

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

                let result = self.lower_input_expr(&let_.value);
                self.exit_scope();

                self.body.cleanup();

                let is_const = self.body.blocks.is_empty()
                    && self.body.values().len() == 1
                    && self.body.value(result).kind.is_const();

                let kind = if is_const {
                    let ValueKind::Const(result) = self.body.values.swap_remove(result).kind else {
                        unreachable!()
                    };
                    GlobalKind::Const(result)
                } else {
                    GlobalKind::Static(StaticGlobal { body: self.body, result })
                };

                let id = self.cx.mir.globals.insert_with_key(|id| Global {
                    id,
                    def_id: name.id,
                    name: full_name.into(),
                    ty,
                    kind,
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
                match &let_.pat {
                    Pat::Name(name) => {
                        let init = self.lower_input_expr(&let_.value);
                        let value =
                            self.push_inst_with(let_.ty, ValueKind::Local(name.id), |value| {
                                Inst::StackAlloc { value, init: Some(init) }
                            });
                        self.create_destroy_flag(value);
                        self.locals.insert(name.id, value);
                    }
                    Pat::Discard(span) => {
                        let init = self.lower_expr(&let_.value);
                        self.destroy_value_entirely(init, *span);
                        self.try_move(init, let_.value.span);
                    }
                }

                self.const_unit()
            }
            hir::ExprKind::Assign(assign) => {
                match &assign.lhs.kind {
                    hir::ExprKind::Index(idx) => {
                        self.lower_slice_assign(assign, idx, expr.span);
                    }
                    _ => {
                        self.lower_assign(assign, expr.span);
                    }
                }

                self.const_unit()
            }
            hir::ExprKind::Swap(swap) => match &swap.lhs.kind {
                hir::ExprKind::Index(idx) => self.lower_slice_swap(swap, idx, expr.span),
                _ => self.lower_swap(swap, expr.span),
            },
            hir::ExprKind::Match(match_) => {
                let output = self.push_inst_with_register(expr.ty, |value| Inst::StackAlloc {
                    value,
                    init: None,
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

                if let Ok((decision, new_guards)) = pmatch::compile(self, rows, expr.span) {
                    for (new_guard, old_guard) in new_guards {
                        state.guards.insert(new_guard, state.guards[&old_guard]);
                    }

                    self.lower_decision(&mut state, decision, self.current_block, vec![]);
                }

                self.position_at(state.join_block);

                output
            }
            hir::ExprKind::Loop(loop_) => {
                let loop_start = self.body.create_block("loop_start");
                let loop_end = self.body.create_block("loop_end");

                self.enter_scope(ScopeKind::Loop(LoopScope::new(loop_end)), expr.span);

                self.ins(self.current_block).br(loop_start);
                self.position_at(loop_start);

                let (loop_start, loop_body) = if let Some(cond_expr) = &loop_.cond {
                    let loop_body = self.body.create_block("loop_body");
                    let cond = self.lower_input_expr(cond_expr);
                    self.ins(loop_start).brif(cond, loop_body, Some(loop_end));
                    (loop_start, loop_body)
                } else {
                    (loop_start, loop_start)
                };

                self.position_at(loop_body);
                self.lower_expr(&loop_.expr);

                let is_connected = self.in_connected_block();

                if is_connected {
                    self.check_loop_moves();
                }

                self.exit_scope();

                if is_connected {
                    self.ins(self.current_block).br(loop_start);
                }

                self.position_at(loop_end);
                self.const_unit()
            }
            hir::ExprKind::Break => {
                self.destroy_loop_values(expr.span);

                let end_block =
                    self.closest_loop_scope().expect("to be inside a loop block").end_block;
                self.ins(self.current_block).br(end_block);

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

                self.move_out(result, block.exprs.last().map_or(expr.span, |e| e.span));
                let result = self.copy_value_before_destroy(result);
                self.exit_scope();

                result
            }
            hir::ExprKind::Unsafe(uns) => self.lower_expr(&uns.expr),
            hir::ExprKind::Return(ret) => {
                let value = self.lower_input_expr(&ret.expr);
                self.push_return(value, expr.span);
                self.const_unit()
            }
            hir::ExprKind::Call(call) => {
                let entered = self.enter_call_scope(expr.span);
                let callee = self.lower_input_expr(&call.callee);
                let builtin = if let ValueKind::Fn(id) = &self.body.value(callee).kind {
                    self.cx.db.builtins.get(&self.cx.mir.fn_sigs[*id].def_id).copied()
                } else {
                    None
                };

                let call_result = if let Some(builtin) = builtin {
                    let args = self.lower_builtin_call_args(&call.args);
                    self.lower_builtin_call(expr, builtin, args)
                } else {
                    let args = self.lower_call_args(&call.args);
                    self.push_inst_with_register(expr.ty, |value| Inst::Call {
                        value,
                        callee,
                        args,
                        span: expr.span,
                    })
                };

                self.exit_call_scope(call_result, entered);

                call_result
            }
            hir::ExprKind::Unary(un) => match un.op {
                UnOp::Neg | UnOp::Not => {
                    let inner = self.lower_input_expr(&un.expr);
                    self.push_inst_with_register(expr.ty, |value| Inst::Unary {
                        value,
                        inner,
                        op: un.op,
                    })
                }
                UnOp::Ref(_) => {
                    let inner = self.lower_expr(&un.expr);
                    self.try_use(inner, un.expr.span);
                    self.create_ref(inner, expr.ty, expr.span)
                }
            },
            hir::ExprKind::Binary(bin) => {
                let lhs = self.lower_input_expr(&bin.lhs);
                let rhs = self.lower_input_expr(&bin.rhs);

                let value = self.create_value(expr.ty, ValueKind::Register(None));
                self.ins(self.current_block).binary(value, lhs, rhs, bin.op, expr.span);
                value
            }
            hir::ExprKind::Deref(deref) => {
                let ptr = self.lower_expr(&deref.expr);
                self.try_use(ptr, deref.expr.span);
                self.lower_deref(ptr, expr.ty)
            }
            hir::ExprKind::Convert(conv) => {
                let source = self.lower_input_expr(&conv.expr);

                self.push_inst_with_register(conv.target, |value| Inst::Convert {
                    value,
                    source,
                    target: conv.target,
                    span: expr.span,
                })
            }
            hir::ExprKind::Cast(trans) => {
                let source = self.lower_input_expr(&trans.expr);

                self.push_inst_with_register(trans.target, |value| Inst::Cast {
                    value,
                    source,
                    target: trans.target,
                    span: expr.span,
                })
            }
            hir::ExprKind::Field(access) => {
                let name = access.field.name();
                let value = self.lower_expr(&access.expr);
                self.field_or_create(value, name, expr.ty)
            }
            hir::ExprKind::Index(idx) => {
                let SliceIndexResult { elem, .. } = self.lower_slice_index(idx, expr.span);

                if self.ty_of(elem).is_move(self.cx.db) {
                    // An element can never be moved out of its slice
                    self.value_states.set_cannot_move(elem, CannotMove::SliceIndex);
                }

                elem
            }
            hir::ExprKind::Slice(slice) => self.lower_slice_slice(expr, slice),
            hir::ExprKind::Name(name) => self.lower_name(name.id, &name.instantiation),
            hir::ExprKind::Variant(variant) => {
                let id = self.cx.get_or_create_variant_ctor(variant.id);
                let value = self.create_value(self.cx.mir.fn_sigs[id].ty, ValueKind::Fn(id));

                if !variant.instantiation.is_empty() {
                    self.body.create_instantation(value, variant.instantiation.clone());
                }

                value
            }
            hir::ExprKind::SliceLit(lit) => {
                let uint = self.cx.db.types.uint;
                let cap = if let Some(cap) = &lit.cap {
                    self.lower_expr(cap)
                } else {
                    self.const_int(uint, lit.exprs.len() as _)
                };

                let slice =
                    self.push_inst_with_register(expr.ty, |value| Inst::SliceAlloc { value, cap });

                for (index, expr) in lit.exprs.iter().enumerate() {
                    let value = self.lower_input_expr(expr);

                    let index = self.const_int(uint, index as _);
                    self.ins(self.current_block).slice_store_unchecked(slice, index, value);
                }

                if !lit.exprs.is_empty() {
                    // slice.len = lit.exprs.len()
                    let value = self.const_int(uint, lit.exprs.len() as i128);
                    let len_field = self.create_untracked_value(
                        uint,
                        ValueKind::Field(slice, ustr(sym::field::LEN)),
                    );
                    self.ins(self.current_block).store(value, len_field);
                }

                slice
            }
            hir::ExprKind::StrLit(lit) => self.lower_const(&Const::Str(*lit), expr.ty),
            hir::ExprKind::CharLit(lit) => self.lower_const(&Const::Int(*lit as i128), expr.ty),
            #[allow(clippy::cast_possible_wrap)]
            hir::ExprKind::IntLit(lit) => self.lower_const(&Const::Int(*lit as i128), expr.ty),
            hir::ExprKind::FloatLit(lit) => self.lower_const(&Const::Float(*lit), expr.ty),
            hir::ExprKind::BoolLit(lit) => self.lower_const(&Const::Bool(*lit), expr.ty),
        }
    }

    fn lower_call_args(&mut self, args: &[hir::CallArg]) -> Vec<ValueId> {
        let mut new_args = vec![];

        for arg in args {
            let idx = arg.index.expect("arg index to be resolved");
            let value = self.lower_input_expr(&arg.expr);
            new_args.push((idx, value));
        }

        new_args.sort_by_key(|(idx, _)| *idx);
        new_args.into_iter().map(|(_, arg)| arg).collect()
    }

    // This is very similar to `lower_call_args`, the only difference is that ref
    // args are not incremented.
    fn lower_builtin_call_args(&mut self, args: &[hir::CallArg]) -> Vec<ValueId> {
        let mut new_args = vec![];

        for arg in args {
            let idx = arg.index.expect("arg index to be resolved");
            let value = self.lower_expr(&arg.expr);
            self.try_move(value, arg.expr.span);
            new_args.push((idx, value));
        }

        new_args.sort_by_key(|(idx, _)| *idx);
        new_args.into_iter().map(|(_, arg)| arg).collect()
    }

    fn lower_input_expr(&mut self, expr: &hir::Expr) -> ValueId {
        let value = self.lower_expr(expr);
        let ty = self.ty_of(value);

        if ty.is_move(self.cx.db) {
            self.try_move(value, expr.span);
            return value;
        }

        let is_register = self.body.value(value).kind.is_register();

        // When a reference is moved, its refcount is incremented.
        if !is_register && ty.is_ref() {
            self.ins(self.current_block).incref(value);
            return value;
        }

        // When a value struct type is moved, its reference fields are incremented
        if !is_register && ty.is_value_struct(self.cx.db) {
            self.copy_value_type(value);
            return value;
        }

        self.set_moved_with_fields(value, expr.span);

        value
    }

    fn lower_assign_rhs(
        &mut self,
        assign: &hir::Assign,
        lhs: impl FnOnce(&mut Self) -> ValueId,
        span: Span,
    ) -> ValueId {
        let rhs = self.lower_input_expr(&assign.rhs);

        if let Some(op) = assign.op {
            let lhs = lhs(self);
            let result = self.create_value(self.ty_of(rhs), ValueKind::Register(None));
            self.ins(self.current_block).binary(result, lhs, rhs, op, span);
            result
        } else {
            rhs
        }
    }

    fn lower_deref(&mut self, ptr: ValueId, pointee_ty: Ty) -> ValueId {
        self.create_value(pointee_ty, ValueKind::Deref(ptr))
    }

    fn lower_assign(&mut self, assign: &hir::Assign, span: Span) {
        let lhs = self.lower_expr(&assign.lhs);
        self.try_use(lhs, assign.lhs.span);
        let rhs = self.lower_assign_rhs(assign, |_| lhs, span);

        self.check_assign_mutability(AssignKind::Assign, lhs, span);

        self.destroy_value_entirely(lhs, assign.lhs.span);
        self.set_owned(lhs);
        self.ins(self.current_block).store(rhs, lhs);
    }

    fn lower_swap(&mut self, swap: &hir::Swap, span: Span) -> ValueId {
        let lhs = self.lower_expr(&swap.lhs);
        self.try_use(lhs, swap.lhs.span);
        let rhs = self.lower_input_expr(&swap.rhs);

        self.check_assign_mutability(AssignKind::Swap, lhs, span);

        let old_lhs = self.push_inst_with_register(self.ty_of(lhs), |value| Inst::StackAlloc {
            value,
            init: Some(lhs),
        });
        self.create_destroy_flag(old_lhs);
        self.ins(self.current_block).store(rhs, lhs);

        old_lhs
    }

    fn lower_slice_assign(&mut self, assign: &hir::Assign, idx: &hir::Index, span: Span) {
        let SliceIndexResult { slice, index, elem } = self.lower_slice_index(idx, span);
        let rhs = self.lower_assign_rhs(assign, |_| elem, span);

        self.check_slice_assign_mutability(AssignKind::Assign, slice, span);

        self.destroy_value_entirely(elem, idx.expr.span);
        self.ins(self.current_block).slice_store(slice, index, rhs, assign.lhs.span);
    }

    fn lower_slice_swap(&mut self, swap: &hir::Swap, idx: &hir::Index, span: Span) -> ValueId {
        let SliceIndexResult { slice, index, elem } = self.lower_slice_index(idx, span);
        let rhs = self.lower_input_expr(&swap.rhs);

        self.check_slice_assign_mutability(AssignKind::Swap, slice, span);

        let old_elem = self.push_inst_with_register(self.ty_of(elem), |value| Inst::StackAlloc {
            value,
            init: Some(elem),
        });
        self.create_destroy_flag(old_elem);
        self.ins(self.current_block).slice_store(slice, index, rhs, swap.lhs.span);

        old_elem
    }

    fn lower_slice_index(&mut self, idx: &hir::Index, span: Span) -> SliceIndexResult {
        let slice = self.lower_expr(&idx.expr);
        self.try_use(slice, idx.expr.span);
        let index = self.lower_input_expr(&idx.index);

        let elem_ty = self.ty_of(slice).auto_deref().slice_elem().unwrap();

        let elem = self.create_value(elem_ty, ValueKind::Register(None));
        self.ins(self.current_block).slice_index(elem, slice, index, span);

        // We must set the slice element as moved, so that we don't destroy it
        self.set_moved(elem, span);
        self.value_roots.insert(slice, elem);

        SliceIndexResult { slice, index, elem }
    }

    fn lower_slice_slice(&mut self, expr: &hir::Expr, slice: &hir::Slice) -> ValueId {
        let og_slice = self.lower_expr(&slice.expr);
        self.try_use(og_slice, slice.expr.span);

        let uint = self.cx.db.types.uint;

        let low = if let Some(low) = &slice.low {
            let value = self.lower_expr(low);
            self.try_move(value, low.span);
            value
        } else {
            self.const_int(uint, 0)
        };

        let high = if let Some(high) = &slice.high {
            let value = self.lower_expr(high);
            self.try_move(value, high.span);
            value
        } else {
            self.create_untracked_value(uint, ValueKind::Field(og_slice, ustr(sym::field::LEN)))
        };

        let sliced = self.create_value(expr.ty, ValueKind::Register(None));
        self.ins(self.current_block).slice_slice(sliced, og_slice, low, high, expr.span);

        // A re-sliced slice can never move out of its original slice
        self.set_moved(sliced, expr.span);
        self.value_states.set_cannot_move(sliced, CannotMove::SliceSlice);

        self.value_roots.insert(og_slice, sliced);

        sliced
    }

    fn lower_builtin_call(
        &mut self,
        expr: &hir::Expr,
        builtin: Builtin,
        args: Vec<ValueId>,
    ) -> ValueId {
        match builtin {
            Builtin::SliceGrow => {
                debug_assert_eq!(args.len(), 2);

                // We must get the root value of this ref, so that it is assigned to
                let slice = self.value_roots.root_of(args[0]);
                let new_cap = args[1];

                self.push_inst_with_register(expr.ty, |value| Inst::RtCall {
                    value,
                    kind: RtCallKind::SliceGrow { slice, new_cap },
                    span: expr.span,
                })
            }
            Builtin::Forget => {
                debug_assert_eq!(args.len(), 1);

                // We must get the root value of this ref, so that we can forget it
                let root = self.value_roots.root_of(args[0]);

                for scope in &mut self.scopes {
                    scope.created_values.shift_remove(&root);
                }

                self.const_unit()
            }
            Builtin::Panic => {
                debug_assert_eq!(args.len(), 1);

                let msg = args[0];

                self.push_inst_with_register(expr.ty, |value| Inst::RtCall {
                    value,
                    kind: RtCallKind::Panic { msg },
                    span: expr.span,
                })
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
                self.destroy_match_values(state, values);
                self.lower_decision_body(state, self.current_block, body.block);
                body.block
            }
            pmatch::Decision::Err => unreachable!(),
            pmatch::Decision::Guard { guard, body, fallback } => {
                self.lower_decision_guard(state, guard, body, *fallback, parent_block, values)
            }
            pmatch::Decision::Switch { cond, cases, fallback } => match cases[0].ctor {
                pmatch::Ctor::Unit => self.lower_decision_unit(state, cases, parent_block, values),
                pmatch::Ctor::True | pmatch::Ctor::False => {
                    self.lower_decision_bool(state, cond, cases, parent_block, values)
                }
                pmatch::Ctor::Int(_) | pmatch::Ctor::Str(_) => self.lower_decision_lit(
                    state,
                    cond,
                    cases,
                    *fallback.unwrap(),
                    parent_block,
                    values,
                ),
                pmatch::Ctor::Struct(_) => {
                    self.lower_decision_struct(state, cond, cases, parent_block, values)
                }
                pmatch::Ctor::Variant(_) => {
                    self.lower_decision_variant(state, cond, cases, parent_block, values)
                }
            },
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
        let block = self.body.create_block("match_test");

        self.ins(parent_block).br(block);

        let blocks: Vec<_> = cases
            .into_iter()
            .map(|case| self.lower_decision(state, case.decision, block, values.clone()))
            .collect();

        self.ins(block).brif(cond, blocks[1], Some(blocks[0]));

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
        let blocks = self.body.create_blocks("match_test", cases.len());

        self.body.create_edge(parent_block, blocks[0]);
        self.body.connect_blocks(&blocks);
        values.push(cond);

        let fallback_block =
            self.lower_decision(state, fallback, *blocks.last().unwrap(), values.clone());

        for (idx, case) in cases.into_iter().enumerate() {
            let test_block = blocks[idx];

            let else_block = blocks.get(idx + 1).copied().unwrap_or(fallback_block);
            self.body.create_edge(test_block, else_block);

            let result_value =
                self.create_untracked_value(self.cx.db.types.bool, ValueKind::Register(None));

            self.position_at(test_block);
            let lit_value =
                self.create_untracked_value(self.ty_of(cond), ValueKind::Const(case.ctor.into()));

            self.ins(test_block).binary(
                result_value,
                cond,
                lit_value,
                BinOp::Cmp(CmpOp::Eq),
                state.span,
            );

            let then_block = self.lower_decision(state, case.decision, test_block, values.clone());

            self.position_at(test_block);

            self.ins(test_block).brif(result_value, then_block, Some(else_block));
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

        if let Some(action) = self.get_value_action(cond) {
            for value in case.values {
                state.actions.insert(value, action);
            }
        }

        self.lower_decision(state, case.decision, parent_block, values)
    }

    fn lower_decision_variant(
        &mut self,
        state: &mut DecisionState,
        cond: ValueId,
        cases: Vec<pmatch::Case>,
        parent_block: BlockId,
        mut values: Vec<ValueId>,
    ) -> BlockId {
        let test_block = self.body.create_block("match_test");
        let mut blocks = vec![];

        self.body.create_edge(parent_block, test_block);
        values.push(cond);

        let action = self.get_value_action(cond);

        for case in cases {
            let block = self.body.create_block("match_variant_case");
            self.body.create_edge(test_block, block);
            blocks.push(block);

            if let Some(action) = action {
                for value in case.values {
                    state.actions.insert(value, action);
                }
            }

            self.lower_decision(state, case.decision, block, values.clone());
        }

        let uint = self.cx.db.types.uint;
        let tag_field = self.create_untracked_value(uint, ValueKind::Field(cond, ustr("tag")));
        self.ins(test_block).switch(tag_field, blocks);

        test_block
    }

    fn get_value_action(&self, cond: ValueId) -> Option<ValueAction> {
        let cond_ty = self.ty_of(cond);

        if cond_ty.is_move(self.cx.db) {
            Some(ValueAction::Move(cond))
        } else if cond_ty.is_ref() {
            Some(ValueAction::IncRef(cond))
        } else {
            None
        }
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
                    let binding_value =
                        self.push_inst_with(binding_ty, ValueKind::Local(id), |value| {
                            Inst::StackAlloc { value, init: Some(source) }
                        });
                    self.create_destroy_flag(binding_value);
                    self.locals.insert(id, binding_value);

                    match state.actions.get(&source) {
                        Some(&ValueAction::Move(_)) => {
                            self.try_move(source, span);
                        }
                        Some(&ValueAction::IncRef(_)) => {
                            self.set_moved(source, span);
                            self.ins(self.current_block).incref(binding_value);
                        }
                        None => {
                            self.set_moved(source, span);
                        }
                    }
                }
                pmatch::Binding::Discard(source, span) => match state.actions.get(&source) {
                    Some(&ValueAction::Move(_)) => {
                        self.destroy_value_entirely(source, span);
                        self.try_move(source, span);
                    }
                    Some(&ValueAction::IncRef(_)) => {
                        self.set_moved(source, span);
                    }
                    None => {
                        self.destroy_value_entirely(source, span);
                        self.set_moved(source, span);
                    }
                },
            }
        }
    }

    fn destroy_match_values(&mut self, state: &DecisionState, mut values: Vec<ValueId>) {
        while let Some(value) = values.pop() {
            if matches!(self.value_state(value), ValueState::Moved(..)) {
                continue;
            }

            match state.actions.get(&value) {
                Some(&ValueAction::Move(parent) | &ValueAction::IncRef(parent))
                    if self.value_is_moved(parent) => {}
                Some(&ValueAction::IncRef(_)) => {
                    self.set_moved(value, state.span);
                }
                _ => {
                    self.set_moved(value, state.span);
                    self.destroy_and_set_flag(value, false, state.span);
                }
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

        // Removing the expression makes sure that the code for a given block is only
        // compiled once.
        let Some(expr) = state.bodies.remove(&start_block) else {
            self.exit_scope();
            return start_block;
        };

        self.position_at(start_block);
        let value = self.lower_input_expr(expr);
        self.exit_scope();

        if self.in_connected_block() {
            self.ins(self.current_block).store(value, state.output).br(state.join_block);
        }

        start_block
    }

    fn lower_name(&mut self, id: DefId, instantiation: &Instantiation) -> ValueId {
        let value = match self.cx.db[id].kind.as_ref() {
            DefKind::Fn(_) => {
                let id = self.cx.id_to_fn_sig[&id];
                self.create_value(self.cx.mir.fn_sigs[id].ty, ValueKind::Fn(id))
            }
            DefKind::ExternGlobal | DefKind::Global => {
                let id = self.cx.lower_global(id);
                self.create_value(self.cx.mir.globals[id].ty, ValueKind::Global(id))
            }
            DefKind::Variable => self.locals[&id],
            DefKind::Adt(adt_id) => {
                let id = self.cx.get_or_create_struct_ctor(*adt_id);
                self.create_value(self.cx.mir.fn_sigs[id].ty, ValueKind::Fn(id))
            }
            DefKind::BuiltinTy(_) => unreachable!("{:?}", &self.cx.db[id]),
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
        let cond = self.lower_input_expr(guard_expr);
        self.exit_scope();

        for (id, old_value, new_value) in restore_bindings {
            let new_state = self.value_state(new_value);
            self.set_value_state(old_value, new_state);
            self.locals.insert(id, old_value);
        }

        let guard_join = self.current_block;
        let fallback_block = self.lower_decision(state, fallback, guard_join, values.clone());

        self.position_at(guard_join);
        self.ins(guard_join).brif(cond, body.block, Some(fallback_block));

        self.lower_decision_bindings(state, body.block, body.bindings);
        self.destroy_match_values(state, values);
        self.lower_decision_body(state, self.current_block, body.block);

        guard
    }

    pub fn lower_const(&mut self, value: &Const, ty: Ty) -> ValueId {
        match value {
            Const::Str(lit) => {
                self.push_inst_with_register(ty, |value| Inst::StrLit { value, lit: *lit })
            }
            Const::Int(value) => self.const_int(ty, *value),
            Const::Float(value) => self.create_value(ty, ValueKind::Const(Const::Float(*value))),
            Const::Bool(value) => self.const_bool(*value),
            Const::Unit => self.const_unit(),
        }
    }

    pub fn const_unit(&mut self) -> ValueId {
        self.create_value(self.cx.db.types.unit, ValueKind::Const(Const::Unit))
    }

    pub fn const_bool(&mut self, value: bool) -> ValueId {
        self.create_value(self.cx.db.types.bool, ValueKind::Const(Const::Bool(value)))
    }

    pub fn const_int(&mut self, ty: Ty, value: i128) -> ValueId {
        self.create_value(ty, ValueKind::Const(Const::Int(value)))
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
        self.current_block_mut().push_inst(f(value));
        value
    }

    fn push_return(&mut self, value: ValueId, span: Span) {
        self.destroy_all_values(span);
        self.ins(self.current_block).ret(value);
    }

    pub fn create_untracked_value(&mut self, ty: Ty, kind: ValueKind) -> ValueId {
        let value = self.body.create_value(ty, kind);
        self.set_owned(value);
        value
    }

    // Used to keep a copy of a value that its parent could potentially be destroyed
    pub fn copy_value_before_destroy(&mut self, value: ValueId) -> ValueId {
        let any_parent_destroyed = self
            .walk_parents(value, |this, parent, _| -> Result<(), ()> {
                if this.value_is_partially_moved(parent) {
                    Err(())
                } else {
                    Ok(())
                }
            })
            .is_err();

        if !any_parent_destroyed {
            return value;
        }

        let sa = self.create_untracked_value(self.ty_of(value), ValueKind::Register(None));
        self.ins(self.current_block).stackalloc(sa, value);
        sa
    }

    pub fn copy_value_type(&mut self, old_value: ValueId) {
        self.walk_fields(old_value, |this, field| {
            if this.ty_of(field).is_ref() {
                this.ins(this.current_block).incref(field);
            }
            Ok(())
        })
        .unwrap();
    }

    pub fn create_value(&mut self, ty: Ty, kind: ValueKind) -> ValueId {
        let value = self.body.create_value(ty, kind);
        self.set_owned(value);
        self.scope_mut().created_values.insert(value);
        self.create_value_fields(value);
        value
    }

    pub fn create_ref(&mut self, to_clone: ValueId, ty: Ty, span: Span) -> ValueId {
        if ty.is_mut_ref() {
            self.check_ref_mutability(to_clone, span);
        }

        let ref_value = self
            .push_inst_with_register(ty, |value| Inst::StackAlloc { value, init: Some(to_clone) });

        self.create_destroy_flag(ref_value);
        self.ins(self.current_block).incref(ref_value);

        self.value_roots.insert(to_clone, ref_value);

        ref_value
    }

    pub fn create_value_fields(&mut self, value: ValueId) {
        let value = self.body.value(value);

        if let TyKind::Adt(adt_id, targs) = value.ty.kind() {
            let adt = &self.cx.db[*adt_id];

            if let AdtKind::Struct(struct_def) = &adt.kind {
                let value = value.id;
                let instantiation = adt.instantiation(targs);
                let mut folder = instantiation.folder();

                // In order for fields to be destroyed in field-order,
                // we must introduce them in reverse order (since values are destroyed in
                // reverse).
                let mut fields = FxHashMap::default();

                for f in struct_def.fields.iter().rev() {
                    let name = f.name.name();
                    let ty = folder.fold(f.ty);

                    let should_destroy = f.ty.is_ref() || f.ty.needs_free(self.cx.db);

                    if should_destroy || f.ty.is_value_struct(self.cx.db) {
                        let field_value = self.create_value(ty, ValueKind::Field(value, name));
                        if should_destroy {
                            self.create_destroy_flag(field_value);
                            fields.insert(name, field_value);
                        }
                    }
                }

                self.fields.insert(value, fields);
            }
        }
    }

    pub(super) fn walk_fields(
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

    pub(super) fn walk_parents<E>(
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
        if let Some(parent) = self.body.parent(value) {
            f(self, parent, value)?;
            self.walk_parents_aux(parent, f)
        } else {
            Ok(())
        }
    }

    pub(super) fn value_depth(&self, value: ValueId) -> usize {
        self.scopes.iter().find(|s| s.created_values.contains(&value)).map_or(0, |s| s.depth)
    }

    #[inline]
    pub fn position_at(&mut self, id: BlockId) {
        self.current_block = id;
    }

    #[inline]
    pub fn ins(&mut self, block: BlockId) -> InstBuilder {
        self.body.ins(block)
    }

    pub fn apply_coercions_to_expr(&mut self, expr: &hir::Expr, value: ValueId) -> ValueId {
        if let Some(coercions) = self.cx.hir.coercions.get(&expr.id) {
            self.apply_coercions(coercions, value, expr.span)
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
                CoercionKind::NeverToAny | CoercionKind::RefToOwned => coerced_value,
                CoercionKind::AnyToUnit => self.const_unit(),
                CoercionKind::IntPromotion => {
                    self.push_inst_with_register(coercion.target, |value| Inst::Convert {
                        value,
                        source: coerced_value,
                        target: coercion.target,
                        span,
                    })
                }
                CoercionKind::MutRefToImm => {
                    self.set_moved(coerced_value, span);
                    self.push_inst_with_register(coercion.target, |value| Inst::StackAlloc {
                        value,
                        init: Some(coerced_value),
                    })
                }
                CoercionKind::OwnedToRef => self.create_ref(coerced_value, coercion.target, span),
            };

            self.body.value_mut(coerced_value).ty = coercion.target;
        }

        coerced_value
    }

    #[track_caller]
    pub(super) fn scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    #[track_caller]
    pub(super) fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn enter_scope(&mut self, kind: ScopeKind, span: Span) {
        self.scopes.push(self.new_scope(kind, span));
    }

    fn exit_scope(&mut self) {
        self.destroy_scope_values();
        let scope = self.scopes.pop().expect("cannot exit the root scope");

        if let Some(curr_scope) = self.scopes.last_mut() {
            curr_scope.created_values.extend(&scope.moved_out);
        }
    }

    fn enter_call_scope(&mut self, span: Span) -> bool {
        // We don't introduce additional call scopes for call chains
        if matches!(self.scope().kind, ScopeKind::Call) {
            return false;
        }

        self.enter_scope(ScopeKind::Call, span);
        true
    }

    fn exit_call_scope(&mut self, result: ValueId, entered: bool) {
        if !entered {
            return;
        }

        self.move_out(result, self.scope().span);
        self.exit_scope();
    }

    fn new_scope(&self, kind: ScopeKind, span: Span) -> Scope {
        let (depth, loop_depth) =
            if let Some(s) = self.scopes.last() { (s.depth + 1, s.loop_depth) } else { (0, 0) };
        let loop_depth = if let ScopeKind::Loop(..) = kind { loop_depth + 1 } else { loop_depth };

        Scope {
            kind,
            depth,
            loop_depth,
            span,
            created_values: IndexSet::default(),
            moved_out: IndexSet::default(),
        }
    }

    fn closest_loop_scope(&self) -> Option<&LoopScope> {
        self.scopes.iter().rev().find_map(|s| s.kind.as_loop())
    }

    pub(super) fn closest_loop_scope_mut(&mut self) -> Option<&mut LoopScope> {
        self.scopes.iter_mut().rev().find_map(|s| s.kind.as_loop_mut())
    }

    pub(super) fn value_name(&self, value: ValueId) -> String {
        match &self.body.value(value).kind {
            ValueKind::Register(_) | ValueKind::Const(_) => "temporary value".to_string(),
            _ => format!("`{}`", self.value_name_aux(value)),
        }
    }

    fn value_name_aux(&self, value: ValueId) -> String {
        match &self.body.value(value).kind {
            ValueKind::Param(id, _) | ValueKind::Local(id) => self.cx.db[*id].name.to_string(),
            ValueKind::Global(id) => self.cx.mir.globals[*id].name.to_string(),
            ValueKind::Fn(id) => self.cx.mir.fn_sigs[*id].mangled_name.to_string(),
            ValueKind::Field(parent, field) => {
                format!("{}.{}", self.value_name_aux(*parent), field)
            }
            ValueKind::Variant(parent, id) => {
                format!("{}.{}", self.value_name_aux(*parent), self.cx.db[*id].name)
            }
            ValueKind::Deref(parent) => {
                format!("{}.0", self.value_name_aux(*parent))
            }
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

    pub fn emit_result(&mut self, result: DiagnosticResult<()>) {
        if let Err(diagnostic) = result {
            self.cx.diagnostics.push(diagnostic);
        }
    }

    pub fn ty_of(&self, value: ValueId) -> Ty {
        self.body.value(value).ty
    }

    pub fn field_or_create(&mut self, of: ValueId, name: Ustr, ty: Ty) -> ValueId {
        self.field(of, name)
            .unwrap_or_else(|| self.create_untracked_value(ty, ValueKind::Field(of, name)))
    }

    pub fn field(&self, of: ValueId, name: Ustr) -> Option<ValueId> {
        self.fields.get(&of).and_then(|fields| fields.get(&name)).copied()
    }
}

#[derive(Debug, Clone)]
pub(super) struct Scope {
    pub(super) kind: ScopeKind,
    pub(super) depth: usize,
    pub(super) loop_depth: usize,
    pub(super) span: Span,

    // Values that were created in this scope
    pub(super) created_values: IndexSet<ValueId>,
    pub(super) moved_out: IndexSet<ValueId>,
}

#[derive(Debug, Clone)]
pub(super) enum ScopeKind {
    Block,
    Call,
    Loop(LoopScope),
}

impl ScopeKind {
    #[must_use]
    pub(super) fn as_loop(&self) -> Option<&LoopScope> {
        if let Self::Loop(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub(super) fn as_loop_mut(&mut self) -> Option<&mut LoopScope> {
        if let Self::Loop(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct LoopScope {
    pub(super) end_block: BlockId,
    pub(super) moved_in: FxHashMap<ValueId, Span>,
}

impl LoopScope {
    fn new(end_block: BlockId) -> Self {
        Self { end_block, moved_in: FxHashMap::default() }
    }
}

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

    /// Actions to take for values in the decision tree
    actions: FxHashMap<ValueId, ValueAction>,

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
            actions: FxHashMap::default(),
            span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum ValueAction {
    Move(ValueId),
    IncRef(ValueId),
}

#[derive(Debug, Clone, Copy)]
pub(super) enum AssignKind {
    Assign,
    Swap,
}

#[derive(Debug, Clone, Copy)]
struct SliceIndexResult {
    slice: ValueId,
    index: ValueId,
    elem: ValueId,
}

#[derive(Debug)]
pub(super) struct ValueRoots(FxHashMap<ValueId, ValueId>);

impl ValueRoots {
    fn new() -> Self {
        Self(FxHashMap::default())
    }

    pub(super) fn insert(&mut self, root: ValueId, value: ValueId) {
        let root_root = self.root_of(root);
        self.0.insert(value, root_root);
    }

    pub(super) fn root_of(&self, value: ValueId) -> ValueId {
        self.0.get(&value).copied().unwrap_or(value)
    }
}
