use std::{fmt, mem, ops};

use indexmap::IndexMap;
use itertools::Itertools as _;
use ustr::{ustr, Ustr};

use crate::{
    db::{Db, DefId, DefKind},
    diagnostics::{Diagnostic, Label},
    hir,
    hir::{FnKind, Hir},
    middle::{Mutability, NamePat, Pat, Vis},
    mir::*,
    span::Spanned,
    ty::{
        coerce::{CoercionKind, Coercions},
        Instantiation, Ty, TyKind,
    },
};

pub fn lower(db: &mut Db, hir: &Hir) -> Mir {
    Lower::new(db, hir).lower_all()
}

struct Lower<'db> {
    db: &'db mut Db,
    hir: &'db Hir,
    mir: Mir,
    id_to_fn_sig: FxHashMap<DefId, FnSigId>,
    id_to_global: FxHashMap<DefId, GlobalId>,
}

impl<'db> Lower<'db> {
    fn new(db: &'db mut Db, hir: &'db Hir) -> Self {
        Self {
            db,
            hir,
            mir: Mir::new(),
            id_to_fn_sig: FxHashMap::default(),
            id_to_global: FxHashMap::default(),
        }
    }

    fn lower_all(mut self) -> Mir {
        for fun in &self.hir.fns {
            let def = &self.db[fun.def_id];
            let is_extern = fun.kind.is_extern();
            let name = if is_extern {
                def.name
            } else {
                hir::mangle::mangle_fn_name(self.db, fun)
            };
            let sig = self.lower_fn_sig(&fun.sig, &fun.kind, name, def.ty);
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

    fn get_or_create_struct_ctor(&mut self, sid: StructId) -> FnSigId {
        if let Some(sig_id) = self.mir.struct_ctors.get(&sid) {
            return *sig_id;
        }

        let struct_info = &self.db[sid];
        let name = ustr(
            &self.db[struct_info.def_id]
                .qpath
                .clone()
                .child(ustr("ctor"))
                .join_with("_"),
        );

        let sig_id = self.mir.fn_sigs.insert_with_key(|id| FnSig {
            id,
            name,
            params: struct_info
                .fields
                .iter()
                .map(|f| FnParam {
                    pat: Pat::Name(NamePat {
                        id: DefId::INVALID,
                        word: f.name,
                        vis: Vis::Private,
                        mutability: Mutability::Imm,
                    }),
                    ty: f.ty,
                })
                .collect(),
            ty: struct_info.ctor_ty,
            is_extern: false,
            is_c_variadic: false,
            span: struct_info.name.span(),
        });

        // TODO: doesn't work for polymorphic structs...
        self.mir.struct_ctors.insert(sid, sig_id);

        sig_id
    }

    fn lower_fn_sig(
        &mut self,
        sig: &hir::FnSig,
        kind: &hir::FnKind,
        name: Ustr,
        ty: Ty,
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
            ty,
            is_extern,
            is_c_variadic,
            span: sig.word.span(),
        })
    }

    fn lower_fn_body(&mut self, sig: FnSigId, f: &hir::Fn) {
        LowerBody::new(self).lower_fn(sig, f);
    }
}

struct LowerBody<'cx, 'db> {
    cx: &'cx mut Lower<'db>,
    body: Body,
    value_states: ValueStates,
    scopes: Vec<Scope>,
    current_block: BlockId,
    locals: FxHashMap<DefId, ValueId>,
    members: FxHashMap<ValueId, FxHashMap<Ustr, ValueId>>,
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
            members: FxHashMap::default(),
        }
    }

    fn lower_fn(mut self, sig: FnSigId, fun: &hir::Fn) {
        match &fun.kind {
            FnKind::Bare { body } => {
                if self.cx.db.main_function_id() == Some(fun.def_id) {
                    self.cx.mir.main_fn = Some(sig);
                }

                self.enter_scope(ScopeKind::Block, body.span);
                let start_blk = self.body.create_block("start");
                self.position_at(start_blk);

                for param in &fun.sig.params {
                    match &param.pat {
                        Pat::Name(name) => {
                            let id = name.id;
                            let value = self.create_value_with_destroy_flag(
                                param.ty,
                                ValueKind::Local(id),
                            );
                            self.locals.insert(id, value);
                        }
                        Pat::Discard(_) => (),
                    }
                }

                let last_value = self.lower_expr(body);

                if !self.body.is_terminating() {
                    // If the body isn't terminating, we must push a return instruction at the
                    // for the function's last value.
                    let fn_ty = self.cx.mir.fn_sigs[sig].ty.as_fn().unwrap();

                    let ret_value = if fn_ty.ret.is_unit()
                        && !self.body.value(last_value).ty.is_unit()
                    {
                        // If the value of this function's block has been coerced to unit, we must
                        // return a unit value
                        self.const_unit()
                    } else {
                        last_value
                    };

                    self.push_return(ret_value, body.span.tail());
                }

                // println!("fn `{}`", fun.sig.word);
                // println!("{}", self.value_states);
                // println!("---------------------");

                self.try_move(last_value, body.span, Rules::new());
                self.exit_scope();

                self.cx.mir.fns.insert(
                    sig,
                    Fn { def_id: fun.def_id, sig, body: self.body },
                );
            }
            FnKind::Extern { .. } => unreachable!(),
        }
    }

    fn lower_global_let(mut self, let_: &hir::Let) -> Option<GlobalId> {
        match &let_.pat {
            Pat::Name(name) => {
                let full_name = self.cx.db[name.id].qpath.join_with("_");
                let ty = self.cx.db[name.id].ty;

                self.enter_scope(ScopeKind::Block, let_.value.span);
                let start_blk = self.body.create_block("start");
                self.position_at(start_blk);

                let value = self.lower_expr(&let_.value);
                self.try_move(value, let_.value.span, Rules::new());
                self.exit_scope();

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
        self.lower_expr_with_rules(expr, Rules::new())
    }

    fn lower_expr_with_rules(
        &mut self,
        expr: &hir::Expr,
        rules: Rules,
    ) -> ValueId {
        let value = self.lower_expr_inner(expr, rules);
        self.apply_coercions_to_expr(expr, value)
    }

    fn lower_expr_inner(&mut self, expr: &hir::Expr, rules: Rules) -> ValueId {
        match &expr.kind {
            hir::ExprKind::Let(let_) => {
                let init = self.lower_expr(&let_.value);
                self.try_move(init, let_.value.span, rules);

                match &let_.pat {
                    Pat::Name(name) => {
                        let value = self.push_inst_with(
                            let_.ty,
                            ValueKind::Local(name.id),
                            |value| Inst::Local { value, init },
                        );
                        self.locals.insert(name.id, value);
                        self.create_destroy_flag(value);
                    }
                    Pat::Discard(_) => (),
                }

                self.const_unit()
            }
            hir::ExprKind::Assign(assign) => {
                let lhs = self.lower_expr(&assign.lhs);
                let rhs = self.lower_expr(&assign.rhs);
                self.try_move(rhs, assign.rhs.span, rules);

                let rhs = if let Some(op) = assign.op {
                    self.push_inst_with_register(assign.lhs.ty, |value| {
                        Inst::Binary { value, lhs, rhs, op, span: expr.span }
                    })
                } else {
                    rhs
                };

                // NOTE: The lhs needs to be destroyed before it's assigned to
                self.destroy_members(lhs, assign.lhs.span);
                self.destroy_value(lhs, assign.lhs.span);
                self.push_inst(Inst::Store { value: rhs, target: lhs });

                self.const_unit()
            }
            hir::ExprKind::If(if_) => {
                let then_blk = self.body.create_block("if_then");
                let else_blk = self.body.create_block("if_else");
                let merge_blk = self.body.create_block("if_merge");

                let cond = self.lower_expr(&if_.cond);
                self.try_move(cond, if_.cond.span, rules);
                self.push_br_if(cond, then_blk, Some(else_blk));

                self.position_at(then_blk);
                let then_value = self.lower_expr(&if_.then);
                self.push_br(merge_blk);

                self.position_at(else_blk);
                let else_value = self.lower_expr(&if_.otherwise);
                self.push_br(merge_blk);

                self.position_at(merge_blk);
                self.push_inst_with_register(expr.ty, |value| Inst::If {
                    value,
                    cond,
                    then: then_value,
                    otherwise: else_value,
                })
            }
            hir::ExprKind::Loop(loop_) => {
                let start_blk = self.body.create_block("loop_start");
                let end_blk = self.body.create_block("loop_end");

                self.enter_scope(
                    ScopeKind::Loop(LoopScope::new(end_blk)),
                    expr.span,
                );

                self.push_br(start_blk);
                self.position_at(start_blk);

                if let Some(cond_expr) = &loop_.cond {
                    let cond = self.lower_expr(cond_expr);
                    self.try_move(cond, cond_expr.span, rules);
                    let not_cond = self.push_inst_with_register(
                        self.cx.db.types.bool,
                        |value| Inst::Unary {
                            value,
                            inner: cond,
                            op: UnOp::Not,
                        },
                    );

                    self.push_br_if(not_cond, end_blk, None);
                }

                self.lower_expr(&loop_.expr);
                self.push_br(start_blk);

                if self.in_connected_block() {
                    self.check_loop_moves();
                }

                self.exit_scope();

                self.position_at(end_blk);
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
            hir::ExprKind::Block(blk) => {
                let mut result: Option<ValueId> = None;

                self.enter_scope(ScopeKind::Block, expr.span);

                for expr in &blk.exprs {
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
                    blk.exprs.last().map_or(expr.span, |e| e.span),
                );
                self.exit_scope();

                result
            }
            hir::ExprKind::Return(ret) => {
                let value = self.lower_expr(&ret.expr);
                self.try_move(value, ret.expr.span, rules);
                self.push_return(value, expr.span);
                self.const_unit()
            }
            hir::ExprKind::Call(call) => {
                let callee = self.lower_expr(&call.callee);
                self.try_move(callee, call.callee.span, rules);

                // NOTE: We evaluate args in passing order, and then sort them to the actual
                // required parameter order
                let mut args = vec![];

                for arg in &call.args {
                    let idx = arg.index.expect("arg index to be resolved");
                    let value = self.lower_expr(&arg.expr);
                    self.try_move(value, arg.expr.span, rules);
                    args.push((idx, value));
                }

                args.sort_by_key(|(idx, _)| *idx);

                self.push_inst_with_register(expr.ty, |value| Inst::Call {
                    value,
                    callee,
                    args: args.into_iter().map(|(_, arg)| arg).collect(),
                })
            }
            hir::ExprKind::Unary(un) => {
                let inner = self.lower_expr(&un.expr);
                self.try_move(inner, un.expr.span, rules);
                self.push_inst_with_register(expr.ty, |value| Inst::Unary {
                    value,
                    inner,
                    op: un.op,
                })
            }
            hir::ExprKind::Binary(bin) => {
                let lhs = self.lower_expr(&bin.lhs);
                let rhs = self.lower_expr(&bin.rhs);

                self.try_move(lhs, bin.lhs.span, rules);
                self.try_move(rhs, bin.rhs.span, rules);

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
                self.try_move(inner, cast.expr.span, rules);

                self.push_inst_with_register(cast.target, |value| Inst::Cast {
                    value,
                    inner,
                    target: cast.target,
                    span: expr.span,
                })
            }
            hir::ExprKind::Member(access) => {
                let member = access.member.name();
                let value = self.lower_expr(&access.expr);

                if let Some(member_value) = self
                    .members
                    .get(&value)
                    .and_then(|members| members.get(&member))
                {
                    *member_value
                } else {
                    self.create_value(expr.ty, ValueKind::Member(value, member))
                }
            }
            hir::ExprKind::Name(name) => {
                self.lower_name(name.id, &name.instantiation, expr.span)
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

    fn lower_name(
        &mut self,
        id: DefId,
        instantiation: &Instantiation,
        span: Span,
    ) -> ValueId {
        match self.cx.db[id].kind.as_ref() {
            DefKind::Fn(_) => {
                let id = self.cx.id_to_fn_sig[&id];

                let value = self.create_value(
                    self.cx.mir.fn_sigs[id].ty,
                    ValueKind::Fn(id),
                );

                if !instantiation.is_empty() {
                    self.body.create_instantation(value, instantiation.clone());
                }

                value
            }
            DefKind::ExternGlobal | DefKind::Global => {
                let id = self.cx.lower_global(id);
                self.create_value(
                    self.cx.mir.globals[id].ty,
                    ValueKind::Global(id),
                )
            }
            DefKind::Variable => {
                let value = self.locals[&id];
                self.report_if_moved(value, span);
                value
            }
            DefKind::Struct(sid) => {
                let id = self.cx.get_or_create_struct_ctor(*sid);
                self.create_value(self.cx.mir.fn_sigs[id].ty, ValueKind::Fn(id))
            }
            DefKind::Ty(_) => unreachable!("{:?}", &self.cx.db[id]),
        }
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
        self.destroy_all_values(span);
        self.push_inst(Inst::Return { value });
    }

    pub fn push_br(&mut self, target: BlockId) {
        self.create_edge(target);
        self.push_inst(Inst::Br { target });
    }

    pub fn push_br_if(
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
        self.body.block_mut(self.current_block).push_inst(inst);
    }

    pub fn create_value(&mut self, ty: Ty, kind: ValueKind) -> ValueId {
        let value = self.body.create_value(ty, kind);
        self.set_owned(value);
        self.scope_mut().created_values.push(value);
        self.create_value_members(value, ty);
        value
    }

    pub fn create_value_members(&mut self, value: ValueId, ty: Ty) {
        if let Some(fields) = ty.fields(self.cx.db) {
            #[allow(clippy::unnecessary_to_owned)]
            let members: FxHashMap<_, _> = fields
                .to_vec()
                .into_iter()
                .filter_map(|field| {
                    let name = field.name.name();
                    self.ty_is_move(field.ty).then(|| {
                        (
                            name,
                            self.create_value_with_destroy_flag(
                                field.ty,
                                ValueKind::Member(value, name),
                            ),
                        )
                    })
                })
                .collect();

            self.members.insert(value, members);
        }
    }

    pub fn create_value_with_destroy_flag(
        &mut self,
        ty: Ty,
        kind: ValueKind,
    ) -> ValueId {
        let value = self.create_value(ty, kind);
        self.create_destroy_flag(value);
        value
    }

    fn walk_members(
        &mut self,
        value: ValueId,
        mut f: impl FnMut(&mut Self, ValueId) -> Result<(), Diagnostic>,
    ) -> Result<(), Diagnostic> {
        self.walk_members_aux(value, &mut f)
    }

    fn walk_members_aux(
        &mut self,
        value: ValueId,
        f: &mut impl FnMut(&mut Self, ValueId) -> Result<(), Diagnostic>,
    ) -> Result<(), Diagnostic> {
        if let Some(members) = self.members.get(&value).cloned() {
            for member in members.values().copied() {
                f(self, member)?;
                self.walk_members_aux(member, f)?;
            }
        }

        Ok(())
    }

    fn walk_parents(
        &mut self,
        value: ValueId,
        mut f: impl FnMut(&mut Self, ValueId) -> Result<(), Diagnostic>,
    ) -> Result<(), Diagnostic> {
        self.walk_parents_aux(value, &mut f)
    }

    fn walk_parents_aux(
        &mut self,
        value: ValueId,
        f: &mut impl FnMut(&mut Self, ValueId) -> Result<(), Diagnostic>,
    ) -> Result<(), Diagnostic> {
        if let &ValueKind::Member(parent, _) = &self.body.value(value).kind {
            f(self, parent)?;
            self.walk_parents_aux(parent, f)
        } else {
            Ok(())
        }
    }

    pub fn move_out(&mut self, value: ValueId, moved_to: Span) {
        if let Err(diagnostic) = self.move_out_aux(value, moved_to) {
            self.cx.db.diagnostics.emit(diagnostic);
        }
    }

    pub fn move_out_aux(
        &mut self,
        value: ValueId,
        moved_to: Span,
    ) -> Result<(), Diagnostic> {
        self.check_if_moved(value, moved_to)?;
        let scope = self.scope_mut();
        scope.created_values.retain(|v| *v != value);
        scope.moved_out.push(value);

        self.walk_members(value, |this, member| {
            this.move_out_aux(member, moved_to)
        })
    }

    pub fn try_move(&mut self, value: ValueId, moved_to: Span, rules: Rules) {
        if let Err(diagnostic) = self.try_move_inner(value, moved_to, rules) {
            self.cx.db.diagnostics.emit(diagnostic);
        }
    }

    pub fn try_move_inner(
        &mut self,
        value: ValueId,
        moved_to: Span,
        rules: Rules,
    ) -> Result<(), Diagnostic> {
        self.check_if_moved(value, moved_to)?;

        // If the value is copy, we don't need to move it.
        // Just check that its parents can be used.
        if !self.value_is_move(value) {
            self.walk_parents(value, |this, parent| {
                this.check_if_moved(parent, moved_to)
            })?;
            return Ok(());
        }

        // Mark the value and its members as moved.
        // Mark its parents (if any) as partially moved
        self.set_moved(value, moved_to);
        self.walk_members(value, |this, member| {
            this.set_moved(member, moved_to);
            Ok(())
        })
        .unwrap();
        self.walk_parents(value, |this, parent| {
            this.set_partially_moved(parent, moved_to);
            Ok(())
        })
        .unwrap();

        self.insert_loop_move(value, moved_to);
        self.check_move_out_of_global(value, moved_to)?;

        self.set_destroy_flag(value);

        Ok(())
    }

    pub fn report_if_moved(&mut self, value: ValueId, moved_to: Span) {
        if let Err(diagnostic) = self.check_if_moved(value, moved_to) {
            self.cx.db.diagnostics.emit(diagnostic);
        }
    }

    pub fn check_if_moved(
        &mut self,
        value: ValueId,
        moved_to: Span,
    ) -> Result<(), Diagnostic> {
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
    ) -> Result<(), Diagnostic> {
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
                            "{name} moved here, in the previous loop iteration"
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

    pub fn value_state(&mut self, value: ValueId) -> ValueState {
        if let Some(state) =
            self.value_states.get(self.current_block, value).cloned()
        {
            return state;
        }

        let state = self.solve_value_state(value);
        self.value_states.insert(self.current_block, value, state.clone());
        state
    }

    pub fn solve_value_state(&self, value: ValueId) -> ValueState {
        let mut work: Vec<BlockId> = self
            .body
            .block(self.current_block)
            .predecessors
            .iter()
            .copied()
            .collect();

        let mut visited = FxHashSet::<BlockId>::default();
        let mut result_state = ValueState::Owned;
        let mut last_move_span: Option<Span> = None;
        let mut is_initial_state = true;

        while let Some(block) = work.pop() {
            visited.insert(block);

            if let Some(state) = self.value_states.get(block, value).cloned() {
                let new_move_span = match &state {
                    ValueState::Owned => last_move_span,
                    ValueState::Moved(moved_to)
                    | ValueState::MaybeMoved(moved_to)
                    | ValueState::PartiallyMoved(moved_to) => Some(*moved_to),
                };

                if new_move_span.is_some() {
                    last_move_span = new_move_span;
                }

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
                // calculate this value's state for this block too
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
            "value v{} is missing a state in block b{}",
            value.0, self.current_block.0,
        );

        result_state
    }

    fn value_is_move(&self, value: ValueId) -> bool {
        self.ty_is_move(self.body.value(value).ty)
    }

    fn ty_is_move(&self, ty: Ty) -> bool {
        match ty.kind() {
            TyKind::Struct(sid) => self.cx.db[*sid].kind.is_ref(),
            TyKind::Param(_) => true,
            _ => false,
        }
    }

    pub fn set_owned(&mut self, value: ValueId) {
        self.value_states.insert(self.current_block, value, ValueState::Owned);
    }

    pub fn set_moved(&mut self, value: ValueId, moved_to: Span) {
        self.value_states.insert(
            self.current_block,
            value,
            ValueState::Moved(moved_to),
        );
    }

    pub fn set_partially_moved(&mut self, value: ValueId, moved_to: Span) {
        self.value_states.insert(
            self.current_block,
            value,
            ValueState::PartiallyMoved(moved_to),
        );
    }

    #[inline]
    pub fn position_at(&mut self, id: BlockId) {
        self.current_block = id;
    }

    pub fn in_connected_block(&self) -> bool {
        self.body.block(self.current_block).is_connected()
    }

    pub fn apply_coercions_to_expr(
        &mut self,
        expr: &hir::Expr,
        value: ValueId,
    ) -> ValueId {
        if let Some(coercions) = self.cx.db.coercions.get(&expr.id) {
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
                CoercionKind::NeverToAny => coerced_value,
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
            created_values: vec![],
            moved_out: vec![],
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
        if !self.value_is_move(value) {
            return;
        }

        match self.value_state(value) {
            ValueState::Moved(_) => {
                // Value has been moved, don't destroy
            }
            ValueState::MaybeMoved(_) => {
                // let destroy_blk = self.body.create_block("destroy");
                // let no_destroy_blk = self.body.create_block("no_destroy");
                //
                // self.push_br_if(destroy_flag, destroy_blk, Some(no_destroy_blk));
                //
                // self.position_at(destroy_blk);
                // self.push_inst(Inst::Destroy { value });
                //
                // // Now that the value is destroyed, it has definitely been moved...
                // self.set_value_as_moved(value, moved_to);
                //
                // self.position_at(no_destroy_blk);

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

    fn destroy_members(&mut self, value: ValueId, span: Span) {
        self.walk_members(value, |this, member| {
            this.destroy_value(member, span);
            Ok(())
        })
        .unwrap();
    }

    fn create_destroy_flag(&mut self, value: ValueId) {
        let init = self.const_bool(true);
        let flag = self.push_inst_with_named_register(
            self.cx.db.types.bool,
            ustr("destroy_flag"),
            |value| Inst::Local { value, init },
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
            self.walk_members(value, |this, member| {
                this.set_destroy_flag(member);
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
            ValueKind::Member(parent, member) => {
                format!("{}.{}", self.value_name_aux(*parent), member)
            }
            ValueKind::Register(_) | ValueKind::Const(_) => {
                "temporary value".to_string()
            }
        }
    }
}

#[derive(Debug)]
struct ValueStates(FxHashMap<BlockId, BlockState>);

impl ValueStates {
    fn new() -> Self {
        Self(FxHashMap::default())
    }

    fn get(&self, block: BlockId, value: ValueId) -> Option<&ValueState> {
        self.0.get(&block).and_then(|b| b.states.get(&value))
    }

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
enum ValueState {
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
struct Scope {
    kind: ScopeKind,
    depth: usize,
    loop_depth: usize,
    span: Span,

    // Values that were created in this scope
    created_values: Vec<ValueId>,
    moved_out: Vec<ValueId>,
}

#[derive(Debug, Clone, EnumAsInner)]
enum ScopeKind {
    Block,
    Loop(LoopScope),
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
struct Rules {
    ignore_moves: bool,
}

impl Rules {
    fn new() -> Self {
        Self { ignore_moves: false }
    }

    fn ignore_moves(mut self, value: bool) -> Self {
        self.ignore_moves = value;
        self
    }
}
