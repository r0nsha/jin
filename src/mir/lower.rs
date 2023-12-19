use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr};

use crate::{
    db::{Db, DefId, DefKind},
    hir,
    hir::{FnKind, Hir},
    index_vec::IndexVecExt,
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
            let id = self.mir.globals.push_with_key(|id| Global {
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
        let destroy_glue = &self.hir.let_destroy_glues[&let_.id];
        LowerBody::new(self, destroy_glue).lower_global_let(let_)
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

        let sig_id = self.mir.fn_sigs.push_with_key(|id| FnSig {
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

        self.mir.fn_sigs.push_with_key(|id| FnSig {
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
        let destroy_glue = &self.hir.fn_destroy_glues[&f.id];
        LowerBody::new(self, destroy_glue).lower_fn(sig, f);
    }
}

struct LowerBody<'cx, 'db> {
    cx: &'cx mut Lower<'db>,
    destroy_glue: &'cx hir::DestroyGlue,
    destroy_flags: FxHashMap<hir::DestroyGlueItem, ValueId>,
    body: Body,
    expr_to_value: FxHashMap<hir::ExprId, ValueId>,
    curr_block: BlockId,
    loop_blocks: Vec<BlockId>,
}

impl<'cx, 'db> LowerBody<'cx, 'db> {
    fn new(
        cx: &'cx mut Lower<'db>,
        destroy_glue: &'cx hir::DestroyGlue,
    ) -> Self {
        Self {
            cx,
            destroy_glue,
            destroy_flags: FxHashMap::default(),
            body: Body::new(),
            expr_to_value: FxHashMap::default(),
            curr_block: BlockId::start(),
            loop_blocks: vec![],
        }
    }

    fn lower_fn(mut self, sig: FnSigId, fun: &hir::Fn) {
        match &fun.kind {
            FnKind::Bare { body } => {
                if self.cx.db.main_function_id() == Some(fun.def_id) {
                    self.cx.mir.main_fn = Some(sig);
                }

                let start_blk = self.body.create_block("start");
                self.position_at(start_blk);

                for param in &fun.sig.params {
                    match &param.pat {
                        Pat::Name(name) => {
                            // TODO:
                            // self.push_destroy_flag(hir::DestroyGlueItem::Def(
                            //     name.id,
                            // ));
                        }
                        Pat::Discard(_) => (),
                    }
                }

                let last_value = self.lower_expr(body);

                if !self.body.is_terminating() {
                    let fn_ty = self.cx.mir.fn_sigs[sig].ty.as_fn().unwrap();

                    let ret_value = if fn_ty.ret.is_unit()
                        && !self.body.value(last_value).ty.is_unit()
                    {
                        self.const_unit()
                    } else {
                        last_value
                    };

                    self.push_inst(Inst::Return { value: ret_value });
                }

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

                let start_blk = self.body.create_block("start");
                self.position_at(start_blk);

                let value = self.lower_expr(&let_.value);
                let kind = GlobalKind::Static(self.body, value);

                let id = self.cx.mir.globals.push_with_key(|id| Global {
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
        let coerced_value = self.apply_coercions_to_expr(expr, value);
        self.expr_to_value.insert(expr.id, coerced_value);
        coerced_value
    }

    fn lower_expr_inner(&mut self, expr: &hir::Expr) -> ValueId {
        match &expr.kind {
            hir::ExprKind::Let(let_) => {
                let init = self.lower_expr(&let_.value);

                match &let_.pat {
                    Pat::Name(name) => {
                        // TODO:
                        // self.push_destroy_flag(hir::DestroyGlueItem::Def(
                        //     name.id,
                        // ));

                        self.push_inst_with(
                            let_.ty,
                            ValueKind::Local(name.id),
                            |value| Inst::Local { value, init },
                        );
                    }
                    Pat::Discard(_) => (),
                }

                self.const_unit()
            }
            hir::ExprKind::Assign(assign) => {
                let lhs = self.lower_expr(&assign.lhs);
                let rhs = self.lower_expr(&assign.rhs);

                let rhs = if let Some(op) = assign.op {
                    self.push_inst_with_register(assign.lhs.ty, |value| {
                        Inst::Binary { value, lhs, rhs, op, span: expr.span }
                    })
                } else {
                    rhs
                };

                // The lhs needs to be destroyed before it's assigned to
                self.push_unconditional_destroy(lhs);
                self.push_inst(Inst::Store { value: rhs, target: lhs });

                self.const_unit()
            }
            hir::ExprKind::If(if_) => {
                let then_blk = self.body.create_block("if_then");
                let else_blk = self.body.create_block("if_else");
                let merge_blk = self.body.create_block("if_merge");

                let cond = self.lower_expr(&if_.cond);
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

                self.push_br(start_blk);
                self.position_at(start_blk);

                if let Some(cond) = &loop_.cond {
                    let cond = self.lower_expr(cond);
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

                self.loop_blocks.push(end_blk);
                self.lower_expr(&loop_.expr);
                self.push_br(start_blk);
                self.loop_blocks.pop();

                self.position_at(end_blk);
                self.const_unit()
            }
            hir::ExprKind::Break => {
                let loop_blk =
                    self.loop_blocks.last().expect("to be inside a loop block");
                self.push_br(*loop_blk);
                self.const_unit()
            }
            hir::ExprKind::Block(blk) => {
                let mut result: Option<ValueId> = None;

                for expr in &blk.exprs {
                    result = Some(self.lower_expr(expr));
                }

                // TODO: self.lower_destroy_glue(expr.id);

                // NOTE: If the block ty is `unit`, we must always return a `unit` value.
                // A situation where we don't return a `unit` value can occur
                // when the expected type of the block is unit, but the last expression doesn't
                // return `unit`.
                if expr.ty.is_unit() {
                    self.const_unit()
                } else {
                    result.unwrap_or_else(|| self.const_unit())
                }
            }
            hir::ExprKind::Return(ret) => {
                let value = self.lower_expr(&ret.expr);
                self.push_inst(Inst::Return { value });
                // TODO: push unreachable inst instead of unit literal
                self.const_unit()
            }
            hir::ExprKind::Call(call) => {
                let callee = self.lower_expr(&call.callee);

                // NOTE: We evaluate args in passing order, and then sort them to the actual
                // required parameter order
                let mut args: Vec<_> = call
                    .args
                    .iter()
                    .map(|arg| {
                        (
                            arg.index.expect("arg index to be resolved"),
                            self.lower_expr(&arg.expr),
                        )
                    })
                    .collect();

                args.sort_by_key(|(idx, _)| *idx);

                self.push_inst_with_register(expr.ty, |value| Inst::Call {
                    value,
                    callee,
                    args: args.into_iter().map(|(_, arg)| arg).collect(),
                })
            }
            hir::ExprKind::Unary(un) => {
                let inner = self.lower_expr(&un.expr);
                self.push_inst_with_register(expr.ty, |value| Inst::Unary {
                    value,
                    inner,
                    op: un.op,
                })
            }
            hir::ExprKind::Binary(bin) => {
                let lhs = self.lower_expr(&bin.lhs);
                let rhs = self.lower_expr(&bin.rhs);
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

                self.push_inst_with_register(cast.target, |value| Inst::Cast {
                    value,
                    inner,
                    target: cast.target,
                    span: expr.span,
                })
            }
            hir::ExprKind::Member(access) => {
                let value = self.lower_expr(&access.expr);
                self.body.create_value(
                    expr.ty,
                    ValueKind::Member(value, access.member.name()),
                )
            }
            hir::ExprKind::Name(name) => {
                self.lower_name(name.id, expr.ty, &name.instantiation)
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

    // Generates Inst::Destroy for all expressions and definitions that need
    // to be destroyed in this expr's scope (usually block), in reverse.
    // Also Sets destroy flags for values that have been conditionally moved to this scope.
    fn lower_destroy_glue(&mut self, block_id: hir::BlockExprId) {
        if let Some(items) = self.destroy_glue.to_destroy.get(&block_id) {
            for item in items.iter().rev() {
                let value = match item {
                    hir::DestroyGlueItem::Expr(expr_id) => {
                        self.expr_to_value[expr_id]
                    }
                    hir::DestroyGlueItem::Def(id) => {
                        let def_ty = self.cx.db[*id].ty;
                        self.body.create_value(def_ty, ValueKind::Local(*id))
                    }
                };

                if let Some(destroy_flag) = self.destroy_flags.get(item) {
                    self.push_conditional_destroy(value, *destroy_flag);
                } else {
                    self.push_unconditional_destroy(value);
                }
            }
        }

        // Set all destroy flags for values that were conditionally moved to this block
        self.destroy_glue
            .needs_destroy_flag
            .iter()
            .filter_map(|(item, moved_to)| {
                (*moved_to == block_id).then_some(*item)
            })
            .for_each(|item| self.set_destroy_flag(item));
    }

    fn lower_name(
        &mut self,
        id: DefId,
        ty: Ty,
        instantiation: &Instantiation,
    ) -> ValueId {
        match self.cx.db[id].kind.as_ref() {
            DefKind::Fn(_) => {
                let id = self.cx.id_to_fn_sig[&id];

                let value = self.body.create_value(
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
                self.body.create_value(
                    self.cx.mir.globals[id].ty,
                    ValueKind::Global(id),
                )
            }
            DefKind::Variable => {
                self.body.create_value(ty, ValueKind::Local(id))
            }
            DefKind::Struct(sid) => {
                let id = self.cx.get_or_create_struct_ctor(*sid);
                self.body
                    .create_value(self.cx.mir.fn_sigs[id].ty, ValueKind::Fn(id))
            }
            DefKind::Ty(_) => unreachable!("{:?}", &self.cx.db[id]),
        }
    }

    pub fn lower_const(&mut self, value: &Const, ty: Ty) -> ValueId {
        match value {
            Const::Str(lit) => self.push_inst_with_register(ty, |value| {
                Inst::StrLit { value, lit: *lit }
            }),
            Const::Int(value) => self
                .body
                .create_value(ty, ValueKind::Const(Const::from(*value))),
            Const::Float(value) => self
                .body
                .create_value(ty, ValueKind::Const(Const::from(*value))),
            Const::Bool(value) => self.const_bool(*value),
            Const::Unit => self.const_unit(),
        }
    }

    pub fn const_unit(&mut self) -> ValueId {
        self.body
            .create_value(self.cx.db.types.unit, ValueKind::Const(Const::Unit))
    }

    pub fn const_bool(&mut self, value: bool) -> ValueId {
        self.body.create_value(
            self.cx.db.types.bool,
            ValueKind::Const(Const::Bool(value)),
        )
    }

    pub fn push_inst_with_register(
        &mut self,
        value_ty: Ty,
        f: impl FnOnce(ValueId) -> Inst,
    ) -> ValueId {
        self.push_inst_with(value_ty, ValueKind::Register, f)
    }

    pub fn push_inst_with(
        &mut self,
        value_ty: Ty,
        kind: ValueKind,
        f: impl FnOnce(ValueId) -> Inst,
    ) -> ValueId {
        let value = self.body.create_value(value_ty, kind);
        self.push_inst(f(value));
        value
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
        self.body.block_mut(self.curr_block).successors.insert(target);
        self.body.block_mut(target).predecessors.insert(self.curr_block);
    }

    pub fn push_inst(&mut self, inst: Inst) {
        self.body.block_mut(self.curr_block).push_inst(inst);
    }

    #[inline]
    pub fn position_at(&mut self, id: BlockId) {
        self.curr_block = id;
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

    fn push_conditional_destroy(
        &mut self,
        value: ValueId,
        destroy_flag: ValueId,
    ) {
        if !self.value_needs_destroy(value) {
            return;
        }

        // Conditional destroy
        let destroy_blk = self.body.create_block("destroy");
        let no_destroy_blk = self.body.create_block("no_destroy");

        self.push_br_if(destroy_flag, destroy_blk, Some(no_destroy_blk));

        self.position_at(destroy_blk);
        self.push_inst(Inst::Destroy { value });

        self.position_at(no_destroy_blk);
    }

    fn push_unconditional_destroy(&mut self, value: ValueId) {
        if self.value_needs_destroy(value) {
            self.push_inst(Inst::Destroy { value });
        }
    }

    fn value_needs_destroy(&self, value_id: ValueId) -> bool {
        let value = self.body.value(value_id);
        match value.ty.kind() {
            TyKind::Struct(sid) => self.cx.db[*sid].kind.is_ref(),
            _ => false,
        }
    }

    fn push_destroy_flag(&mut self, item: hir::DestroyGlueItem) {
        if !self.destroy_glue.needs_destroy_flag.contains_key(&item) {
            return;
        }

        let bool_ty = self.cx.db.types.bool;

        let init = self.const_bool(true);
        let value = self.push_inst_with_register(bool_ty, |value| {
            Inst::Local { value, init }
        });

        self.destroy_flags.insert(item, value);
    }

    fn set_destroy_flag(&mut self, item: hir::DestroyGlueItem) {
        let destroy_flag = self.destroy_flags[&item];
        let value = self.const_bool(false);
        self.push_inst(Inst::Store { value, target: destroy_flag });
    }
}
