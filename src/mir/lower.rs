use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr};

use crate::{
    db::{Db, DefId, DefKind},
    hir,
    hir::{const_eval::Const, FnKind, Hir},
    index_vec::IndexVecExt,
    mir::*,
    subst::{ParamFolder, Subst},
    ty::{
        coerce::{CoercionKind, Coercions},
        fold::TyFolder,
        Instantiation, Ty, TyKind,
    },
};

pub fn lower(db: &mut Db, hir: &Hir) -> Mir {
    LowerCx::new(db, hir).lower_all()
}

struct LowerCx<'db> {
    db: &'db mut Db,
    hir: &'db Hir,
    mir: Mir,

    // Maps functions to their lowered signatures
    fn_map: FxHashMap<DefId, FnSigId>,

    // Already monomorphized functions
    mono_fns: FxHashMap<MonoItem, FnSigId>,

    // Maps global lets to their lowered globals
    globals_map: FxHashMap<DefId, GlobalId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoItem {
    pub id: DefId,
    pub ty: Ty,
}

impl<'db> LowerCx<'db> {
    fn new(db: &'db mut Db, hir: &'db Hir) -> Self {
        Self {
            db,
            hir,
            mir: Mir::new(),
            fn_map: FxHashMap::default(),
            mono_fns: FxHashMap::default(),
            globals_map: FxHashMap::default(),
        }
    }

    fn lower_all(mut self) -> Mir {
        for f in &self.hir.fns {
            if !f.sig.ty.is_polymorphic() {
                let def = &self.db[f.id];
                let is_extern = f.kind.is_extern();
                let name = if is_extern { def.name } else { def.qpath.join_with("_").into() };
                let sig = self.lower_fn_sig(&f.sig, &f.kind, name, def.ty);
                self.fn_map.insert(f.id, sig);
            }
        }

        for let_ in &self.hir.lets {
            if !let_.pat.any(|name| self.globals_map.get(&name.id).is_some()) {
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

            self.globals_map.insert(let_.id, id);
        }

        for f in &self.hir.fns {
            if !f.sig.ty.is_polymorphic() {
                let sig = self.fn_map[&f.id];
                self.lower_fn_body(sig, f);
            }
        }

        self.mir
    }

    fn lower_global(&mut self, def_id: DefId) -> GlobalId {
        if let Some(target_id) = self.globals_map.get(&def_id).copied() {
            return target_id;
        }

        let let_ = self.hir.lets.iter().find(|let_| match &let_.pat {
            hir::Pat::Name(n) => n.id == def_id,
            hir::Pat::Discard(_) => false,
        });

        if let Some(let_) = let_ {
            self.lower_global_let(let_).expect("to output a GlobalId")
        } else {
            panic!("global let {} not found in hir.lets", self.db[def_id].qpath);
        }
    }

    fn lower_global_let(&mut self, let_: &hir::Let) -> Option<GlobalId> {
        LowerBodyCx::new(self).lower_global(let_)
    }

    fn monomorphize_fn(&mut self, mono_item: &MonoItem, instantiation: &Instantiation) -> FnSigId {
        if let Some(target_id) = self.mono_fns.get(mono_item).copied() {
            return target_id;
        }

        let fun = self.hir.fns.iter().find(|f| f.id == mono_item.id);

        if let Some(fun) = fun {
            let mut new_fun = fun.clone();
            new_fun.subst(&mut ParamFolder { db: self.db, instantiation });

            let name = {
                let args_str = instantiation
                    .values()
                    .map(|t| t.to_string(self.db))
                    .collect::<Vec<String>>()
                    .join("_");

                let def = &self.db[fun.id];

                let name = def
                    .qpath
                    .clone()
                    .with_name(ustr(&format!("{}_{}", def.name, args_str)))
                    .join_with("_");

                ustr(&name)
            };

            let sig = self.lower_fn_sig(&new_fun.sig, &new_fun.kind, name, mono_item.ty);

            self.mono_fns.insert(mono_item.clone(), sig);
            self.lower_fn_body(sig, &new_fun);

            sig
        } else {
            panic!("function {} not found in hir.fns", self.db[mono_item.id].qpath);
        }
    }

    fn get_or_create_struct_ctor(&mut self, sid: StructId) -> FnSigId {
        if let Some(sig_id) = self.mir.struct_ctors.get(&sid) {
            return *sig_id;
        }

        let struct_info = &self.db[sid];
        let name =
            ustr(&self.db[struct_info.def_id].qpath.clone().child(ustr("ctor")).join_with("_"));

        let sig_id = self.mir.fn_sigs.push_with_key(|id| FnSig {
            id,
            name,
            params: struct_info
                .fields
                .iter()
                .map(|f| FnParam { def_id: DefId::INVALID, name: f.name.name(), ty: f.ty })
                .collect(),
            ret: TyKind::Struct(sid).into(),
            ty: struct_info.ctor_ty,
            is_extern: false,
            is_c_variadic: false,
        });

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
                .map(|p| FnParam { def_id: p.id, name: p.name.name(), ty: p.ty })
                .collect(),
            ret: ty.as_fn().unwrap().ret,
            ty,
            is_extern,
            is_c_variadic,
        })
    }

    fn lower_fn_body(&mut self, sig: FnSigId, f: &hir::Fn) {
        if f.kind.is_extern() {
            return;
        }
        assert!(
            !self.mir.fn_sigs[sig].ty.is_polymorphic(),
            "lowering polymorphic functions to mir is not allowed"
        );

        LowerBodyCx::new(self).lower_fn(sig, f);
    }
}

struct LowerBodyCx<'cx, 'db> {
    cx: &'cx mut LowerCx<'db>,
    body: Body,
    curr_block: BlockId,
    loop_blocks: Vec<BlockId>,
}

impl<'cx, 'db> LowerBodyCx<'cx, 'db> {
    fn new(cx: &'cx mut LowerCx<'db>) -> Self {
        Self { cx, body: Body::new(), curr_block: BlockId::start(), loop_blocks: vec![] }
    }

    fn lower_fn(mut self, sig: FnSigId, f: &hir::Fn) {
        match &f.kind {
            FnKind::Bare { body } => {
                if self.cx.db.main_function_id() == Some(f.id) {
                    self.cx.mir.main_fn = Some(sig);
                }

                let start_blk = self.body.create_block("start");
                self.position_at(start_blk);

                let last_value = self.lower_expr(body);

                if !self.body.is_terminating() {
                    let fn_ty = self.cx.mir.fn_sigs[sig].ty.as_fn().unwrap();

                    let ret_value =
                        if fn_ty.ret.is_unit() && !self.body.value(last_value).ty.is_unit() {
                            self.const_unit()
                        } else {
                            last_value
                        };

                    self.push_inst(Inst::Return { value: ret_value });
                }

                self.cx.mir.fns.push(Fn { def_id: f.id, sig, body: self.body });
            }
            FnKind::Extern { .. } => unreachable!(),
        }
    }

    fn lower_global(self, let_: &hir::Let) -> Option<GlobalId> {
        let kind = if let Some(value) = self.cx.db.const_storage.expr(let_.value.id).cloned() {
            GlobalKind::Const(value)
        } else {
            let mut cx = LowerBodyCx::new(self.cx);
            let start_blk = cx.body.create_block("start");
            cx.position_at(start_blk);
            let value = cx.lower_expr(&let_.value);
            GlobalKind::Static(cx.body, value)
        };

        match &let_.pat {
            hir::Pat::Name(name) => {
                let full_name = self.cx.db[name.id].qpath.join_with("_");
                let ty = self.cx.db[name.id].ty;

                let id = self.cx.mir.globals.push_with_key(|id| Global {
                    id,
                    def_id: name.id,
                    name: full_name.into(),
                    ty,
                    kind,
                });

                self.cx.globals_map.insert(name.id, id);

                Some(id)
            }
            hir::Pat::Discard(_) => None,
        }
    }

    fn lower_expr(&mut self, expr: &hir::Expr) -> ValueId {
        let value = if let Some(value) = self.cx.db.const_storage.expr(expr.id).cloned() {
            self.lower_const(&value, expr.ty)
        } else {
            self.lower_expr_inner(expr)
        };

        self.apply_coercions_to_expr(expr, value)
    }

    fn lower_place(&mut self, expr: &hir::Expr) -> ValueId {
        let value = self.lower_expr_inner(expr);
        self.apply_coercions_to_expr(expr, value)
    }

    fn lower_expr_inner(&mut self, expr: &hir::Expr) -> ValueId {
        match &expr.kind {
            hir::ExprKind::Let(let_) => {
                let init = self.lower_expr(&let_.value);

                match &let_.pat {
                    hir::Pat::Name(name) => {
                        self.push_inst_with(let_.ty, ValueKind::Local(name.id), |value| {
                            Inst::Local { value, init }
                        });
                    }
                    hir::Pat::Discard(_) => (),
                }

                self.const_unit()
            }
            hir::ExprKind::Assign(assign) => {
                let lhs = self.lower_place(&assign.lhs);
                let rhs = self.lower_expr(&assign.rhs);

                let rhs = if let Some(op) = assign.op {
                    self.push_inst_with_register(assign.lhs.ty, |value| Inst::Binary {
                        value,
                        lhs,
                        rhs,
                        op,
                        span: expr.span,
                    })
                } else {
                    rhs
                };

                self.push_inst(Inst::Store { value: rhs, target: lhs });

                self.const_unit()
            }
            hir::ExprKind::If(if_) => {
                let then_blk = self.body.create_block("if_then");
                let else_blk = self.body.create_block("if_else");
                let merge_blk = self.body.create_block("if_merge");

                let cond = self.lower_expr(&if_.cond);
                self.push_inst(Inst::BrIf { cond, then: then_blk, otherwise: Some(else_blk) });

                self.position_at(then_blk);
                let then_value = self.lower_expr(&if_.then);
                self.push_inst(Inst::Br { target: merge_blk });

                self.position_at(else_blk);
                let else_value = self.lower_expr(&if_.otherwise);
                self.push_inst(Inst::Br { target: merge_blk });

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

                self.push_inst(Inst::Br { target: start_blk });

                self.position_at(start_blk);

                if let Some(cond) = &loop_.cond {
                    let cond = self.lower_expr(cond);
                    let not_cond = self.push_inst_with_register(self.cx.db.types.bool, |value| {
                        Inst::Unary { value, inner: cond, op: UnOp::Not }
                    });

                    self.push_inst(Inst::BrIf { cond: not_cond, then: end_blk, otherwise: None });
                }

                self.loop_blocks.push(end_blk);
                self.lower_expr(&loop_.expr);
                self.push_inst(Inst::Br { target: start_blk });
                self.loop_blocks.pop();

                self.position_at(end_blk);
                self.const_unit()
            }
            hir::ExprKind::Break => {
                let loop_blk = self.loop_blocks.last().expect("to be inside a loop block");
                self.push_inst(Inst::Br { target: *loop_blk });
                self.const_unit()
            }
            hir::ExprKind::Block(blk) => {
                let mut result: Option<ValueId> = None;

                for expr in &blk.exprs {
                    result = Some(self.lower_expr(expr));
                }

                // TODO: is this necessary now?
                // // NOTE: If the block ty is (), we must always return a () value.
                // // A situation where we don't return a () value can occur
                // // when the expected type of the block is unit, but the last expression doesn't
                // // return ().
                // if expr.ty.is_unit() {
                //     exprs.push(self.create_expr(ExprKind::Const(Const::Unit), expr.ty));
                // }

                result.unwrap_or_else(|| self.const_unit())
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
                        (arg.index.expect("arg index to be resolved"), self.lower_expr(&arg.expr))
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
                self.body.create_value(expr.ty, ValueKind::Member(value, access.member.name()))
            }
            hir::ExprKind::Name(name) => {
                match self.cx.db[name.id].kind.as_ref() {
                    DefKind::Fn(_) => {
                        let id = if name.instantiation.is_empty() {
                            self.cx.fn_map[&name.id]
                        } else {
                            let mut folder =
                                ParamFolder { db: self.cx.db, instantiation: &name.instantiation };

                            let ty = folder.fold(expr.ty);

                            // let instantiation: Instantiation = name
                            //     .instantiation
                            //     .iter()
                            //     .map(|(var, ty)| (*var, folder.fold(*ty)))
                            //     .collect();

                            self.cx
                                .monomorphize_fn(&MonoItem { id: name.id, ty }, &name.instantiation)
                        };

                        self.body.create_value(self.cx.mir.fn_sigs[id].ty, ValueKind::Fn(id))
                    }
                    DefKind::ExternGlobal | DefKind::Global => {
                        let id = self.cx.lower_global(name.id);
                        self.body.create_value(self.cx.mir.globals[id].ty, ValueKind::Global(id))
                    }
                    DefKind::Variable => self.body.create_value(expr.ty, ValueKind::Local(name.id)),
                    DefKind::Struct(sid) => {
                        let id = self.cx.get_or_create_struct_ctor(*sid);
                        self.body.create_value(self.cx.mir.fn_sigs[id].ty, ValueKind::Fn(id))
                    }
                    DefKind::Ty(_) => unreachable!(),
                }
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

    pub fn lower_const(&mut self, value: &Const, ty: Ty) -> ValueId {
        match value {
            Const::Str(lit) => {
                self.push_inst_with_register(ty, |value| Inst::StrLit { value, lit: *lit })
            }
            Const::Int(value) => self.body.create_value(ty, ValueKind::Const(Const::from(*value))),
            Const::Float(value) => {
                self.body.create_value(ty, ValueKind::Const(Const::from(*value)))
            }
            Const::Bool(value) => self.body.create_value(ty, ValueKind::Const(Const::from(*value))),
            Const::Unit => self.body.create_value(ty, ValueKind::Const(Const::Unit)),
        }
    }

    pub fn const_unit(&mut self) -> ValueId {
        self.body.create_value(self.cx.db.types.unit, ValueKind::Const(Const::Unit))
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

    pub fn push_inst(&mut self, inst: Inst) {
        self.body.block_mut(self.curr_block).push_inst(inst);
    }

    #[inline]
    pub fn position_at(&mut self, id: BlockId) {
        self.curr_block = id;
    }

    pub fn apply_coercions_to_expr(&mut self, expr: &hir::Expr, value: ValueId) -> ValueId {
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
                    self.push_inst_with_register(coercion.target, |value| Inst::Cast {
                        value,
                        inner: coerced_value,
                        target: coercion.target,
                        span,
                    })
                }
            };

            self.body.value_mut(coerced_value).ty = coercion.target;
        }

        coerced_value
    }
}
