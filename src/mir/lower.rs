use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr};

use crate::{
    db::{Db, DefId, DefKind},
    hir,
    hir::{const_eval::Const, FnKind, Hir},
    index_vec::IndexVecExt,
    mir::{
        Body, Expr, ExprId, ExprKind, Exprs, Fn, FnParam, FnSig, FnSigId, Global, GlobalId,
        GlobalKind, Id, Local, LocalId, Tir,
    },
    subst::{ParamFolder, Subst},
    sym,
    ty::{
        coerce::{CoercionKind, Coercions},
        fold::TyFolder,
        Instantiation, Ty, TyKind,
    },
};

pub fn lower(db: &mut Db, hir: &Hir) -> Tir {
    LowerCx::new(db, hir).lower_all()
}

struct LowerCx<'db> {
    db: &'db mut Db,
    hir: &'db Hir,
    tir: Tir,

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
            tir: Tir::new(),
            fn_map: FxHashMap::default(),
            mono_fns: FxHashMap::default(),
            globals_map: FxHashMap::default(),
        }
    }

    fn lower_all(mut self) -> Tir {
        for f in &self.hir.fns {
            if !self.db[f.id].ty.is_polymorphic() {
                let def = &self.db[f.id];
                let is_extern = f.kind.is_extern();
                let name = if is_extern { def.name } else { def.qpath.join_with("_").into() };
                let sig = self.lower_fn_sig(&f.sig, name, def.ty, is_extern);
                self.fn_map.insert(f.id, sig);
            }
        }

        for let_ in &self.hir.lets {
            if !let_.pat.any(|name| self.globals_map.get(&name.id).is_some()) {
                self.lower_global_let(let_);
            }
        }

        for let_ in &self.hir.extern_lets {
            let id = self.tir.globals.push_with_key(|id| Global {
                id,
                def_id: let_.id,
                name: let_.word.name(),
                ty: self.db[let_.id].ty,
                kind: GlobalKind::Extern,
            });

            self.globals_map.insert(let_.id, id);
        }

        for f in &self.hir.fns {
            if !self.db[f.id].ty.is_polymorphic() {
                let sig = self.fn_map[&f.id];
                self.lower_fn_body(sig, f);
            }
        }

        self.tir
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
                    .with_name(ustr(&format!("{}${}", def.name, args_str)))
                    .join_with("_");

                ustr(&name)
            };

            let sig = self.lower_fn_sig(&new_fun.sig, name, mono_item.ty, false);

            self.mono_fns.insert(mono_item.clone(), sig);
            self.lower_fn_body(sig, &new_fun);

            sig
        } else {
            panic!("function {} not found in hir.fns", self.db[mono_item.id].qpath);
        }
    }

    fn lower_fn_sig(&mut self, sig: &hir::FnSig, name: Ustr, ty: Ty, is_extern: bool) -> FnSigId {
        self.tir.fn_sigs.push_with_key(|id| FnSig {
            id,
            name,
            params: sig.params.iter().map(|p| FnParam { def_id: p.id, ty: p.ty }).collect(),
            ret: ty.as_fn().unwrap().ret,
            ty,
            is_extern,
        })
    }

    fn lower_fn_body(&mut self, sig: FnSigId, f: &hir::Fn) {
        if f.kind.is_extern() {
            return;
        }

        assert!(
            !self.db[f.id].ty.is_polymorphic(),
            "lowering polymorphic functions to TIR is not allowed"
        );

        LowerBodyCx::new(self).lower_fn(sig, f);
    }
}

struct LowerBodyCx<'cx, 'db> {
    cx: &'cx mut LowerCx<'db>,
    body: Body,
    def_to_local: FxHashMap<DefId, LocalId>,
}

impl<'cx, 'db> LowerBodyCx<'cx, 'db> {
    fn new(cx: &'cx mut LowerCx<'db>) -> Self {
        Self { cx, body: Body::new(), def_to_local: FxHashMap::default() }
    }

    fn lower_fn(mut self, sig: FnSigId, f: &hir::Fn) {
        for param in self.cx.tir.fn_sigs[sig].params.clone() {
            self.create_local(param.def_id, param.ty);
        }

        match &f.kind {
            FnKind::Bare { body } => {
                if self.cx.db.main_function_id() == Some(f.id) {
                    self.cx.tir.main_fn = Some(sig);
                }

                let value = self.lower_expr(body);

                self.cx.tir.fns.push(Fn { def_id: f.id, sig, value, body: self.body });
            }
            FnKind::Extern => unreachable!(),
        }
    }

    fn lower_global(mut self, let_: &hir::Let) -> Option<GlobalId> {
        let value = self.lower_expr(&let_.value);

        match &let_.pat {
            hir::Pat::Name(name) => {
                let full_name = self.cx.db[name.id].qpath.join_with("_");
                let ty = self.cx.db[name.id].ty;

                let id = self.cx.tir.globals.push_with_key(|id| Global {
                    id,
                    def_id: name.id,
                    name: full_name.into(),
                    ty,
                    kind: GlobalKind::Bare { value, body: self.body },
                });

                self.cx.globals_map.insert(name.id, id);

                Some(id)
            }
            hir::Pat::Discard(_) => None,
        }
    }

    fn lower_expr(&mut self, expr: &hir::Expr) -> ExprId {
        let kind = if let Some(val) = self.cx.db.const_storage.expr(expr.id) {
            ExprKind::Const(val.clone())
        } else {
            match &expr.kind {
                hir::ExprKind::Let(let_) => {
                    let value = self.lower_expr(&let_.value);

                    match &let_.pat {
                        hir::Pat::Name(name) => {
                            let ty = self.cx.db[name.id].ty;
                            let id = self.create_local(name.id, ty);
                            ExprKind::Let { id, def_id: name.id, value }
                        }
                        hir::Pat::Discard(_) => ExprKind::Const(Const::Unit),
                    }
                }
                hir::ExprKind::If(if_) => ExprKind::If {
                    cond: self.lower_expr(&if_.cond),
                    then: self.lower_expr(&if_.then),
                    otherwise: self.lower_expr(&if_.otherwise),
                },
                hir::ExprKind::Block(blk) => {
                    let mut exprs: Vec<_> = blk.exprs.iter().map(|e| self.lower_expr(e)).collect();

                    // NOTE: If the block ty is (), we must always return a () value.
                    // A situation where we don't return a () value can occur
                    // when the expected type of the block is unit, but the last expression doesn't
                    // return ().
                    if expr.ty.is_unit() {
                        exprs.push(self.create_expr(ExprKind::Const(Const::Unit), expr.ty));
                    }

                    ExprKind::Block { exprs }
                }
                hir::ExprKind::Return(ret) => {
                    ExprKind::Return { value: self.lower_expr(&ret.expr) }
                }
                hir::ExprKind::Call(call) => {
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

                    let callee = self.lower_expr(&call.callee);
                    ExprKind::Call { callee, args: args.into_iter().map(|(_, arg)| arg).collect() }
                }
                hir::ExprKind::Unary(un) => {
                    ExprKind::Unary { value: self.lower_expr(&un.expr), op: un.op }
                }
                hir::ExprKind::Binary(bin) => {
                    let lhs = self.lower_expr(&bin.lhs);
                    let rhs = self.lower_expr(&bin.rhs);

                    ExprKind::Binary { lhs, rhs, op: bin.op }
                }
                hir::ExprKind::Cast(cast) => {
                    ExprKind::Cast { value: self.lower_expr(&cast.expr), target: expr.ty }
                }
                hir::ExprKind::Member(access) => {
                    let value = self.lower_expr(&access.expr);

                    match access.expr.ty.kind() {
                        TyKind::Str if access.member.name() == sym::PTR => {
                            ExprKind::Index { value, index: 0 }
                        }
                        TyKind::Str if access.member.name() == sym::LEN => {
                            ExprKind::Index { value, index: 1 }
                        }
                        _ => {
                            panic!("invalid type when lowering Member: {:?}", expr.ty.kind())
                        }
                    }
                }
                hir::ExprKind::Name(name) => match self.cx.db[name.id].kind.as_ref() {
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

                        ExprKind::Id(Id::Fn(id))
                    }
                    DefKind::ExternGlobal | DefKind::Global => {
                        ExprKind::Id(Id::Global(self.cx.lower_global(name.id)))
                    }
                    DefKind::Variable => ExprKind::Id(Id::Local(self.def_to_local[&name.id])),
                    DefKind::Ty(_) => unreachable!(),
                },
                hir::ExprKind::Lit(lit) => match lit {
                    hir::Lit::Str(value) => ExprKind::Const(Const::Str(*value)),
                    hir::Lit::Int(value) => {
                        ExprKind::Const(Const::Int(i128::try_from(*value).unwrap()))
                    }
                    hir::Lit::Bool(value) => ExprKind::Const(Const::Bool(*value)),
                },
            }
        };

        let new_expr = self.create_expr(kind, expr.ty);

        if let Some(coercions) = self.cx.db.coercions.get(&expr.id) {
            apply_coercions(&mut self.body.exprs, coercions, new_expr)
        } else {
            new_expr
        }
    }

    #[inline]
    pub fn create_expr(&mut self, kind: ExprKind, ty: Ty) -> ExprId {
        self.body.exprs.push_with_key(|id| Expr { id, kind, ty })
    }

    #[inline]
    pub fn create_local(&mut self, def_id: DefId, ty: Ty) -> LocalId {
        let id = self.body.locals.push_with_key(|id| Local {
            id,
            def_id,
            name: self.cx.db[def_id].name,
            ty,
        });
        self.def_to_local.insert(def_id, id);
        id
    }
}

pub fn apply_coercions(exprs: &mut Exprs, coercions: &Coercions, mut expr: ExprId) -> ExprId {
    for coercion in coercions.iter() {
        expr = match coercion.kind {
            CoercionKind::NeverToAny => expr,
            CoercionKind::IntPromotion => exprs.push_with_key(|id| Expr {
                id,
                ty: coercion.target,
                kind: ExprKind::Cast { value: expr, target: coercion.target },
            }),
        };

        exprs[expr].ty = coercion.target;
    }

    expr
}
