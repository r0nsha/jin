use std::collections::HashMap;

use ustr::{ustr, Ustr};

use crate::{
    db::{Db, DefId, DefKind},
    hir,
    hir::{const_eval::Const, FnKind, Hir},
    subst::{ParamFolder, Subst},
    sym,
    tir::{
        Body, Expr, ExprId, ExprKind, Exprs, ExternFn, Fn, FnParam, FnSig, FnSigId, Global,
        GlobalId, Id, Local, LocalId, Tir,
    },
    ty::{
        coerce::{CoercionKind, Coercions},
        fold::TyFolder,
        Instantiation, Ty, TyKind,
    },
};

pub fn lower(db: &mut Db, hir: &Hir) -> Tir {
    let mut tir = Tir::new();
    let mut cx = LowerCtxt::new(db, hir, &mut tir);

    cx.lower_all();

    tir
}

struct LowerCtxt<'db> {
    db: &'db mut Db,
    hir: &'db Hir,
    tir: &'db mut Tir,

    // Maps functions to their lowered signatures
    fn_map: HashMap<DefId, FnSigId>,

    // Already monomorphized functions
    mono_fns: HashMap<MonoItem, FnSigId>,

    // Maps global lets to their lowered globals
    globals_map: HashMap<DefId, GlobalId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoItem {
    pub id: DefId,
    pub ty: Ty,
}

impl<'db> LowerCtxt<'db> {
    fn new(db: &'db mut Db, hir: &'db Hir, tir: &'db mut Tir) -> Self {
        Self {
            db,
            hir,
            tir,
            fn_map: HashMap::new(),
            mono_fns: HashMap::new(),
            globals_map: HashMap::new(),
        }
    }

    fn lower_all(&mut self) {
        for f in &self.hir.fns {
            if !self.db[f.id].ty.is_polymorphic() {
                let def = &self.db[f.id];
                let sig = self.lower_fn_sig(&f.sig, def.qpath.standard_full_name().into(), def.ty);
                self.fn_map.insert(f.id, sig);
            }
        }

        for let_ in &self.hir.lets {
            if !let_.pat.any(|name| self.globals_map.get(&name.id).is_some()) {
                self.lower_global_let(let_);
            }
        }

        for f in &self.hir.fns {
            if !self.db[f.id].ty.is_polymorphic() {
                let sig = self.fn_map[&f.id];
                self.lower_fn_body(sig, f);
            }
        }
    }

    fn lower_global(&mut self, def_id: DefId) -> GlobalId {
        if let Some(target_id) = self.globals_map.get(&def_id).copied() {
            return target_id;
        }

        let let_ = self.hir.lets.iter().find(|let_| match &let_.pat {
            hir::Pat::Name(n) => n.id == def_id,
            hir::Pat::Ignore(_) => false,
        });

        if let Some(let_) = let_ {
            self.lower_global_let(let_).expect("to output a GlobalId")
        } else {
            panic!("global let {} not found in hir.lets", self.db[def_id].qpath);
        }
    }

    fn lower_global_let(&mut self, let_: &hir::Let) -> Option<GlobalId> {
        LowerBodyCtxt::new(self).lower_global(let_)
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
                    .standard_full_name();

                ustr(&name)
            };

            let sig = self.lower_fn_sig(&new_fun.sig, name, mono_item.ty);

            self.mono_fns.insert(mono_item.clone(), sig);
            self.lower_fn_body(sig, &new_fun);

            sig
        } else {
            panic!("function {} not found in hir.fns", self.db[mono_item.id].qpath);
        }
    }

    fn lower_fn_sig(&mut self, sig: &hir::FnSig, name: Ustr, ty: Ty) -> FnSigId {
        let sig = FnSig {
            id: self.tir.sigs.next_key(),
            name,
            params: sig.params.iter().map(|p| FnParam { def_id: p.id, ty: p.ty }).collect(),
            ret: ty.as_fn().unwrap().ret,
            ty,
        };

        self.tir.sigs.push(sig)
    }

    fn lower_fn_body(&mut self, sig: FnSigId, f: &hir::Fn) {
        assert!(
            !self.db[f.id].ty.is_polymorphic(),
            "lowering polymorphic functions to TIR is not allowed"
        );
        LowerBodyCtxt::new(self).lower_fn(sig, f);
    }
}

struct LowerBodyCtxt<'cx, 'db> {
    cx: &'cx mut LowerCtxt<'db>,
    body: Body,
    def_to_local: HashMap<DefId, LocalId>,
}

impl<'cx, 'db> LowerBodyCtxt<'cx, 'db> {
    fn new(cx: &'cx mut LowerCtxt<'db>) -> Self {
        Self { cx, body: Body::new(), def_to_local: HashMap::new() }
    }

    fn lower_fn(mut self, sig: FnSigId, f: &hir::Fn) {
        for param in self.cx.tir.sigs[sig].params.clone() {
            self.create_local(param.def_id, param.ty);
        }

        match &f.kind {
            FnKind::Bare { body } => {
                if self.cx.db.main_function_id() == Some(f.id) {
                    self.cx.tir.main_fn = Some(sig);
                }

                let value = self.lower_expr(body);

                self.cx.tir.fns.push_with_key(|id| Fn {
                    id,
                    def_id: f.id,
                    sig,
                    value,
                    body: self.body,
                });
            }
            FnKind::Extern => {
                self.cx.tir.extern_fns.push_with_key(|id| ExternFn { id, def_id: f.id, sig });
            }
        }
    }

    fn lower_global(mut self, let_: &hir::Let) -> Option<GlobalId> {
        let value = self.lower_expr(&let_.value);

        match &let_.pat {
            hir::Pat::Name(name) => {
                let full_name = self.cx.db[name.id].qpath.standard_full_name();
                let ty = self.cx.db[name.id].ty;

                let id = self.cx.tir.globals.push_with_key(|id| Global {
                    id,
                    def_id: name.id,
                    name: full_name.into(),
                    value,
                    ty,
                    body: self.body,
                });

                self.cx.globals_map.insert(name.id, id);

                Some(id)
            }
            hir::Pat::Ignore(_) => None,
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
                        hir::Pat::Ignore(_) => ExprKind::Const(Const::Unit),
                    }
                }
                hir::ExprKind::If(if_) => ExprKind::If {
                    cond: self.lower_expr(&if_.cond),
                    then: self.lower_expr(&if_.then),
                    otherwise: if_.otherwise.as_ref().map(|o| self.lower_expr(o)),
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
                hir::ExprKind::Binary(bin) => ExprKind::Binary {
                    lhs: self.lower_expr(&bin.lhs),
                    rhs: self.lower_expr(&bin.rhs),
                    op: bin.op,
                },
                hir::ExprKind::Cast(cast) => {
                    ExprKind::Cast { value: self.lower_expr(&cast.expr), target: expr.ty }
                }
                hir::ExprKind::MemberAccess(access) => {
                    let value = self.lower_expr(&access.expr);

                    match access.expr.ty.kind() {
                        TyKind::Str if access.member.name() == sym::PTR => {
                            ExprKind::Index { value, index: 0 }
                        }
                        TyKind::Str if access.member.name() == sym::LEN => {
                            ExprKind::Index { value, index: 1 }
                        }
                        _ => {
                            panic!("invalid type when lowering MemberAccess: {:?}", expr.ty.kind())
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
                    DefKind::Global => ExprKind::Id(Id::Global(self.cx.lower_global(name.id))),
                    DefKind::Variable => ExprKind::Id(Id::Local(self.def_to_local[&name.id])),
                    DefKind::Ty(_) => unreachable!(),
                },
                hir::ExprKind::Lit(_) => unreachable!("lits should always have a const value"),
            }
        };

        let new_expr = self.create_expr(kind, expr.ty);

        if let Some(coercions) = self.cx.db.coercions.get(&expr.id) {
            coercions.apply(&mut self.body.exprs, new_expr)
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

impl Coercions {
    fn apply(&self, exprs: &mut Exprs, mut expr: ExprId) -> ExprId {
        for coercion in self.iter() {
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
}
