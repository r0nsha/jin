use std::collections::{HashMap, HashSet};

use ustr::ustr;

use crate::{
    common::IndexVec,
    db::{Db, Def, DefId},
    hir,
    hir::Hir,
    passes::{
        subst::{ParamFolder, Subst},
        MonoItem,
    },
    tir::{Expr, ExprId, ExprKind, Exprs, Fn, FnParam, FnSig, FnSigId, Id, Tir},
    ty::{
        coerce::{CoercionKind, Coercions},
        fold::TyFolder,
        Instantiation, Ty,
    },
};

pub fn lower(db: &mut Db, hir: &Hir, mono_items: HashSet<MonoItem>) -> Tir {
    let mut tir = Tir::new();
    let mut cx = LowerCtxt::new(db, hir, &mut tir, mono_items);

    for item in &hir.items {
        cx.lower_item(item);
    }

    tir
}

type MonoItemTarget = DefId;

struct LowerCtxt<'db> {
    db: &'db mut Db,
    hir: &'db Hir,
    tir: &'db mut Tir,
    mono_fns: HashMap<MonoItem, FnSigId>,
    mono_items: HashMap<MonoItem, Option<MonoItemTarget>>,
}

impl<'db> LowerCtxt<'db> {
    fn new(
        db: &'db mut Db,
        hir: &'db Hir,
        tir: &'db mut Tir,
        mono_items: HashSet<MonoItem>,
    ) -> Self {
        Self {
            db,
            hir,
            tir,
            mono_fns: HashMap::new(),
            mono_items: mono_items.into_iter().map(|i| (i, None)).collect(),
        }
    }

    fn get_mono_def(&mut self, mono_item: &MonoItem, instantiation: &Instantiation) -> DefId {
        let target_id = self.mono_items.get(mono_item).copied().flatten();

        if let Some(target_id) = target_id {
            // This is a polymorphic item that has already been monomorphized
            target_id
        } else {
            // This is a polymorphic item that needs monomorphization
            self.lower_mono_def(mono_item, instantiation)
        }
    }

    fn lower_item(&mut self, item: &hir::Item) {
        match &item.kind {
            hir::ItemKind::Fn(fun) => {
                if !self.db[fun.id].ty.is_polymorphic() {
                    self.lower_fn(fun);
                }
            }
            hir::ItemKind::Let(_) => todo!(),
        }
    }

    fn lower_mono_def(&mut self, mono_item: &MonoItem, instantiation: &Instantiation) -> DefId {
        for item in &self.hir.items {
            match &item.kind {
                hir::ItemKind::Fn(fun) if fun.id == mono_item.id => {
                    let mut new_fun = fun.clone();
                    new_fun.subst(&mut ParamFolder { db: self.db, instantiation });

                    let sig = self.lower_fn_sig(mono_item.id, &fun.sig);

                    self.mono_fns.insert(mono_item.clone(), sig);
                    self.lower_fn_body(sig, &new_fun);

                    break;
                }
                hir::ItemKind::Fn(_) | hir::ItemKind::Let(_) => (),
            }
        }

        panic!("function not found");
    }

    // TODO: LocalId Remove
    // fn alloc_mono_def(&mut self, mono_item: &MonoItem, instantiation: &Instantiation) -> DefId {
    //     let def = &self.db[mono_item.id];
    //
    //     let new_qpath = if def.kind.is_fn() {
    //         let args_str = instantiation
    //             .values()
    //             .map(|t| t.to_string(self.db))
    //             .collect::<Vec<String>>()
    //             .join("_");
    //
    //         if args_str.is_empty() {
    //             def.qpath.clone()
    //         } else {
    //             def.qpath.clone().with_name(ustr(&format!("{}${}", def.name, args_str)))
    //         }
    //     } else {
    //         def.qpath.clone()
    //     };
    //
    //     let new_scope = def.scope.clone();
    //     let new_kind = def.kind.as_ref().clone();
    //     let new_span = def.span;
    //
    //     let ty = def.ty;
    //     let new_ty = ParamFolder { db: self.db, instantiation }.fold(ty);
    //
    //     Def::alloc(self.db, new_qpath, new_scope, new_kind, new_ty, new_span)
    // }

    fn lower_fn(&mut self, f: &hir::Fn) {
        let sig = self.lower_fn_sig(f.id, &f.sig);
        self.lower_fn_body(sig, f);
    }

    fn lower_fn_sig(&mut self, def_id: DefId, sig: &hir::FnSig) -> FnSigId {
        let name = ustr(&self.db[def_id].qpath.standard_full_name());
        let ty = self.db[def_id].ty;

        let sig = FnSig {
            id: self.tir.sigs.next_key(),
            // TODO: don't use the name given from the def id. use a name given from outside
            name,
            params: sig
                .params
                .iter()
                .map(|p| FnParam {
                    // TODO: LocalId
                    def_id: self
                        .get_mono_def(&MonoItem { id: p.id, ty: p.ty }, &Instantiation::new()),
                    ty: p.ty,
                })
                .collect(),
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
        LowerFnCtxt::new(self).lower_fn(sig, f);
    }
}

struct LowerFnCtxt<'cx, 'db> {
    cx: &'cx mut LowerCtxt<'db>,
    exprs: Exprs,
}

impl<'cx, 'db> LowerFnCtxt<'cx, 'db> {
    fn new(cx: &'cx mut LowerCtxt<'db>) -> Self {
        Self { cx, exprs: IndexVec::new() }
    }

    fn lower_fn(mut self, sig: FnSigId, f: &hir::Fn) {
        let f = Fn {
            id: self.cx.tir.fns.next_key(),
            def_id: f.id,
            sig,
            body: self.lower_expr(&f.body),
            exprs: self.exprs,
        };

        if self.cx.db.main_function_id() == Some(f.def_id) {
            self.cx.tir.main_fn = Some(f.sig);
        }

        self.cx.tir.fns.push(f);
    }

    fn lower_expr(&mut self, expr: &hir::Expr) -> ExprId {
        let kind = match &expr.kind {
            hir::ExprKind::Let(let_) => {
                let value = self.lower_expr(&let_.value);

                match &let_.pat {
                    hir::Pat::Name(name) => ExprKind::Let {
                        // TODO: LocalId
                        def_id: self.cx.get_mono_def(
                            &MonoItem { id: name.id, ty: let_.value.ty },
                            &Instantiation::new(),
                        ),
                        value,
                    },
                    hir::Pat::Ignore(_) => ExprKind::UnitLit,
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
                    exprs.push(self.create_expr(ExprKind::UnitLit, expr.ty));
                }

                ExprKind::Block { exprs }
            }
            hir::ExprKind::Return(ret) => ExprKind::Return { value: self.lower_expr(&ret.expr) },
            hir::ExprKind::Call(call) => {
                // NOTE: Call arguments are expected to be sorted by parameter order, from left-to-right
                let args = call.args.iter().map(|a| self.lower_expr(&a.expr)).collect();
                let callee = self.lower_expr(&call.callee);
                ExprKind::Call { callee, args }
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
            hir::ExprKind::Name(name) => {
                let id = self
                    .cx
                    .get_mono_def(&MonoItem { id: name.id, ty: expr.ty }, &name.instantiation);

                ExprKind::Id { id: Id::Local(id) }
            }
            hir::ExprKind::Const(value) => match value {
                hir::Const::Int(value) => ExprKind::IntLit { value: *value },
                hir::Const::Bool(value) => ExprKind::BoolLit { value: *value },
                hir::Const::Unit => ExprKind::UnitLit,
            },
        };

        let new_expr = self.create_expr(kind, expr.ty);

        if let Some(coercions) = self.cx.db.coercions.get(&expr.id) {
            coercions.apply(&mut self.exprs, new_expr)
        } else {
            new_expr
        }
    }

    #[inline]
    pub fn create_expr(&mut self, kind: ExprKind, ty: Ty) -> ExprId {
        self.exprs.push_with_key(|id| Expr { id, kind, ty })
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
