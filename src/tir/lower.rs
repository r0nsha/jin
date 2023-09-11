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
    tir::{Expr, ExprId, ExprKind, Exprs, Fn, FnParam, FnSig, Tir},
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
    mono_items: HashMap<MonoItem, Option<MonoItemTarget>>,
}

impl<'db> LowerCtxt<'db> {
    fn new(
        db: &'db mut Db,
        hir: &'db Hir,
        tir: &'db mut Tir,
        mono_items: HashSet<MonoItem>,
    ) -> Self {
        Self { db, hir, tir, mono_items: mono_items.into_iter().map(|i| (i, None)).collect() }
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
        let new_def_id = self.alloc_mono_def(mono_item, instantiation);

        // Add the monomorphized item to the visited list
        self.mono_items.insert(mono_item.clone(), Some(new_def_id));

        // Monomorphize the item if needed
        let found_item = self.hir.items.iter().find(|item| match &item.kind {
            hir::ItemKind::Fn(f) => f.id == mono_item.id,
            hir::ItemKind::Let(_) => todo!(),
        });

        if let Some(item) = found_item {
            match &item.kind {
                hir::ItemKind::Fn(fun) => {
                    // Clone the function's contents and substitute its type args
                    let mut new_fun = fun.clone();
                    new_fun.id = new_def_id;
                    new_fun.subst(&mut ParamFolder { db: self.db, instantiation });

                    // Lower the newly created function to TIR
                    self.lower_fn(&new_fun);
                }
                hir::ItemKind::Let(_) => todo!(),
            }
        }

        new_def_id
    }

    fn alloc_mono_def(&mut self, mono_item: &MonoItem, instantiation: &Instantiation) -> DefId {
        let def = &self.db[mono_item.id];

        let new_qpath = if def.kind.is_fn() {
            let args_str = instantiation
                .values()
                .map(|t| t.to_string(self.db))
                .collect::<Vec<String>>()
                .join("_");

            if args_str.is_empty() {
                def.qpath.clone()
            } else {
                def.qpath.clone().with_name(ustr(&format!("{}${}", def.name, args_str)))
            }
        } else {
            def.qpath.clone()
        };

        let new_scope = def.scope.clone();
        let new_kind = def.kind.as_ref().clone();
        let new_span = def.span;

        let ty = def.ty;
        let new_ty = ParamFolder { db: self.db, instantiation }.fold(ty);

        Def::alloc(self.db, new_qpath, new_scope, new_kind, new_ty, new_span)
    }

    fn lower_fn(&mut self, f: &hir::Fn) {
        assert!(
            !self.db[f.id].ty.is_polymorphic(),
            "lowering polymorphic functions to TIR is not allowed"
        );
        let f = LowerFnCtxt::new(self).lower_fn(f);
        self.tir.functions.push(f);
    }
}

struct LowerFnCtxt<'cx, 'db> {
    inner: &'cx mut LowerCtxt<'db>,
    exprs: Exprs,
}

impl<'cx, 'db> LowerFnCtxt<'cx, 'db> {
    fn new(inner: &'cx mut LowerCtxt<'db>) -> Self {
        Self { inner, exprs: IndexVec::new() }
    }

    fn lower_fn(mut self, f: &hir::Fn) -> Fn {
        Fn {
            id: f.id,
            sig: FnSig {
                params: f
                    .sig
                    .params
                    .iter()
                    .map(|p| FnParam {
                        id: self
                            .inner
                            .get_mono_def(&MonoItem { id: p.id, ty: p.ty }, &Instantiation::new()),
                        ty: p.ty,
                    })
                    .collect(),
                ret: self.inner.db[f.id].ty.as_fn().unwrap().ret,
            },
            body: self.lower_expr(&f.body),
            exprs: self.exprs,
        }
    }

    fn lower_expr(&mut self, expr: &hir::Expr) -> ExprId {
        let kind = match &expr.kind {
            hir::ExprKind::Let(let_) => {
                let value = self.lower_expr(&let_.value);

                match &let_.pat {
                    hir::Pat::Name(name) => ExprKind::Let {
                        id: self.inner.get_mono_def(
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
                    .inner
                    .get_mono_def(&MonoItem { id: name.id, ty: expr.ty }, &name.instantiation);
                ExprKind::Name { id }
            }
            hir::ExprKind::Const(value) => match value {
                hir::Const::Int(value) => ExprKind::IntLit { value: *value },
                hir::Const::Bool(value) => ExprKind::BoolLit { value: *value },
                hir::Const::Unit => ExprKind::UnitLit,
            },
        };

        let new_expr = self.create_expr(kind, expr.ty);

        if let Some(coercions) = self.inner.db.coercions.get(&expr.id) {
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
