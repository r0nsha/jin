use std::collections::{HashMap, HashSet};

use ustr::{ustr, Ustr};

use crate::{
    common::IndexVec,
    db::{Db, DefId, DefKind},
    hir,
    hir::Hir,
    passes::{
        subst::{ParamFolder, Subst},
        MonoItem,
    },
    tir::{
        Expr, ExprId, ExprKind, Exprs, Fn, FnParam, FnSig, FnSigId, Id, Local, LocalId, Locals, Tir,
    },
    ty::{
        coerce::{CoercionKind, Coercions},
        fold::TyFolder,
        Instantiation, Ty,
    },
};

pub fn lower(db: &mut Db, hir: &Hir, mono_items: HashSet<MonoItem>) -> Tir {
    let mut tir = Tir::new();
    let mut cx = LowerCtxt::new(db, hir, &mut tir, mono_items);

    cx.lower_all();

    tir
}

type MonoItemTarget = DefId;

struct LowerCtxt<'db> {
    db: &'db mut Db,
    hir: &'db Hir,
    tir: &'db mut Tir,

    // A mapping of functions to their predeclared signatures
    fn_to_sig: HashMap<DefId, FnSigId>,

    // Already monomorphized functions
    mono_fns: HashMap<MonoItem, FnSigId>,

    // TODO: remove
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
            fn_to_sig: HashMap::new(),
            mono_fns: HashMap::new(),
            mono_items: mono_items.into_iter().map(|i| (i, None)).collect(),
        }
    }

    fn lower_all(&mut self) {
        for item in &self.hir.items {
            self.declare_fn_item(item);
        }

        for item in &self.hir.items {
            self.lower_root_item(item);
        }
    }

    fn declare_fn_item(&mut self, item: &hir::Item) {
        match &item.kind {
            hir::ItemKind::Fn(fun) => {
                if !self.db[fun.id].ty.is_polymorphic() {
                    let sig = self.lower_fn_sig(fun.id, &fun.sig);
                    self.fn_to_sig.insert(fun.id, sig);
                }
            }
            hir::ItemKind::Let(_) => (),
        }
    }

    fn lower_root_item(&mut self, item: &hir::Item) {
        match &item.kind {
            hir::ItemKind::Fn(fun) => {
                if !self.db[fun.id].ty.is_polymorphic() {
                    let sig = self.fn_to_sig[&fun.id];
                    self.lower_fn_body(sig, fun);
                }
            }
            hir::ItemKind::Let(_) => todo!("global variables"),
        }
    }

    fn monomorphize_fn(&mut self, mono_item: &MonoItem, instantiation: &Instantiation) -> FnSigId {
        if let Some(target_id) = self.mono_fns.get(mono_item).copied() {
            println!(
                "found: {} + {} -> {}",
                self.db[mono_item.id].name,
                mono_item.ty.display(self.db),
                self.tir.sigs[target_id].ty.display(self.db)
            );
            return target_id;
        }

        for item in &self.hir.items {
            match &item.kind {
                hir::ItemKind::Fn(fun) if fun.id == mono_item.id => {
                    let mut new_fun = fun.clone();
                    new_fun.subst(&mut ParamFolder { db: self.db, instantiation });

                    let name = 
                    let sig = self.lower_fn_sig( mono_item.ty, &new_fun.sig);

                    self.mono_fns.insert(mono_item.clone(), sig);
                    self.lower_fn_body(sig, &new_fun);

                    return sig;
                }
                hir::ItemKind::Fn(_) | hir::ItemKind::Let(_) => (),
            }
        }

        panic!("function {} not found in hir.items", self.db[mono_item.id].qpath);
    }

    fn lower_fn_sig(&mut self, name: Ustr, ty: Ty, sig: &hir::FnSig) -> FnSigId {
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
        LowerFnCtxt::new(self).lower_fn(sig, f);
    }
}

struct LowerFnCtxt<'cx, 'db> {
    cx: &'cx mut LowerCtxt<'db>,
    exprs: Exprs,
    locals: Locals,
    def_to_local: HashMap<DefId, LocalId>,
}

impl<'cx, 'db> LowerFnCtxt<'cx, 'db> {
    fn new(cx: &'cx mut LowerCtxt<'db>) -> Self {
        Self { cx, exprs: IndexVec::new(), locals: IndexVec::new(), def_to_local: HashMap::new() }
    }

    fn lower_fn(mut self, sig: FnSigId, f: &hir::Fn) {
        for param in self.cx.tir.sigs[sig].params.clone() {
            self.create_local(param.def_id, param.ty);
        }

        let f = Fn {
            id: self.cx.tir.fns.next_key(),
            def_id: f.id,
            sig,
            body: self.lower_expr(&f.body),
            exprs: self.exprs,
            locals: self.locals,
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
                    hir::Pat::Name(name) => {
                        let ty = self.cx.db[name.id].ty;
                        // ParamFolder { db: self.cx.db, instantiation:&Instantiation }
                        //     .fold(self.cx.db[name.id].ty);
                        let id = self.create_local(name.id, ty);
                        ExprKind::Let { id, def_id: name.id, value }
                    }
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
            hir::ExprKind::Name(name) => match self.cx.db[name.id].kind.as_ref() {
                DefKind::Fn(_) => {
                    let id = if name.instantiation.is_empty() {
                        self.cx.fn_to_sig[&name.id]
                    } else {
                        let mut folder =
                            ParamFolder { db: self.cx.db, instantiation: &name.instantiation };
                        let ty = folder.fold(expr.ty);
                        let instantiation: Instantiation = name
                            .instantiation
                            .iter()
                            .map(|(var, ty)| (*var, folder.fold(*ty)))
                            .collect();

                        println!(
                            "monomorphizing: {} + {}",
                            self.cx.db[name.id].name,
                            ty.display(self.cx.db)
                        );
                        self.cx.monomorphize_fn(&MonoItem { id: name.id, ty }, &instantiation)
                    };

                    ExprKind::Id { id: Id::Fn(id) }
                }
                DefKind::Variable => ExprKind::Id { id: Id::Local(self.def_to_local[&name.id]) },
                DefKind::Ty(_) => unreachable!(),
            },
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

    #[inline]
    pub fn create_local(&mut self, def_id: DefId, ty: Ty) -> LocalId {
        let id =
            self.locals.push_with_key(|id| Local { id, def_id, name: self.cx.db[def_id].name, ty });
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
