use std::collections::HashMap;

use ustr::{ustr, Ustr};

use crate::{
    common::IndexVec,
    db::{Db, DefId, DefKind},
    hir,
    hir::Hir,
    passes::subst::{ParamFolder, Subst},
    tir::{
        Expr, ExprId, ExprKind, Exprs, Fn, FnParam, FnSig, FnSigId, Id, Local, LocalId, Locals, Tir,
    },
    ty::{
        coerce::{CoercionKind, Coercions},
        fold::TyFolder,
        Instantiation, Ty,
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

    // A mapping of functions to their predeclared signatures
    fn_to_sig: HashMap<DefId, FnSigId>,

    // Already monomorphized functions
    mono_fns: HashMap<MonoItem, FnSigId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoItem {
    pub id: DefId,
    pub ty: Ty,
}

impl<'db> LowerCtxt<'db> {
    fn new(db: &'db mut Db, hir: &'db Hir, tir: &'db mut Tir) -> Self {
        Self { db, hir, tir, fn_to_sig: HashMap::new(), mono_fns: HashMap::new() }
    }

    fn lower_all(&mut self) {
        // Declare fns
        for f in &self.hir.fns {
            if !self.db[f.id].ty.is_polymorphic() {
                let def = &self.db[f.id];
                let sig = self.lower_fn_sig(&f.sig, def.qpath.standard_full_name().into(), def.ty);
                self.fn_to_sig.insert(f.id, sig);
            }
        }

        for _ in &self.hir.lets {
            todo!("global variables");
        }

        for f in &self.hir.fns {
            if !self.db[f.id].ty.is_polymorphic() {
                let sig = self.fn_to_sig[&f.id];
                self.lower_fn_body(sig, f);
            }
        }
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
            panic!("function {} not found in hir.items", self.db[mono_item.id].qpath);
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
                    hir::Pat::Ignore(_) => ExprKind::UnitValue,
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
                    exprs.push(self.create_expr(ExprKind::UnitValue, expr.ty));
                }

                ExprKind::Block { exprs }
            }
            hir::ExprKind::Return(ret) => ExprKind::Return { value: self.lower_expr(&ret.expr) },
            hir::ExprKind::Call(call) => {
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
            hir::ExprKind::Name(name) => match self.cx.db[name.id].kind.as_ref() {
                DefKind::Fn(_) => {
                    let id = if name.instantiation.is_empty() {
                        self.cx.fn_to_sig[&name.id]
                    } else {
                        let mut folder =
                            ParamFolder { db: self.cx.db, instantiation: &name.instantiation };

                        let ty = folder.fold(expr.ty);

                        // let instantiation: Instantiation = name
                        //     .instantiation
                        //     .iter()
                        //     .map(|(var, ty)| (*var, folder.fold(*ty)))
                        //     .collect();

                        self.cx.monomorphize_fn(&MonoItem { id: name.id, ty }, &name.instantiation)
                    };

                    ExprKind::Id(Id::Fn(id))
                }
                DefKind::Variable => ExprKind::Id(Id::Local(self.def_to_local[&name.id])),
                DefKind::Ty(_) => unreachable!(),
            },
            hir::ExprKind::Const(value) => match value {
                hir::Const::Int(value) => ExprKind::IntValue(*value),
                hir::Const::Bool(value) => ExprKind::BoolValue(*value),
                hir::Const::Unit => ExprKind::UnitValue,
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
