use anyhow::Result;
use ustr::ustr;

use crate::{
    ast::BinOp,
    db::{Db, Def, DefKind},
    hir::{self, Hir},
    mir::{builder::FunctionBuilder, DefId, Function, Mir, ValueId},
    passes::subst::{Subst, SubstTy},
    span::{Span, Spanned},
    ty::{fold::TyFolder, Ty, TyKind, Typed},
};

pub fn lower(db: &mut Db, hir: &Hir) -> Result<Mir> {
    let mut mir = Mir::new();
    let mut cx = LowerCtxt::new(db, hir, &mut mir);

    for item in &hir.items {
        cx.lower_item(item)?;
    }

    Ok(mir)
}

struct LowerCtxt<'db> {
    db: &'db mut Db,
    hir: &'db Hir,
    mir: &'db mut Mir,
    mono_items: Vec<MonoItem>,
}

impl<'db> LowerCtxt<'db> {
    fn new(db: &'db mut Db, hir: &'db Hir, mir: &'db mut Mir) -> Self {
        Self { db, hir, mir, mono_items: vec![] }
    }

    fn lower_item(&mut self, item: &hir::Item) -> Result<()> {
        match &item.kind {
            hir::ItemKind::Fn(fun) => {
                if fun.ty.is_polymorphic() {
                    Ok(())
                } else {
                    self.lower_fn(fun)
                }
            }
        }
    }

    fn lower_mono_def(&mut self, id: DefId, args: &[Ty]) -> DefId {
        let new_def_id = self.alloc_mono_def(id, args);

        // Monomorphize the item if needed
        let item = self.hir.items.iter().find(|item| match &item.kind {
            hir::ItemKind::Fn(f) => f.id == id,
        });

        if let Some(item) = item {
            match &item.kind {
                hir::ItemKind::Fn(fun) => {
                    // Add the monomorphized item to the visited list
                    self.mono_items.push(MonoItem {
                        args: args.to_owned(),
                        source_id: fun.id,
                        target_id: new_def_id,
                    });

                    // Clone the function's contents and substitute its type args
                    let mut new_fun = fun.clone();
                    new_fun.id = new_def_id;
                    new_fun.subst(&mut ParamFolder { db: self.db, args });

                    // Lower the newly created function to MIR
                    self.lower_fn(&new_fun).expect("lowering MIR to work");
                }
            }
        }

        new_def_id
    }

    fn alloc_mono_def(&mut self, id: DefId, args: &[Ty]) -> DefId {
        let def = &self.db[id];

        let new_qpath = if def.scope.level.is_local() && matches!(def.kind.as_ref(), DefKind::Fn(_))
        {
            def.qpath.clone()
        } else {
            let args_str =
                args.iter().map(|t| t.to_string(self.db)).collect::<Vec<String>>().join("_");
            def.qpath.clone().with_name(ustr(&format!("{}${}", def.name, args_str)))
        };

        let new_scope = def.scope.clone();
        let new_kind = def.kind.as_ref().clone();
        let new_span = def.span;

        let ty = def.ty;
        let new_ty = ParamFolder { db: self.db, args }.fold(ty);

        Def::alloc(self.db, new_qpath, new_scope, new_kind, new_ty, new_span)
    }

    fn lower_fn(&mut self, fun: &hir::Fn) -> Result<(), anyhow::Error> {
        assert!(!fun.ty.is_polymorphic(), "lowering polymorphic functions to MIR is not allowed");
        let fun = LowerFunctionCtxt::new(self, fun.id).lower_fn(fun)?;
        self.mir.add_function(fun);
        Ok(())
    }

    fn lookup_mono_item(&self, id: DefId, tys: &[Ty]) -> Option<&MonoItem> {
        self.mono_items.iter().find(|item| item.source_id == id && item.args == tys)
    }
}

struct LowerFunctionCtxt<'cx, 'db> {
    inner: &'cx mut LowerCtxt<'db>,
    bx: FunctionBuilder,
}

#[derive(Debug)]
struct MonoItem {
    args: Vec<Ty>,
    source_id: DefId,
    target_id: DefId,
}

impl<'cx, 'db> LowerFunctionCtxt<'cx, 'db> {
    fn new(inner: &'cx mut LowerCtxt<'db>, fun_id: DefId) -> Self {
        Self { inner, bx: FunctionBuilder::new(fun_id) }
    }

    fn lower_fn(mut self, fun: &hir::Fn) -> Result<Function> {
        let blk_start = self.bx.create_block("start");
        self.bx.position_at(blk_start);

        for param in &fun.sig.params {
            self.bx.create_param(param.id);
        }

        let body_value = self.lower_block(&fun.body);

        // Insert a final return instruction if the function's isn't terminating
        if !self.bx.current_block().is_terminating() {
            let span = fun.body.span;

            let ret_ty = fun.ty.as_fn().unwrap().ret;
            let ret_value = if ret_ty.is_unit() && !self.bx.value(body_value).unwrap().ty.is_unit()
            {
                self.bx.build_unit_lit(ret_ty, fun.body.span)
            } else {
                body_value
            };

            self.bx.build_return(ret_value, span);
        }

        self.bx.finish()
    }

    fn lower_expr(&mut self, expr: &hir::Expr) -> ValueId {
        match expr {
            hir::Expr::Item(inner) => self.lower_local_item(inner),
            hir::Expr::If(inner) => self.lower_if(inner),
            hir::Expr::Block(inner) => self.lower_block(inner),
            hir::Expr::Return(inner) => self.lower_return(inner),
            hir::Expr::Call(inner) => self.lower_call(inner),
            hir::Expr::Bin(inner) => self.lower_bin(inner),
            hir::Expr::Name(inner) => self.lower_name(inner),
            hir::Expr::Lit(inner) => self.lower_lit(inner),
        }
    }

    fn lower_local_item(&mut self, item: &hir::Item) -> ValueId {
        self.inner.lower_item(item).expect("mir lowering to succeed");
        self.bx.build_unit_lit(item.ty, item.span())
    }

    fn lower_if(&mut self, if_: &hir::If) -> ValueId {
        let cond = self.lower_expr(&if_.cond);

        let then_blk = self.bx.create_block("if_then");
        let else_blk = self.bx.create_block("if_else");

        self.bx.build_brif(cond, then_blk, else_blk, if_.cond.span());

        let merge_blk = self.bx.create_block("if_merge");

        self.bx.position_at(then_blk);
        let then_value = self.lower_branch(&if_.then);
        self.bx.build_br(merge_blk, if_.span);

        self.bx.position_at(else_blk);
        let else_value = if let Some(otherwise) = &if_.otherwise {
            self.lower_branch(otherwise)
        } else {
            Some(self.bx.build_unit_lit(Ty::new(TyKind::Unit), if_.span))
        };

        self.bx.build_br(merge_blk, if_.span);

        self.bx.position_at(merge_blk);

        match (then_value, else_value) {
            (Some(then_value), Some(else_value)) => self.bx.build_phi(
                if_.ty,
                vec![(then_blk, then_value), (else_blk, else_value)].into_boxed_slice(),
                if_.span,
            ),
            (Some(then_value), None) => then_value,
            (None, Some(else_value)) => else_value,
            (None, None) => self.bx.build_unreachable(if_.ty, if_.span),
        }
    }

    fn lower_branch(&mut self, expr: &hir::Expr) -> Option<ValueId> {
        let value = self.lower_expr(expr);

        if self.bx.current_block().is_terminating() {
            None
        } else {
            Some(value)
        }
    }

    fn lower_block(&mut self, blk: &hir::Block) -> ValueId {
        let mut value: Option<ValueId> = None;

        for expr in &blk.exprs {
            value = Some(self.lower_expr(expr));
        }

        value.unwrap_or_else(|| self.bx.build_unit_lit(blk.ty, blk.span))
    }

    fn lower_call(&mut self, call: &hir::Call) -> ValueId {
        // NOTE: Call arguments are expected to be sorted by parameter order, from left-to-right
        let args = call.args.iter().map(|arg| self.lower_expr(&arg.expr)).collect::<Vec<_>>();
        let callee = self.lower_expr(&call.callee);
        self.bx.build_call(call.ty, callee, args, call.span)
    }

    fn lower_bin(&mut self, bin: &hir::Bin) -> ValueId {
        let lhs = self.lower_expr(&bin.lhs);

        match bin.op {
            BinOp::And => {
                let true_blk = self.bx.create_block("and_true");
                let false_blk = self.bx.create_block("and_false");

                self.bx.build_brif(lhs, true_blk, false_blk, bin.span);

                let merge_blk = self.bx.create_block("and_merge");

                self.bx.position_at(true_blk);
                let true_value = self.lower_expr(&bin.rhs);
                self.bx.build_br(merge_blk, bin.span);

                self.bx.position_at(false_blk);
                let false_value = self.bx.build_bool_lit(bin.ty, false, bin.span);
                self.bx.build_br(merge_blk, bin.span);

                self.bx.position_at(merge_blk);

                self.bx.build_phi(
                    bin.ty,
                    vec![(true_blk, true_value), (false_blk, false_value)].into_boxed_slice(),
                    bin.span,
                )
            }
            BinOp::Or => {
                let true_blk = self.bx.create_block("or_true");
                let false_blk = self.bx.create_block("or_false");

                self.bx.build_brif(lhs, true_blk, false_blk, bin.span);

                let merge_blk = self.bx.create_block("or_merge");

                self.bx.position_at(true_blk);
                let true_value = self.bx.build_bool_lit(bin.ty, true, bin.span);
                self.bx.build_br(merge_blk, bin.span);

                self.bx.position_at(false_blk);
                let false_value = self.lower_expr(&bin.rhs);
                self.bx.build_br(merge_blk, bin.span);

                self.bx.position_at(merge_blk);

                self.bx.build_phi(
                    bin.ty,
                    vec![(true_blk, true_value), (false_blk, false_value)].into_boxed_slice(),
                    bin.span,
                )
            }
            _ => {
                let rhs = self.lower_expr(&bin.rhs);
                self.bx.build_bin(bin.ty, bin.op, lhs, rhs, bin.span)
            }
        }
    }

    fn lower_return(&mut self, ret: &hir::Return) -> ValueId {
        if !self.bx.current_block().is_terminating() {
            let value = self.lower_expr(&ret.expr);
            self.bx.build_return(value, ret.span);
        }

        self.bx.build_unreachable(ret.expr.ty(), ret.span)
    }

    fn lower_name(&mut self, name: &hir::Name) -> ValueId {
        let id = if name.args.is_empty() {
            // This is a monomorphic item
            name.id
        } else if let Some(item) = self.inner.lookup_mono_item(name.id, &name.args) {
            // This is a polymorphic item that has already been monomorphized
            item.target_id
        } else {
            // This is a polymorphic item that needs monomorphization
            self.inner.lower_mono_def(name.id, &name.args)
        };

        let def = &self.inner.db[id];
        dbg!(def);

        self.bx.build_load(def.ty, def.id, name.span)
    }

    fn lower_lit(&mut self, lit: &hir::Lit) -> ValueId {
        match &lit.kind {
            hir::LitKind::Int(v) => self.bx.build_int_lit(lit.ty, *v, lit.span),
            hir::LitKind::Bool(v) => self.bx.build_bool_lit(lit.ty, *v, lit.span),
            hir::LitKind::Unit => self.bx.build_unit_lit(lit.ty, lit.span),
        }
    }
}

struct ParamFolder<'db, 'a> {
    db: &'db mut Db,
    args: &'a [Ty],
}

impl SubstTy for ParamFolder<'_, '_> {
    fn subst_ty(&mut self, ty: Ty, _: Span) -> Ty {
        self.fold(ty)
    }

    fn db(&mut self) -> &mut Db {
        self.db
    }
}

impl TyFolder for ParamFolder<'_, '_> {
    fn fold(&mut self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Param(p) => self.args[p.index],
            _ => self.super_fold(ty),
        }
    }
}
