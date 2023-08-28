use anyhow::Result;

use crate::{
    ast::BinOp,
    db::Db,
    mir::{builder::FunctionBuilder, Function, Mir, SymbolId, ValueId},
    span::Spanned,
    tast::{self, TypedAst},
    ty::{Ty, TyKind, Typed},
};

pub fn lower(db: &mut Db, tast: &TypedAst) -> Result<Mir> {
    let mut mir = Mir::new();

    for item in &tast.items {
        lower_item(db, &mut mir, item)?;
    }

    Ok(mir)
}

fn lower_item(db: &mut Db, mir: &mut Mir, item: &tast::Item) -> Result<()> {
    match &item.kind {
        tast::ItemKind::Function(fun) => {
            let fun = LowerFunctionCtxt::new(db, mir, fun.id).lower_function(fun)?;
            mir.add_function(fun);
        }
    }

    Ok(())
}

struct LowerFunctionCtxt<'db> {
    db: &'db mut Db,
    mir: &'db mut Mir,
    bx: FunctionBuilder,
}

impl<'db> LowerFunctionCtxt<'db> {
    fn new(db: &'db mut Db, mir: &'db mut Mir, fun_id: SymbolId) -> Self {
        Self { db, mir, bx: FunctionBuilder::new(fun_id) }
    }

    fn lower_function(mut self, fun: &tast::Fn) -> Result<Function> {
        let blk_start = self.bx.create_block("start");
        self.bx.position_at(blk_start);

        for param in &fun.sig.params {
            self.bx.create_param(param.id);
        }

        let body_value = self.lower_block(&fun.body);

        // Insert a final return instruction if the function's isn't terminating
        if !self.bx.current_block().is_terminating() {
            let span = fun.body.span;
            self.bx.build_return(body_value, span);
        }

        self.bx.finish()
    }

    fn lower_expr(&mut self, expr: &tast::Expr) -> ValueId {
        match expr {
            tast::Expr::Item(inner) => self.lower_local_item(inner),
            tast::Expr::If(inner) => self.lower_if(inner),
            tast::Expr::Block(inner) => self.lower_block(inner),
            tast::Expr::Return(inner) => self.lower_return(inner),
            tast::Expr::Call(inner) => self.lower_call(inner),
            tast::Expr::Bin(inner) => self.lower_bin(inner),
            tast::Expr::Name(inner) => self.lower_name(inner),
            tast::Expr::Lit(inner) => self.lower_lit(inner),
        }
    }

    fn lower_local_item(&mut self, item: &tast::Item) -> ValueId {
        lower_item(self.db, self.mir, item).expect("mir lowering to succeed");
        self.bx.build_unit_lit(item.ty, item.span())
    }

    fn lower_if(&mut self, if_: &tast::If) -> ValueId {
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

    fn lower_branch(&mut self, expr: &tast::Expr) -> Option<ValueId> {
        let value = self.lower_expr(expr);

        if self.bx.current_block().is_terminating() {
            None
        } else {
            Some(value)
        }
    }

    fn lower_block(&mut self, blk: &tast::Block) -> ValueId {
        let mut value: Option<ValueId> = None;

        for expr in &blk.exprs {
            value = Some(self.lower_expr(expr));
        }

        value.unwrap_or_else(|| self.bx.build_unit_lit(blk.ty, blk.span))
    }

    fn lower_call(&mut self, call: &tast::Call) -> ValueId {
        // NOTE: Call arguments are expected to be sorted by parameter order, from left-to-right
        let args = call.args.iter().map(|arg| self.lower_expr(&arg.expr)).collect::<Vec<_>>();
        let callee = self.lower_expr(&call.callee);
        self.bx.build_call(call.ty, callee, args, call.span)
    }

    fn lower_bin(&mut self, bin: &tast::Bin) -> ValueId {
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

    fn lower_return(&mut self, ret: &tast::Return) -> ValueId {
        if !self.bx.current_block().is_terminating() {
            let value = self.lower_expr(&ret.expr);
            self.bx.build_return(value, ret.span);
        }

        self.bx.build_unreachable(ret.expr.ty(), ret.span)
    }

    fn lower_name(&mut self, name: &tast::Name) -> ValueId {
        let symbol = &self.db[name.id];
        self.bx.build_load(symbol.ty, symbol.id, name.span)
    }

    fn lower_lit(&mut self, lit: &tast::Lit) -> ValueId {
        match &lit.kind {
            tast::LitKind::Int(v) => self.bx.build_int_lit(lit.ty, *v, lit.span),
            tast::LitKind::Bool(v) => self.bx.build_bool_lit(lit.ty, *v, lit.span),
            tast::LitKind::Unit => self.bx.build_unit_lit(lit.ty, lit.span),
        }
    }
}
