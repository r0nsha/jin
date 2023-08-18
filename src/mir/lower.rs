use super::{builder::FunctionBuilder, DefId, Function, Mir, Span};
use crate::{
    ast::BinaryOp,
    db::{Database, ScopeLevel},
    hir::{self, Hir},
    mir::ValueId,
    span::Spanned,
    ty::Ty,
};

pub fn lower(db: &mut Database, hir: &Hir) -> Mir {
    let mut mir = Mir::new();

    for module in &hir.modules {
        for def in &module.definitions {
            lower_def(db, &mut mir, def);
        }
    }

    mir
}

fn lower_def(db: &mut Database, mir: &mut Mir, def: &hir::Def) {
    match &def.kind {
        hir::DefKind::Function(fun) => {
            let id = fun.id.expect("to be resolved");
            let fun = LowerCx::new(db, id).lower_function(fun).unwrap();
            mir.add_function(fun);
        }
    }
}

struct LowerCx<'db> {
    db: &'db mut Database,
    builder: FunctionBuilder,
}

impl<'db> LowerCx<'db> {
    fn new(db: &'db mut Database, fun_id: DefId) -> Self {
        Self { db, builder: FunctionBuilder::new(fun_id) }
    }

    fn lower_function(mut self, fun: &hir::Function) -> Result<Function, String> {
        let blk_start = self.builder.create_block("start");
        self.builder.position_at(blk_start);

        let body_value = self.lower_block(&fun.body);

        // Insert a final return instruction if the function's isn't terminating
        if !self.builder.current_block().is_terminating() {
            let span = fun.body.span;
            self.builder.build_return(body_value, span);
            self.build_unreachable(span);
        }

        self.builder.finish()
    }

    fn lower_expr(&mut self, expr: &hir::Expr) -> ValueId {
        match expr {
            hir::Expr::Function(_) => todo!("function expr"),
            hir::Expr::If(inner) => self.lower_if(inner),
            hir::Expr::Block(inner) => self.lower_block(inner),
            hir::Expr::Return(inner) => self.lower_return(inner),
            hir::Expr::Call(inner) => self.lower_call(inner),
            hir::Expr::Binary(inner) => self.lower_binary(inner),
            hir::Expr::Name(inner) => self.lower_name(inner),
            hir::Expr::Lit(inner) => self.lower_lit(inner),
        }
    }

    fn lower_if(&mut self, if_: &hir::If) -> ValueId {
        let cond = self.lower_expr(&if_.cond);

        let then_blk = self.builder.create_block("if_then");
        let else_blk = self.builder.create_block("if_else");

        self.builder.build_brif(cond, then_blk, else_blk, if_.cond.span());

        let merge_blk = self.builder.create_block("if_merge");

        self.builder.position_at(then_blk);
        let then_value = self.lower_branch(&if_.then);
        self.builder.build_br(merge_blk, if_.span);

        self.builder.position_at(else_blk);
        let else_value = if_.otherwise.as_ref().and_then(|otherwise| self.lower_branch(otherwise));
        self.builder.build_br(merge_blk, if_.span);

        self.builder.position_at(merge_blk);

        match (then_value, else_value) {
            (Some(then_value), Some(else_value)) => {
                let value = self.builder.create_value(if_.ty);

                self.builder.build_phi(
                    value,
                    vec![(then_blk, then_value), (else_blk, else_value)].into_boxed_slice(),
                    if_.span,
                );

                value
            }
            (Some(then_value), None) => then_value,
            (None, Some(else_value)) => else_value,
            (None, None) => self.build_unreachable(if_.span),
        }
    }

    fn lower_branch(&mut self, expr: &hir::Expr) -> Option<ValueId> {
        let value = self.lower_expr(expr);

        if self.builder.current_block().is_terminating() {
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

        value.unwrap_or_else(|| self.create_unit_value(blk.span))
    }

    fn lower_call(&mut self, call: &hir::Call) -> ValueId {
        let callee = self.lower_expr(&call.callee);
        let value = self.builder.create_value(call.ty);
        self.builder.build_call(value, callee, call.span);
        value
    }

    fn lower_binary(&mut self, bin: &hir::Binary) -> ValueId {
        let lhs = self.lower_expr(&bin.lhs);

        match bin.op {
            BinaryOp::And => {
                let true_blk = self.builder.create_block("and_true");
                let false_blk = self.builder.create_block("and_false");

                self.builder.build_brif(lhs, true_blk, false_blk, bin.span);

                let merge_blk = self.builder.create_block("and_merge");

                self.builder.position_at(true_blk);
                let true_value = self.lower_expr(&bin.rhs);
                self.builder.build_br(merge_blk, bin.span);

                self.builder.position_at(false_blk);
                let false_value = self.create_bool_value(false, bin.span);
                self.builder.build_br(merge_blk, bin.span);

                self.builder.position_at(merge_blk);

                let result_value = self.builder.create_value(bin.ty);

                self.builder.build_phi(
                    result_value,
                    vec![(true_blk, true_value), (false_blk, false_value)].into_boxed_slice(),
                    bin.span,
                );

                result_value
            }
            BinaryOp::Or => {
                let true_blk = self.builder.create_block("or_true");
                let false_blk = self.builder.create_block("or_false");

                self.builder.build_brif(lhs, true_blk, false_blk, bin.span);

                let merge_blk = self.builder.create_block("or_merge");

                self.builder.position_at(true_blk);
                let true_value = self.create_bool_value(true, bin.span);
                self.builder.build_br(merge_blk, bin.span);

                self.builder.position_at(false_blk);
                let false_value = self.lower_expr(&bin.rhs);
                self.builder.build_br(merge_blk, bin.span);

                self.builder.position_at(merge_blk);

                let result_value = self.builder.create_value(bin.ty);

                self.builder.build_phi(
                    result_value,
                    vec![(true_blk, true_value), (false_blk, false_value)].into_boxed_slice(),
                    bin.span,
                );

                result_value
            }
            _ => {
                let rhs = self.lower_expr(&bin.rhs);
                let value = self.builder.create_value(bin.ty);
                self.builder.build_binary(value, bin.op, lhs, rhs, bin.span);
                value
            }
        }
    }

    fn lower_return(&mut self, ret: &hir::Return) -> ValueId {
        if !self.builder.current_block().is_terminating() {
            let value = self.lower_expr(&ret.expr);
            self.builder.build_return(value, ret.span);
        }

        self.build_unreachable(ret.span)
    }

    fn lower_name(&mut self, name: &hir::Name) -> ValueId {
        let def = &self.db[name.id.expect("to be resolved")];

        if let ScopeLevel::Global(_) = def.scope_level {
            let value = self.builder.create_value(def.ty);
            self.builder.build_load_global(value, def.id, name.span);
            value
        } else {
            todo!("local/nested name")
        }
    }

    fn lower_lit(&mut self, lit: &hir::Lit) -> ValueId {
        match &lit.kind {
            hir::LitKind::Int(v) => {
                let value = self.builder.create_value(lit.ty);
                self.builder.build_int_lit(value, *v, lit.span);
                value
            }
            hir::LitKind::Bool(v) => {
                let value = self.builder.create_value(lit.ty);
                self.builder.build_bool_lit(value, *v, lit.span);
                value
            }
            hir::LitKind::Unit => self.create_unit_value(lit.span),
        }
    }

    fn create_bool_value(&mut self, lit: bool, span: Span) -> ValueId {
        let ty = self.db.alloc_ty(Ty::Bool(span));
        let value = self.builder.create_value(ty);
        self.builder.build_bool_lit(value, lit, span);
        value
    }

    fn create_unit_value(&mut self, span: Span) -> ValueId {
        let ty = self.db.alloc_ty(Ty::Unit(span));
        let value = self.builder.create_value(ty);
        self.builder.build_unit_lit(value, span);
        value
    }

    fn build_unreachable(&mut self, span: Span) -> ValueId {
        let ty = self.db.alloc_ty(Ty::Never(span));
        self.builder.create_value(ty)
    }
}
