use super::{builder::FunctionBuilder, DefinitionId, Function, Mir, Span, TyId, Value};
use crate::{
    ast::BinaryOp,
    db::{Database, ScopeLevel},
    hir::{self, Hir},
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

fn lower_def(db: &mut Database, mir: &mut Mir, def: &hir::Definition) {
    match &def.kind {
        hir::DefinitionKind::Function(fun) => {
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
    fn new(db: &'db mut Database, fun_id: DefinitionId) -> Self {
        Self { db, builder: FunctionBuilder::new(fun_id) }
    }

    fn lower_function(mut self, fun: &hir::Function) -> Result<Function, String> {
        let blk_start = self.builder.create_block("start");
        self.builder.position_at(blk_start);

        let body_reg = self.lower_block(&fun.body);

        // Insert a final return instruction if the function's isn't terminating
        if !self.builder.current_block().is_terminating() {
            let span = fun.body.span;
            self.builder.build_return(body_reg, span);
            self.build_unreachable(span);
        }

        self.builder.finish()
    }

    fn lower_expr(&mut self, expr: &hir::Expr) -> Value {
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

    fn lower_if(&mut self, if_: &hir::If) -> Value {
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
                let reg = self.builder.create_register(if_.ty);

                self.builder.build_phi(
                    reg,
                    vec![(then_blk, then_value), (else_blk, else_value)].into_boxed_slice(),
                    if_.span,
                );

                reg.into()
            }
            (Some(then_value), None) => then_value,
            (None, Some(else_value)) => else_value,
            (None, None) => self.build_unreachable(if_.span),
        }
    }

    fn lower_branch(&mut self, expr: &hir::Expr) -> Option<Value> {
        let value = self.lower_expr(expr);

        if self.builder.current_block().is_terminating() {
            None
        } else {
            Some(value)
        }
    }

    fn lower_block(&mut self, blk: &hir::Block) -> Value {
        let mut reg: Option<Value> = None;

        for expr in &blk.exprs {
            reg = Some(self.lower_expr(expr));
        }

        reg.unwrap_or_else(|| self.create_unit_register_with_ty(blk.ty, blk.span))
    }

    fn lower_call(&mut self, call: &hir::Call) -> Value {
        let value = self.lower_expr(&call.callee);
        let reg = self.builder.create_register(call.ty);
        self.builder.build_call(reg, value, call.span);
        reg.into()
    }

    fn lower_binary(&mut self, bin: &hir::Binary) -> Value {
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
                let false_value = self.create_bool_register(false, bin.span);
                self.builder.build_br(merge_blk, bin.span);

                self.builder.position_at(merge_blk);

                let reg = self.builder.create_register(bin.ty);

                self.builder.build_phi(
                    reg,
                    vec![(true_blk, true_value), (false_blk, false_value)].into_boxed_slice(),
                    bin.span,
                );

                reg.into()
            }
            BinaryOp::Or => {
                let true_blk = self.builder.create_block("or_true");
                let false_blk = self.builder.create_block("or_false");

                self.builder.build_brif(lhs, true_blk, false_blk, bin.span);

                let merge_blk = self.builder.create_block("or_merge");

                self.builder.position_at(true_blk);
                let true_value = self.create_bool_register(true, bin.span);
                self.builder.build_br(merge_blk, bin.span);

                self.builder.position_at(false_blk);
                let false_value = self.lower_expr(&bin.rhs);
                self.builder.build_br(merge_blk, bin.span);

                self.builder.position_at(merge_blk);

                let reg = self.builder.create_register(bin.ty);

                self.builder.build_phi(
                    reg,
                    vec![(true_blk, true_value), (false_blk, false_value)].into_boxed_slice(),
                    bin.span,
                );

                reg.into()
            }
            _ => {
                let rhs = self.lower_expr(&bin.rhs);
                let reg = self.builder.create_register(bin.ty);
                self.builder.build_binary(reg, bin.op, lhs, rhs, bin.span);
                reg.into()
            }
        }
    }

    fn lower_return(&mut self, ret: &hir::Return) -> Value {
        if !self.builder.current_block().is_terminating() {
            let reg = self.lower_expr(&ret.expr);
            self.builder.build_return(reg, ret.span);
        }

        self.build_unreachable(ret.span)
    }

    fn lower_name(&mut self, name: &hir::Name) -> Value {
        let def = &self.db[name.id.expect("to be resolved")];

        if let ScopeLevel::Global(_) = def.scope_level {
            Value::Definition(def.id)
        } else {
            todo!("local/nested name")
        }
    }

    fn lower_lit(&mut self, lit: &hir::Lit) -> Value {
        match &lit.kind {
            hir::LitKind::Int(value) => {
                let reg = self.builder.create_register(lit.ty);
                self.builder.build_int_lit(reg, *value, lit.span);
                reg.into()
            }
            hir::LitKind::Bool(value) => {
                let reg = self.builder.create_register(lit.ty);
                self.builder.build_bool_lit(reg, *value, lit.span);
                reg.into()
            }
            hir::LitKind::Unit => self.create_unit_register_with_ty(lit.ty, lit.span),
        }
    }

    fn create_bool_register(&mut self, value: bool, span: Span) -> Value {
        let ty = self.db.alloc_ty(Ty::Bool(span));
        let reg = self.builder.create_register(ty);
        self.builder.build_bool_lit(reg, value, span);
        reg.into()
    }

    fn create_unit_register(&mut self, span: Span) -> Value {
        let ty = self.db.alloc_ty(Ty::Unit(span));
        self.create_unit_register_with_ty(ty, span)
    }

    fn create_unit_register_with_ty(&mut self, ty: TyId, span: Span) -> Value {
        let reg = self.builder.create_register(ty);
        self.builder.build_unit_lit(reg, span);
        reg.into()
    }

    fn build_unreachable(&mut self, span: Span) -> Value {
        let ty = self.db.alloc_ty(Ty::Never(span));
        self.builder.create_register(ty).into()
    }
}
