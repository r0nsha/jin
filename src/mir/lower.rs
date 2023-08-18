use super::{builder::FunctionBuilder, DefinitionId, Function, Mir, Span, TyId, Value};
use crate::{
    db::{Database, ScopeLevel},
    hir::{self, Hir},
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
        if !self.builder.is_terminating() {
            let span = fun.body.span;
            self.builder.build_return(body_reg, span);
            self.build_unreachable(span);
        }

        self.builder.finish()
    }

    fn lower_block(&mut self, blk: &hir::Block) -> Value {
        let mut reg: Option<Value> = None;

        for expr in &blk.exprs {
            reg = Some(self.lower_expr(expr));
        }

        reg.unwrap_or_else(|| self.create_unit_register_with_ty(blk.ty, blk.span))
    }

    fn lower_expr(&mut self, expr: &hir::Expr) -> Value {
        match expr {
            hir::Expr::Function(_) => todo!("function expr"),
            hir::Expr::Block(blk) => self.lower_block(blk),
            hir::Expr::Return(ret) => self.lower_return(ret),
            hir::Expr::Call(ret) => self.lower_call(ret),
            hir::Expr::Binary(bin) => self.lower_binary(bin),
            hir::Expr::Name(ret) => self.lower_name(ret),
            hir::Expr::Lit(lit) => self.lower_lit(lit),
        }
    }

    fn lower_call(&mut self, call: &hir::Call) -> Value {
        let value = self.lower_expr(&call.callee);
        let reg = self.builder.create_register(call.ty);
        self.builder.build_call(reg, value, call.span);
        reg.into()
    }

    fn lower_binary(&mut self, bin: &hir::Binary) -> Value {
        let lhs = self.lower_expr(&bin.lhs);
        let rhs = self.lower_expr(&bin.rhs);
        let reg = self.builder.create_register(bin.ty);

        self.builder.build_binary(reg, bin.op, lhs, rhs, bin.span);

        reg.into()
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
            hir::LitKind::Unit => self.create_unit_register_with_ty(lit.ty, lit.span),
        }
    }

    #[allow(unused)]
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
