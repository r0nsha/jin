use crate::{db::Database, hir, ty::Ty};

use super::{builder::FunctionBuilder, *};

pub(crate) fn lower(db: &mut Database, hir: Vec<hir::Module>) -> Mir {
    let mut mir = Mir::new();

    for module in &hir {
        for def in &module.definitions {
            match &def.kind {
                hir::DefinitionKind::Function(fun) => {
                    let fun = LowerCx::new(db, fun.id).lower(fun).unwrap();
                    mir.add_function(fun);
                }
            }
        }
    }

    mir
}

struct LowerCx<'db> {
    db: &'db mut Database,
    builder: FunctionBuilder,
}

impl<'db> LowerCx<'db> {
    fn new(db: &'db mut Database, fun_id: FunctionId) -> Self {
        Self {
            db,
            builder: FunctionBuilder::new(fun_id),
        }
    }

    fn lower(mut self, fun: &hir::Function) -> Result<Function, String> {
        let blk_start = self.builder.create_block();
        self.builder.position_at(blk_start);

        self.lower_block(&fun.body);

        self.builder.finish()
    }

    fn lower_block(&mut self, blk: &hir::Block) -> RegisterId {
        let mut reg: Option<RegisterId> = None;

        for expr in &blk.exprs {
            reg = Some(self.lower_node(expr));
        }

        reg.unwrap_or_else(|| self.create_unit_register(blk.span))
    }

    fn lower_node(&mut self, node: &hir::Hir) -> RegisterId {
        match node {
            hir::Hir::Function(_) => todo!("function node"),
            hir::Hir::Block(blk) => self.lower_block(blk),
            hir::Hir::Return(ret) => self.lower_return(ret),
            hir::Hir::Lit(lit) => self.lower_lit(lit),
        }
    }

    fn lower_return(&mut self, ret: &hir::Return) -> RegisterId {
        let reg = if let Some(expr) = &ret.expr {
            self.lower_node(expr)
        } else {
            self.create_unit_register(ret.span)
        };

        // self.builder.build_return()

        todo!()
    }

    fn lower_lit(&mut self, lit: &hir::Lit) -> RegisterId {
        match &lit.kind {
            hir::LitKind::Int(value) => {
                let reg = self.builder.create_register(lit.ty);
                self.builder.build_int_lit(reg, *value, lit.span);
                reg
            }
            hir::LitKind::Unit => self.create_unit_register(lit.span),
        }
    }

    fn create_unit_register(&mut self, span: Span) -> RegisterId {
        let ty = Ty::alloc(&mut self.db, Ty::unit(span));
        self.create_unit_register_with_ty(ty, span)
    }

    fn create_unit_register_with_ty(&mut self, ty: TyId, span: Span) -> RegisterId {
        let reg = self.builder.create_register(ty);
        self.builder.build_unit_lit(reg, span);
        reg
    }
}
