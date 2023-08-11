use crate::db::ScopeLevel;
use crate::{
    db::Database,
    hir::{self, Hir},
    ty::Ty,
};

use super::{builder::FunctionBuilder, *};

pub(crate) fn lower(db: &mut Database, hir: Hir) -> Mir {
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
            let fun = LowerCx::new(db, id).lower(fun).unwrap();
            mir.add_function(fun);
        }
    }
}

struct LowerCx<'db> {
    db: &'db mut Database,
    builder: FunctionBuilder,
}

impl<'db> LowerCx<'db> {
    fn new(db: &'db mut Database, fun_id: FunctionId) -> Self {
        Self { db, builder: FunctionBuilder::new(fun_id) }
    }

    fn lower(mut self, fun: &hir::Function) -> Result<Function, String> {
        let blk_start = self.builder.create_block("start");
        self.builder.position_at(blk_start);

        self.lower_block(&fun.body);

        self.builder.finish()
    }

    fn lower_block(&mut self, blk: &hir::Block) -> Value {
        let mut reg: Option<Value> = None;

        for expr in &blk.exprs {
            reg = Some(self.lower_node(expr));
        }

        reg.unwrap_or_else(|| {
            self.create_unit_register_with_ty(blk.ty, blk.span)
        })
    }

    fn lower_node(&mut self, node: &hir::Node) -> Value {
        match node {
            hir::Node::Function(_) => todo!("function node"),
            hir::Node::Block(blk) => self.lower_block(blk),
            hir::Node::Return(ret) => self.lower_return(ret),
            hir::Node::Name(ret) => self.lower_name(ret),
            hir::Node::Lit(lit) => self.lower_lit(lit),
        }
    }

    fn lower_return(&mut self, ret: &hir::Return) -> Value {
        let reg = if let Some(expr) = &ret.expr {
            self.lower_node(expr)
        } else {
            self.create_unit_register(ret.span)
        };

        self.builder.build_return(reg, ret.span);
        self.build_unreachable(ret.span)
    }

    fn lower_name(&mut self, name: &hir::Name) -> Value {
        let sym = name.id.expect("to be resolved").get(self.db);

        if let ScopeLevel::Global(_) = sym.scope_level {
            Value::Symbol(sym.id)
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
            hir::LitKind::Unit => {
                self.create_unit_register_with_ty(lit.ty, lit.span)
            }
        }
    }

    fn create_unit_register(&mut self, span: Span) -> Value {
        let ty = Ty::alloc(self.db, Ty::unit(span));
        self.create_unit_register_with_ty(ty, span)
    }

    fn create_unit_register_with_ty(&mut self, ty: TyId, span: Span) -> Value {
        let reg = self.builder.create_register(ty);
        self.builder.build_unit_lit(reg, span);
        reg.into()
    }

    fn build_unreachable(&mut self, span: Span) -> Value {
        let ty = Ty::alloc(self.db, Ty::never(span));
        self.builder.create_register(ty).into()
    }
}
