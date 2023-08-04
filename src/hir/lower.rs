use crate::{
    db::{self, Database},
    parse::ast::{self, Ast},
};

use super::*;

pub(crate) fn lower(db: &mut Database, modules: Vec<ast::Module>) -> Vec<Module> {
    modules
        .into_iter()
        .map(|module| {
            let id = db::Module::alloc(db, module.source, module.name.clone(), module.is_main);
            Lower { db, id }.run(module)
        })
        .collect()
}

struct Lower<'a> {
    db: &'a mut Database,
    id: ModuleId,
}

impl<'a> Lower<'a> {
    fn run(&mut self, module: ast::Module) -> Module {
        Module {
            id: self.id,
            bindings: module
                .bindings
                .into_iter()
                .map(|binding| self.lower_binding(binding))
                .collect(),
        }
    }

    fn lower_binding(&mut self, binding: ast::Binding) -> Binding {
        match binding.kind {
            ast::BindingKind::Fun(fun) => Binding {
                id: SymbolId::null(),
                name: fun.name,
                kind: BindingKind::Fun(self.lower_fun(fun)),
                span: binding.span,
                ty: TypeId::null(),
            },
        }
    }

    fn lower_fun(&mut self, fun: ast::Fun) -> Fun {
        Fun {
            id: FunId::null(),
            name: fun.name,
            body: Block {
                statements: vec![self.lower_ast(*fun.body)],
                span: fun.span,
                ty: TypeId::null(),
            },
            span: fun.span,
            ty: TypeId::null(),
        }
    }

    fn lower_ast(&mut self, ast: Ast) -> Hir {
        match ast {
            Ast::Ret(ret) => Hir::Ret(Ret {
                value: ret.value.map(|v| Box::new(self.lower_ast(*v))),
                span: ret.span,
                ty: TypeId::null(),
            }),
            Ast::Lit(lit) => Hir::Const(Const {
                kind: match lit.kind {
                    ast::LitKind::Int(v) => ConstKind::Int(v),
                },
                span: lit.span,
                ty: TypeId::null(),
            }),
        }
    }
}
