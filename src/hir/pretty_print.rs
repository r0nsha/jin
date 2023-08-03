use std::io;

use crate::db::{Database, TypeId};

use super::*;

pub(crate) fn print_module(db: &Database, module: &Module) -> io::Result<()> {
    let mut p = PrettyPrint {
        db,
        builder: ptree::TreeBuilder::new(module.id.get(db).name.standard_full_name()),
    };

    p.print(module);

    let tree = p.builder.build();
    ptree::print_tree_with(&tree, &ptree::PrintConfig::default())
}

struct PrettyPrint<'a> {
    db: &'a Database,
    builder: ptree::TreeBuilder,
}

impl<'a> PrettyPrint<'a> {
    fn print(&mut self, module: &Module) {
        for binding in &module.bindings {
            self.print_binding(binding);
        }
    }

    fn print_hir(&mut self, hir: &Hir) {
        match hir {
            Hir::Ret(ret) => {
                self.builder.begin_child("return".to_string());

                if let Some(value) = ret.value.as_ref() {
                    self.print_hir(value);
                }

                self.builder.end_child();
            }
            Hir::Const(lit) => {
                let value = match lit.kind {
                    ConstKind::Int(value) => value.to_string(),
                    ConstKind::Unit => "()".to_string(),
                };

                self.builder
                    .add_empty_child(format!("{value} {}", self.print_ty(lit.ty)));
            }
        }
    }

    fn print_binding(&mut self, binding: &Binding) {
        match &binding.kind {
            BindingKind::Fun(fun) => {
                self.builder
                    .begin_child(format!("fn {} {}", fun.name, self.print_ty(binding.ty)));

                self.print_block(&fun.body);
            }
        }
    }

    fn print_block(&mut self, block: &Block) {
        self.builder
            .begin_child(format!("block {}", self.print_ty(block.ty)));

        for stmt in &block.statements {
            self.print_hir(stmt);
        }

        self.builder.end_child();
        self.builder.end_child();
    }

    fn print_ty(&self, ty: TypeId) -> String {
        format!(
            "(type: {})",
            self.db
                .types
                .get(ty)
                .map_or("?".to_string(), |ty| ty.to_string())
        )
    }
}
