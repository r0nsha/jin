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
            Hir::Block(block) => self.print_block(block),
            Hir::Fun(fun) => self.print_fun(fun),
            Hir::Ret(ret) => {
                self.builder.begin_child("return".to_string());

                if let Some(value) = ret.expr.as_ref() {
                    self.print_hir(value);
                }

                self.builder.end_child();
            }
            Hir::Lit(lit) => {
                let value = match lit.kind {
                    LitKind::Int(value) => value.to_string(),
                    LitKind::Unit => "()".to_string(),
                };

                self.builder
                    .add_empty_child(format!("{value} {}", self.print_ty(lit.ty)));
            }
        }
    }

    fn print_binding(&mut self, binding: &Binding) {
        self.builder.begin_child(format!(
            "let {} {}",
            binding.name,
            self.print_ty(binding.id.get(&self.db).ty)
        ));

        self.print_hir(&binding.expr);

        self.builder.end_child();
    }

    fn print_fun(&mut self, fun: &Fun) {
        self.builder
            .begin_child(format!("fn {} {}", fun.name, self.print_ty(fun.ty)));

        self.print_block(&fun.body);
    }

    fn print_block(&mut self, block: &Block) {
        self.builder
            .begin_child(format!("block {}", self.print_ty(block.ty)));

        for stmt in &block.exprs {
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
