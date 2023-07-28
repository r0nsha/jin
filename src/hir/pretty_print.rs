use std::io;

use super::*;

pub fn print_hir(cache: &Cache) -> io::Result<()> {
    let mut p = PrettyPrint {
        builder: ptree::TreeBuilder::new("hir".to_string()),
        cache,
    };

    let tree = p.builder.build();
    ptree::print_tree_with(&tree, &ptree::PrintConfig::default())
}

struct PrettyPrint<'a> {
    builder: ptree::TreeBuilder,
    cache: &'a Cache,
}

impl<'a> PrettyPrint<'a> {
    fn print(&mut self) {
        for binding in self.cache.global_bindings.values() {
            self.print_binding(binding);
        }
    }

    fn print_hir(&mut self, hir: &Hir) {
        match hir {
            Hir::Binding(fun) => self.print_binding(fun),
            // Hir::Fun(fun) => self.print_fun(fun),
            Hir::Ret(ret) => {
                self.builder.begin_child("return".to_string());

                if let Some(value) = ret.value.as_ref() {
                    self.print_hir(value);
                }

                self.builder.end_child();
            }
            Hir::Lit(lit) => match lit.kind {
                LitKind::Int(value) => {
                    self.builder
                        .add_empty_child(format!("{value} {}", Self::print_ty(&lit.ty)));
                }
                LitKind::Unit => {
                    self.builder
                        .add_empty_child(format!("() {}", Self::print_ty(&lit.ty)));
                }
            },
        }
    }

    fn print_binding(&mut self, binding: &Binding) {
        let name = self
            .cache
            .get_binding_info(binding.id)
            .unwrap()
            .qualified_name
            .standard_full_name();

        match &binding.kind {
            BindingKind::Value(_) => todo!(),
            BindingKind::Fun(fun) => match &fun.kind {
                FunKind::Orphan { body } => {
                    self.builder.begin_child(format!(
                        "fn {} {}",
                        name,
                        Self::print_ty(&binding.ty)
                    ));
                    self.print_block(body);
                }
            },
        }
    }

    fn print_block(&mut self, block: &Block) {
        self.builder
            .begin_child(format!("block {}", Self::print_ty(&block.ty)));

        for stmt in &block.statements {
            self.print_hir(stmt);
        }

        self.builder.end_child();
        self.builder.end_child();
    }

    fn print_ty(ty: &Ty) -> String {
        format!("(ty: {ty})")
    }
}
