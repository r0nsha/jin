use std::io;

use super::ast::*;

pub(super) fn print_module(module: &Module) -> io::Result<()> {
    let mut p = PrettyPrint {
        builder: ptree::TreeBuilder::new(module.name.standard_full_name()),
    };

    for binding in &module.bindings {
        p.print_binding(binding);
    }

    let tree = p.builder.build();
    ptree::print_tree_with(&tree, &ptree::PrintConfig::default())
}

struct PrettyPrint {
    builder: ptree::TreeBuilder,
}

impl PrettyPrint {
    fn print_ast(&mut self, ast: &Ast) {
        match ast {
            Ast::Ret(ret) => {
                self.builder.begin_child("return".to_string());

                if let Some(value) = ret.expr.as_ref() {
                    self.print_ast(value);
                }

                self.builder.end_child();
            }
            Ast::Lit(lit) => match lit.kind {
                LitKind::Int(value) => {
                    self.builder.add_empty_child(format!("int: {value}"));
                }
            },
        }
    }

    fn print_binding(&mut self, binding: &Binding) {
        match &binding.kind {
            BindingKind::Fun(fun) => self.print_fun(fun),
        }
    }

    fn print_fun(&mut self, fun: &Fun) {
        self.builder.begin_child(format!("fn {}", fun.name));

        self.builder.begin_child("body".to_string());

        self.print_ast(&fun.body);

        self.builder.end_child();
        self.builder.end_child();
    }
}
