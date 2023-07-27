use std::io;

use super::{Ast, Binding, BindingKind, Fun, LitKind, Module};

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
            Ast::Binding(fun) => self.print_binding(fun),
            Ast::Fun(fun) => self.print_fun(fun),
            Ast::Ret(ret) => {
                self.builder.begin_child("return".to_string());

                if let Some(value) = ret.value.as_ref() {
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
            BindingKind::Fun { name, fun } => {
                self.builder.begin_child(format!("fn {}", name));
                self.print_fun_body(fun);
            }
        }
    }

    fn print_fun(&mut self, fun: &Fun) {
        self.builder.begin_child("fn".to_string());
        self.print_fun_body(fun);
    }

    fn print_fun_body(&mut self, fun: &Fun) {
        self.builder.begin_child("body".to_string());

        self.print_ast(&fun.body);

        self.builder.end_child();
        self.builder.end_child();
    }
}
