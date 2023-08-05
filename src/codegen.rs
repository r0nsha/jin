use std::io;

use pretty::{Arena, Doc, DocAllocator, DocBuilder, RefDoc};

use crate::{hir::*, ty::*};

pub(crate) fn codegen(modules: &[Module]) {
    let arena = Arena::new();
    Codegen::new(&arena).gen(modules).write_to_stdout()
}

struct Codegen<'a> {
    arena: &'a Arena<'a>,
    prelude: DocBuilder<'a, Arena<'a>>,
    declarations: DocBuilder<'a, Arena<'a>>,
    definitions: DocBuilder<'a, Arena<'a>>,
}

impl<'a> Codegen<'a> {
    fn new(arena: &'a Arena<'a>) -> Self {
        let prelude = arena.nil();
        let declarations = arena.nil();
        let definitions = arena.nil();

        Codegen {
            arena,
            prelude,
            declarations,
            definitions,
        }
    }

    fn gen(mut self, modules: &[Module]) -> Self {
        self
    }

    fn write_to_stdout(self) {
        self.prelude
            .append(self.declarations)
            .append(self.definitions)
            .render(80, &mut io::stdout())
            .unwrap();
    }
}
// impl Codegen {
//     fn gen(&mut self, module: Module) {
//         self.prelude.push_str(
//             r#"#include <stdint.h>
// typedef void never;"#,
//         );
//
//         self.definitions.push_str(
//             r#"int main() {
//     main_main();
//     return 0;
// }
//
// "#,
//         );
//
//         for binding in &module.bindings {
//             let definitions = self.gen_binding(binding);
//             self.definitions.push_str(&definitions);
//         }
//     }
//
//     fn gen_ast(&mut self, ast: &Ast) -> String {
//         match ast {
//             Ast::Binding(binding) => self.gen_binding(binding),
//             Ast::Fun(fun) => self.gen_fun(fun),
//             Ast::Ret(ret) => self.gen_ret(ret),
//             Ast::Lit(lit) => self.gen_lit(lit),
//         }
//     }
//
//     fn gen_binding(&mut self, binding: &Binding) -> String {
//         match &binding.kind {
//             BindingKind::Fun { name, fun } => {
//                 let decl = format!(
//                     "{} {}()",
//                     c_type(fun.ty.as_ref().unwrap().kind.as_fun().unwrap().ret.as_ref()),
//                     name
//                 );
//
//                 self.push_declaration(&decl);
//
//                 let body = self.gen_ast(&fun.body);
//                 let body_str = format!("\t{body};\n");
//
//                 format!("{decl} {{\n{body_str}}}")
//             }
//         }
//     }
//
//     fn gen_fun(&mut self, fun: &Fun) -> String {
//         todo!("anonymous functions")
//         // let decl = format!(
//         //     "{} {}()",
//         //     c_type(fun.ty.as_ref().unwrap().as_fun().ret.as_ref()),
//         //     fun.name
//         // );
//         //
//         // self.add_declaration(&decl);
//         //
//         // let body = self.gen_ast(&fun.body);
//         // let body_str = format!("\t{body};\n");
//         //
//         // format!("{decl} {{\n{body_str}}}\n\n")
//     }
//
//     fn gen_ret(&mut self, ret: &Ret) -> String {
//         let value = if let Some(value) = ret.value.as_ref() {
//             self.gen_ast(value)
//         } else {
//             "".to_string()
//         };
//
//         format!("return {}", value)
//     }
//
//     fn gen_lit(&mut self, lit: &Lit) -> String {
//         match lit.kind {
//             LitKind::Int(value) => value.to_string(),
//         }
//     }
//
//     fn push_declaration(&mut self, decl: &str) {
//         self.declarations.reserve(decl.len() + 1);
//         self.declarations.push_str(decl);
//         self.declarations.push(';');
//     }
//
//     // fn push_definition(&mut self, def: &str) {
//     //     const SUFFIX: &str = "\n\n";
//     //     self.definitions.reserve(def.len() + SUFFIX.len());
//     //     self.definitions.push_str(def);
//     //     self.definitions.push_str(SUFFIX);
//     // }
// }
//
// fn c_type(ty: &Ty) -> String {
//     match &ty.kind {
//         TyKind::Int(int) => match int {
//             IntTy::Int => "intptr_t",
//         }
//         .to_string(),
//         TyKind::Fun(_) => todo!(),
//         TyKind::Unit | TyKind::Never => "void".to_string(),
//         TyKind::Var(_) => panic!("unexpected type: {ty}"),
//     }
// }
