use std::io;

use pretty::{Arena, DocAllocator, DocBuilder, Pretty};

use crate::{hir::*, ty::*};

pub(crate) fn codegen(modules: &[Module]) {
    let arena = Arena::new();
    Codegen::new(&arena).gen(modules).write_to_stdout()
}

struct Codegen<'a> {
    arena: &'a Arena<'a>,
    prelude: DocBuilder<'a, Arena<'a>>,
    declarations: Vec<DocBuilder<'a, Arena<'a>>>,
    definitions: Vec<DocBuilder<'a, Arena<'a>>>,
}

impl<'a> Codegen<'a> {
    fn new(arena: &'a Arena<'a>) -> Self {
        let prelude = arena
            .text("include <stdint.h>")
            .append(arena.line())
            .append(arena.line())
            .append(arena.statement(arena.text("typedef void never")));

        Codegen {
            arena,
            prelude,
            declarations: vec![],
            definitions: vec![],
        }
    }

    fn gen(mut self, modules: &[Module]) -> Self {
        //         self.definitions.push_str(
        //             r#"int main() {
        //     main_main();
        //     return 0;
        // }

        let binding_count = modules.iter().map(|m| m.bindings.len()).sum();

        self.declarations.reserve(binding_count);
        self.definitions.reserve(binding_count);

        self
    }

    fn add_definition(&mut self, def: DocBuilder<'a, Arena<'a>, ()>) {
        self.definitions.push(def);
    }

    fn add_declaration(&mut self, decl: DocBuilder<'a, Arena<'a>, ()>) {
        self.declarations.push(decl);
    }

    fn write_to_stdout(self) {
        println!();
        self.write(io::stdout());
        println!();
    }

    fn write(self, mut w: impl io::Write) {
        self.prelude
            .append(self.arena.line())
            .append(self.arena.intersperse(
                self.declarations.into_iter().chain(self.definitions),
                self.arena.line(),
            ))
            .render(80, &mut w)
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

trait ArenaExt<'a, A>
where
    Self: DocAllocator<'a, A>,
    Self::Doc: Clone,
    A: Clone + 'a,
    <Self as DocAllocator<'a, A>>::Doc: Pretty<'a, Self, A>,
{
    fn statement(&'a self, doc: DocBuilder<'a, Self, A>) -> DocBuilder<'a, Self, A> {
        self.nil().append(doc).append(";").group()
    }
}

impl<'a, A: Clone> ArenaExt<'a, A> for Arena<'a, A> {}
