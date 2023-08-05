use std::io;

use pretty::{Arena, DocAllocator, DocBuilder, Pretty};

use crate::{db::Database, hir::*, ty::*};

pub(crate) fn codegen(db: &Database, modules: &[Module]) {
    let arena = Arena::new();
    CodegenCx::new(db, &arena).gen(modules).write_to_stdout()
}

struct CodegenCx<'a> {
    db: &'a Database,
    arena: &'a Arena<'a>,
    prelude: DocBuilder<'a, Arena<'a>>,
    declarations: Vec<DocBuilder<'a, Arena<'a>>>,
    definitions: Vec<DocBuilder<'a, Arena<'a>>>,
}

impl<'a> CodegenCx<'a> {
    fn new(db: &'a Database, arena: &'a Arena<'a>) -> Self {
        let includes = arena.text("include <stdint.h>");

        let typedefs = arena
            .statement(arena.text("typedef void never"))
            .append(arena.line())
            .append(arena.statement(arena.text("typedef struct {} unit")));

        let prelude = includes
            .append(arena.line())
            .append(arena.line())
            .append(typedefs);

        CodegenCx {
            db,
            arena,
            prelude,
            declarations: vec![],
            definitions: vec![],
        }
    }

    fn gen(mut self, modules: &[Module]) -> Self {
        let binding_count = modules.iter().map(|m| m.bindings.len()).sum();

        self.declarations.reserve(binding_count);
        self.definitions.reserve(binding_count);

        self.gen_main();

        for module in modules {
            module.gen(&mut self);
        }

        self
    }

    fn gen_main(&mut self) {
        let main_fun_name = self.db.main_fun().unwrap().name.name();

        self.add_definition(
            self.arena
                .text("int main() {")
                .append(self.arena.line())
                .append(
                    self.arena.intersperse(
                        [
                            self.arena.text(format!("{}()", main_fun_name)),
                            self.arena.text("return 0"),
                        ]
                        .map(|d| self.arena.statement(d)),
                        self.arena.line(),
                    ),
                )
                .nest(1)
                .append(self.arena.line())
                .append(self.arena.text("}")),
        );
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
            .append(self.arena.line())
            .append(self.arena.intersperse(
                self.declarations.into_iter().chain(self.definitions),
                self.arena.line().append(self.arena.line()),
            ))
            .render(80, &mut w)
            .unwrap();
    }
}

trait Gen<'a, 'cx> {
    fn gen(&self, cx: &'a mut CodegenCx<'cx>);
}

impl<'a, 'cx> Gen<'a, 'cx> for Module {
    fn gen(&self, cx: &'a mut CodegenCx<'cx>) {
        for binding in &self.bindings {
            binding.gen(cx);
        }
    }
}

impl<'a, 'cx> Gen<'a, 'cx> for Binding {
    fn gen(&self, cx: &'a mut CodegenCx<'cx>) {}
}

fn c_type(ty: &Type) -> String {
    match &ty.kind {
        TypeKind::Int(int) => match int {
            IntType::Int => "intptr_t",
        }
        .to_string(),
        TypeKind::Fun(_) => todo!(),
        TypeKind::Unit => "unit".to_string(),
        TypeKind::Never => "void".to_string(),
        TypeKind::Var(_) => panic!("unexpected type: {ty}"),
    }
}

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
