use std::io;

use pretty::{Arena, DocAllocator, DocBuilder, Pretty};

use crate::{db::Database, hir::*, ty::*};

pub(crate) fn codegen(db: &Database, modules: &[Module], writer: &mut impl io::Write) {
    let arena = Arena::new();

    CodegenCx::new(db, &arena)
        .codegen_all(&arena, modules)
        .write(&arena, writer)
}

struct CodegenCx<'a> {
    db: &'a Database,
    prelude: DocBuilder<'a, Arena<'a>>,
    declarations: Vec<DocBuilder<'a, Arena<'a>>>,
    definitions: Vec<DocBuilder<'a, Arena<'a>>>,
}

const TYPE_UNIT: &str = "Unit";
const TYPE_NEVER: &str = "Never";

const CONST_UNIT: &str = "unit";

impl<'a> CodegenCx<'a> {
    fn new(db: &'a Database, arena: &'a Arena<'a>) -> Self {
        let includes = arena.text("#include <stdint.h>");

        let typedefs = arena.intersperse(
            [
                arena.text(format!("typedef void {TYPE_NEVER}")),
                arena.text(format!("typedef struct {{}} {TYPE_UNIT}")),
            ]
            .into_iter()
            .map(|d| arena.statement(d)),
            arena.line(),
        );

        let constants = arena.intersperse(
            [arena.text(format!("const {TYPE_UNIT} {CONST_UNIT} = {{}}"))]
                .into_iter()
                .map(|d| arena.statement(d)),
            arena.line(),
        );

        let prelude = arena.intersperse(
            [includes, typedefs, constants],
            arena.line().append(arena.line()),
        );

        CodegenCx {
            db,
            prelude,
            declarations: vec![],
            definitions: vec![],
        }
    }

    fn codegen_all(mut self, arena: &'a Arena<'a>, modules: &[Module]) -> Self {
        let binding_count = modules.iter().map(|m| m.bindings.len()).sum();

        self.declarations.reserve(binding_count);
        self.definitions.reserve(binding_count);

        self.gen_main(&arena);

        for module in modules {
            module.codegen(&mut self, &arena);
        }

        self
    }

    fn gen_main(&mut self, arena: &'a Arena<'a>) {
        let main_fun_name = self.db.main_fun().unwrap().name.full_c_name();

        self.add_definition(
            arena
                .text("int main() {")
                .append(arena.line())
                .append(
                    arena.intersperse(
                        [
                            arena.text(format!("{}()", main_fun_name)),
                            arena.text("return 0"),
                        ]
                        .map(|d| arena.statement(d)),
                        arena.line(),
                    ),
                )
                .nest(1)
                .append(arena.line())
                .append(arena.text("}")),
        );
    }

    fn add_definition(&mut self, def: DocBuilder<'a, Arena<'a>, ()>) {
        self.definitions.push(def);
    }

    fn add_declaration(&mut self, arena: &'a Arena<'a>, decl: DocBuilder<'a, Arena<'a>, ()>) {
        self.declarations.push(arena.statement(decl));
    }

    fn write(self, arena: &'a Arena<'a>, w: &mut impl io::Write) {
        self.prelude
            .append(arena.line())
            .append(arena.line())
            .append(arena.intersperse(
                self.declarations.into_iter().chain(self.definitions),
                arena.line().append(arena.line()),
            ))
            .render(80, w)
            .unwrap();
    }
}

trait Codegen<'a, 'cx> {
    fn codegen(
        &self,
        cx: &'a mut CodegenCx<'cx>,
        arena: &'cx Arena<'cx>,
    ) -> DocBuilder<'cx, Arena<'cx>, ()>;
}

impl<'a, 'cx> Codegen<'a, 'cx> for Module {
    fn codegen(
        &self,
        cx: &'a mut CodegenCx<'cx>,
        arena: &'cx Arena<'cx>,
    ) -> DocBuilder<'cx, Arena<'cx>, ()> {
        for binding in &self.bindings {
            binding.codegen(cx, arena);
        }

        arena.nil()
    }
}

impl<'a, 'cx> Codegen<'a, 'cx> for Hir {
    fn codegen(
        &self,
        cx: &'a mut CodegenCx<'cx>,
        arena: &'cx Arena<'cx>,
    ) -> DocBuilder<'cx, Arena<'cx>, ()> {
        match self {
            Hir::Fun(x) => x.codegen(cx, arena),
            Hir::Block(x) => x.codegen(cx, arena),
            Hir::Ret(x) => x.codegen(cx, arena),
            Hir::Lit(x) => x.codegen(cx, arena),
        }
    }
}

impl<'a, 'cx> Codegen<'a, 'cx> for Binding {
    fn codegen(
        &self,
        cx: &'a mut CodegenCx<'cx>,
        arena: &'cx Arena<'cx>,
    ) -> DocBuilder<'cx, Arena<'cx>, ()> {
        if let Hir::Fun(fun) = self.expr.as_ref() {
            fun.codegen(cx, arena)
        } else {
            todo!("local/global variable")
        }
    }
}

impl<'a, 'cx> Codegen<'a, 'cx> for Fun {
    fn codegen(
        &self,
        cx: &'a mut CodegenCx<'cx>,
        arena: &'cx Arena<'cx>,
    ) -> DocBuilder<'cx, Arena<'cx>, ()> {
        let fun_ty = self.ty.get(&cx.db).kind.as_fun().unwrap();
        let name = self.id.get(&cx.db).name.full_c_name();

        let sig = arena
            .text(c_type(&fun_ty.ret))
            .append(arena.space())
            .append(arena.text(name))
            .append(arena.text("()"));

        cx.add_declaration(arena, sig.clone());

        let def = sig
            .append(arena.space())
            .append(self.body.codegen(cx, arena));

        cx.add_definition(def);

        arena.nil()
    }
}

impl<'a, 'cx> Codegen<'a, 'cx> for Block {
    fn codegen(
        &self,
        cx: &'a mut CodegenCx<'cx>,
        arena: &'cx Arena<'cx>,
    ) -> DocBuilder<'cx, Arena<'cx>, ()> {
        arena
            .text("{")
            .append(arena.line())
            .append(arena.intersperse(
                self.exprs.iter().map(|e| e.codegen(cx, arena)),
                arena.line(),
            ))
            .append(arena.text(";"))
            .nest(1)
            .append(arena.line())
            .append(arena.text("}"))
    }
}

impl<'a, 'cx> Codegen<'a, 'cx> for Ret {
    fn codegen(
        &self,
        cx: &'a mut CodegenCx<'cx>,
        arena: &'cx Arena<'cx>,
    ) -> DocBuilder<'cx, Arena<'cx>, ()> {
        arena.text("return").append(arena.space()).append(
            self.expr
                .as_ref()
                .map_or_else(|| arena.nil(), |e| e.codegen(cx, arena)),
        )
    }
}

impl<'a, 'cx> Codegen<'a, 'cx> for Lit {
    fn codegen(
        &self,
        _cx: &'a mut CodegenCx<'cx>,
        arena: &'cx Arena<'cx>,
    ) -> DocBuilder<'cx, Arena<'cx>, ()> {
        match &self.kind {
            LitKind::Int(v) => arena.text(v.to_string()),
            LitKind::Unit => arena.text(CONST_UNIT),
        }
    }
}

fn c_type(ty: &Type) -> String {
    match &ty.kind {
        TypeKind::Int(int) => match int {
            IntType::Int => "intptr_t",
        }
        .to_string(),
        TypeKind::Fun(_) => todo!(),
        TypeKind::Unit => TYPE_UNIT.to_string(),
        TypeKind::Never => TYPE_NEVER.to_string(),
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
