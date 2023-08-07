use std::io;

use pretty::{Arena, DocAllocator, DocBuilder, Pretty};

use crate::{db::Database, mir::*, ty::*};

pub(crate) fn codegen(db: &Database, mir: &Mir, writer: &mut impl io::Write) {
    let arena = Arena::new();

    CodegenCx::new(db, &arena)
        .codegen_all(&arena, mir)
        .write(&arena, writer)
}

struct CodegenCx<'db> {
    db: &'db Database,
    prelude: DocBuilder<'db, Arena<'db>>,
    declarations: Vec<DocBuilder<'db, Arena<'db>>>,
    definitions: Vec<DocBuilder<'db, Arena<'db>>>,
}

const TYPE_UNIT: &str = "Unit";
const TYPE_NEVER: &str = "Never";

const CONST_UNIT: &str = "unit";

impl<'db> CodegenCx<'db> {
    fn new(db: &'db Database, arena: &'db Arena<'db>) -> Self {
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

    fn codegen_all(mut self, arena: &'db Arena<'db>, mir: &Mir) -> Self {
        let fun_count = mir.functions.len();

        self.declarations.reserve(fun_count);
        self.definitions.reserve(fun_count);

        self.gen_main(&arena);

        for fun in &mir.functions {
            fun.codegen(&mut self, fun, &arena);
        }

        self
    }

    fn gen_main(&mut self, arena: &'db Arena<'db>) {
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

    fn add_definition(&mut self, def: DocBuilder<'db, Arena<'db>, ()>) {
        self.definitions.push(def);
    }

    fn add_declaration(&mut self, arena: &'db Arena<'db>, decl: DocBuilder<'db, Arena<'db>, ()>) {
        self.declarations.push(arena.statement(decl));
    }

    fn write(self, arena: &'db Arena<'db>, w: &mut impl io::Write) {
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

trait Codegen<'a, 'db> {
    fn codegen(
        &self,
        cx: &'a mut CodegenCx<'db>,
        fun: &'a Function,
        arena: &'db Arena<'db>,
    ) -> DocBuilder<'db, Arena<'db>, ()>;
}

impl<'a, 'db> Codegen<'a, 'db> for Function {
    fn codegen(
        &self,
        cx: &'a mut CodegenCx<'db>,
        fun: &'a Function,
        arena: &'db Arena<'db>,
    ) -> DocBuilder<'db, Arena<'db>, ()> {
        let fun = self.id().get(&cx.db);

        let fun_ty = fun.ty.get(&cx.db).kind.as_function().unwrap();
        let name = fun.name.full_c_name();

        let sig = arena
            .text(c_type(&fun_ty.ret))
            .append(arena.space())
            .append(arena.text(name))
            .append(arena.text("()"));

        cx.add_declaration(arena, sig.clone());

        let def = sig
            .append(arena.space())
            .append(arena.text("{"))
            .append(arena.line())
            .append(arena.intersperse(
                self.blocks().iter().map(|blk| blk.codegen(cx, self, arena)),
                arena.line(),
            ))
            .append(arena.text(";"))
            .nest(1)
            .append(arena.line())
            .append(arena.text("}"));

        cx.add_definition(def);

        arena.nil()
    }
}

impl<'a, 'db> Codegen<'a, 'db> for Block {
    fn codegen(
        &self,
        cx: &'a mut CodegenCx<'db>,
        fun: &'a Function,
        arena: &'db Arena<'db>,
    ) -> DocBuilder<'db, Arena<'db>, ()> {
        arena.intersperse(
            self.instructions
                .iter()
                .map(|inst| inst.codegen(cx, fun, arena)),
            arena.text(";").append(arena.line()),
        )
    }
}

impl<'a, 'db> Codegen<'a, 'db> for Instruction {
    fn codegen(
        &self,
        cx: &'a mut CodegenCx<'db>,
        fun: &'a Function,
        arena: &'db Arena<'db>,
    ) -> DocBuilder<'db, Arena<'db>, ()> {
        todo!()
    }
}

// impl<'a, 'db> Codegen<'a, 'db> for Return {
//     fn codegen(
//         &self,
//         cx: &'a mut CodegenCx<'db>,
//         arena: &'db Arena<'db>,
//     ) -> DocBuilder<'db, Arena<'db>, ()> {
//         arena.text("return").append(arena.space()).append(
//             self.expr
//                 .as_ref()
//                 .map_or_else(|| arena.nil(), |e| e.codegen(cx, arena)),
//         )
//     }
// }

// impl<'a, 'db> Codegen<'a, 'db> for Lit {
//     fn codegen(
//         &self,
//         _cx: &'a mut CodegenCx<'db>,
//         arena: &'db Arena<'db>,
//     ) -> DocBuilder<'db, Arena<'db>, ()> {
//         match &self.kind {
//             LitKind::Int(v) => arena.text(v.to_string()),
//             LitKind::Unit => arena.text(CONST_UNIT),
//         }
//     }
// }

fn c_type(ty: &Ty) -> String {
    match &ty.kind {
        TyKind::Int(int) => match int {
            IntTy::Int => "intptr_t",
        }
        .to_string(),
        TyKind::Function(_) => todo!(),
        TyKind::Unit => TYPE_UNIT.to_string(),
        TyKind::Never => TYPE_NEVER.to_string(),
        TyKind::Var(_) => panic!("unexpected type: {ty}"),
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
