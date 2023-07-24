mod ast;
mod codegen;
mod span;
mod ty;
mod typecheck;

use std::fs;

use ast::Ast;
use codegen::codegen;
use span::Span;
use typecheck::typecheck;

fn main() {
    let ast = Ast::fun(
        "main_main",
        Ast::ret(
            Some(Ast::int(42, Span::unknown(), None)),
            Span::unknown(),
            None,
        ),
        Span::unknown(),
        None,
    );

    let (typed_ast, _type_schema) = typecheck(ast).unwrap();

    typed_ast.pretty_print().unwrap();

    let code = codegen(&typed_ast);

    // TODO: don't create this out dir
    fs::create_dir("out").unwrap();
    // TODO: rename file
    fs::write("out/main.c", &code).unwrap();

    println!();
    println!("{code}");
}
