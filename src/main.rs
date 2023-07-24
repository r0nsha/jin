mod ast;
mod codegen;
mod span;
mod ty;
mod typecheck;
mod state;

use std::{fs, os::unix::process::CommandExt, process::Command};

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

    println!("\n{code}");

    // TODO: don't create this out dir
    // TODO: handle error (ICE)
    fs::create_dir_all("out").unwrap();

    // TODO: rename file
    // TODO: handle error (ICE)
    fs::write("out/main.c", &code).unwrap();

    // TODO: rename input
    // TODO: rename output
    // TODO: handle error (ICE)
    Command::new("clang")
        .args(["out/main.c", "-o", "out/main", "-x", "c", "-std=c99"])
        .exec();
}
