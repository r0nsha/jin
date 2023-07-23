mod ast;
mod codegen;
mod span;
mod ty;
mod typecheck;

use ast::Ast;
use codegen::codegen;
use span::Span;
use typecheck::typecheck;

fn main() {
    let mut ast = Ast::fun(
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

    println!();
    println!("{code}");
}
