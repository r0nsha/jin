mod ast;
mod span;
mod ty;
mod typecheck;

use ast::Ast;
use span::Span;
use typecheck::typecheck;

fn main() {
    let mut ast = Ast::fun(
        "main",
        Ast::int(42, Span::unknown(), None),
        Span::unknown(),
        None,
    );
    let typed_ast = typecheck(ast);
    typed_ast.pretty_print().unwrap();
}
