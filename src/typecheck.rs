pub fn typecheck() {}

#[cfg(test)]
mod test {
    use crate::ast::Ast;

    use super::typecheck;

    #[test]
    fn empty_main() {
        let ast = Ast::fun("main", Ast::int(42));
        // assert_eq!(f(), 0);
    }
}
