use crate::{
    ast::{Ast, Module},
    lexer::{Token, TokenKind},
    span::Source,
};

pub fn parse(source: &Source, tokens: Vec<Token>) -> Module {
    Parser::new(source, tokens).parse()
}

#[derive(Debug)]
struct Parser<'a> {
    source: &'a Source,
    tokens: Vec<Token>,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(source: &'a Source, tokens: Vec<Token>) -> Self {
        Self {
            source,
            tokens,
            pos: 0,
        }
    }

    fn parse(&mut self) -> Module {
        let mut module = Module { bindings: vec![] };

        while self.pos < self.tokens.len() - 1 {
            self.parse_top_level(&mut module);
        }

        module
    }

    fn parse_top_level(&mut self, module: &mut Module) {}

    fn is(&mut self, token: TokenKind) -> bool {
        match self.tokens.get(self.pos) {
            Some(tok) if tok.kind == token => {
                self.bump();
                true
            }
            _ => false,
        }
    }

    // TODO: is_any

    fn expect(&mut self, token: TokenKind) {
        // TODO: diagnostic
    }

    #[inline]
    fn bump(&mut self) {
        self.pos += 1;
    }
}
