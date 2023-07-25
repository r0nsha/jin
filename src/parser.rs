use miette::{miette, Diagnostic, SourceSpan};
use thiserror::Error;

use crate::{
    ast::*,
    lexer::{Token, TokenKind},
    span::{Source, Span},
};

pub fn parse(source: &Source, tokens: Vec<Token>) -> Result<Module> {
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
}

impl<'a> Parser<'a> {
    fn parse(&mut self) -> Result<Module> {
        let mut module = Module { bindings: vec![] };

        while self.pos < self.tokens.len() - 1 {
            let binding = self.parse_binding()?; // TODO: diagnostic
            module.bindings.push(binding);
        }

        Ok(module)
    }

    fn parse_binding(&mut self) -> Result<Binding> {
        if self.is(TokenKind::Fn) {
            let start = self.last_span();

            let name = self.expect_ident().unwrap().ident(); // TODO: diagnostic
            self.expect(TokenKind::OpenParen).unwrap(); // TODO: diagnostic
                                                        // TODO: args
            self.expect(TokenKind::CloseParen).unwrap(); // TODO: diagnostic
            self.expect(TokenKind::Eq).unwrap(); // TODO: diagnostic

            let body = self.parse_fun_body()?;

            let span = start.merge(body.span());

            Ok(Binding {
                kind: BindingKind::Fun {
                    name,
                    fun: Box::new(Fun {
                        body: Box::new(body),
                        span,
                        ty: None,
                    }),
                },
                span,
                ty: None,
            })
        } else {
            todo!() // TODO: diagnostic
        }
    }

    fn parse_fun_body(&mut self) -> Result<Ast> {
        // TODO: don't require curlies (need to impl block expressions)
        self.expect(TokenKind::OpenCurly).unwrap(); // TODO: diagnostic
        let body = self.parse_expr(); // TODO: diagnostic
        self.expect(TokenKind::CloseCurly).unwrap(); // TODO: diagnostic
        body
    }

    fn parse_expr(&mut self) -> Result<Ast> {
        if self.is(TokenKind::Return) {
            self.parse_ret()
        } else if let Some(TokenKind::Int(value)) = self.token_kind() {
            self.advance();

            Ok(Ast::Lit(Lit {
                kind: LitKind::Int(value),
                span: self.last_span(),
                ty: None,
            }))
        } else {
            todo!() // TODO: diagnostic
        }
    }

    fn parse_ret(&mut self) -> Result<Ast> {
        // TODO: naked return
        let start = self.last_span();
        let value = self.parse_expr();
        let span = start.merge(value.span());

        Ok(Ast::Ret(Ret {
            value: Box::new(Some(value)),
            span,
            ty: None,
        }))
    }
}

impl<'a> Parser<'a> {
    fn is(&mut self, token: TokenKind) -> bool {
        match self.token() {
            Some(tok) if tok.kind == token => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    // TODO: is_any

    fn require(&mut self) -> Result<Token> {
        // TODO: diagnostic
        self.token().ok_or_else(|| miette!("oish"))
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        self.expect_pred(|tok| tok.kind == kind, kind)
    }

    fn expect_ident(&mut self) -> Result<Token> {
        self.expect_pred(Token::is_ident, TokenKind::Ident(""))
    }

    fn expect_pred(
        &mut self,
        pred: impl FnOnce(&Token) -> bool,
        on_err: impl FnOnce() -> TokenKind,
    ) -> Result<Token> {
        let tok = self.require()?;

        if pred(&tok) {
            self.advance();
            Ok(tok)
        } else {
            Err(ParseError::ExpectedToken {
                expected: (),
                actual: (),
                span: (),
            })
        }
    }

    fn token_kind(&self) -> Option<TokenKind> {
        self.token().map(|t| t.kind)
    }

    fn token(&self) -> Option<Token> {
        self.tokens.get(self.pos).cloned()
    }

    fn last_span(&self) -> Span {
        self.tokens[self.pos - 1].span
    }

    #[inline]
    fn advance(&mut self) {
        self.pos += 1;
    }
}

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Error, Diagnostic, Debug)]
pub enum ParseError {
    #[diagnostic(code(parse::expected_token))]
    ExpectedToken {
        expected: TokenKind,
        actual: Option<TokenKind>,
        #[label("expected here")]
        span: SourceSpan,
    },
}
