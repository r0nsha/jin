use std::sync::Arc;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;
use ustr::ustr;

use crate::{
    ast::*,
    lexer::{Token, TokenKind},
    span::{Source, Span},
};

pub fn parse(source: Arc<Source>, tokens: Vec<Token>) -> Result<Module> {
    Parser::new(source, tokens).parse()
}

#[derive(Debug)]
struct Parser {
    source: Arc<Source>,
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(source: Arc<Source>, tokens: Vec<Token>) -> Self {
        Self {
            source,
            tokens,
            pos: 0,
        }
    }
}

impl Parser {
    fn parse(&mut self) -> Result<Module> {
        let mut module = Module { bindings: vec![] };

        while self.pos < self.tokens.len() - 1 {
            let binding = self.parse_binding().map_err(|err| ParseError {
                source_code: self.source.clone(),
                related: vec![err],
            })?;
            module.bindings.push(binding);
        }

        Ok(module)
    }

    fn parse_binding(&mut self) -> ParseResult<Binding> {
        if self.is(TokenKind::Fn) {
            let start = self.last_span();

            let name = self.expect(TokenKind::Ident(ustr("")))?.ident();
            self.expect(TokenKind::OpenParen)?;
            // TODO: args
            self.expect(TokenKind::CloseParen)?;
            self.expect(TokenKind::Eq)?;

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
            // TODO: diagnostic
            todo!()
        }
    }

    fn parse_fun_body(&mut self) -> ParseResult<Ast> {
        // TODO: don't require curlies (need to impl block expressions)
        self.expect(TokenKind::OpenCurly)?;
        let body = self.parse_expr();
        self.expect(TokenKind::CloseCurly)?;
        body
    }

    fn parse_expr(&mut self) -> ParseResult<Ast> {
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
            // TODO: diagnostic
            todo!()
        }
    }

    fn parse_ret(&mut self) -> ParseResult<Ast> {
        // TODO: naked return
        let start = self.last_span();
        let value = self.parse_expr()?;
        let span = start.merge(value.span());

        Ok(Ast::Ret(Ret {
            value: Box::new(Some(value)),
            span,
            ty: None,
        }))
    }
}

impl Parser {
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

    fn expect(&mut self, expected: TokenKind) -> ParseResult<Token> {
        match self.token() {
            Some(tok) if tok.kind == expected => {
                self.advance();
                Ok(tok)
            }
            Some(tok) => Err(InnerError::ExpectedToken {
                expected,
                actual: tok.kind,
                span: tok.span.into(),
            }),
            None => Err(InnerError::Eof {
                expected,
                span: self.last_span().into(),
            }),
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

pub type ParseResult<T> = std::result::Result<T, InnerError>;
pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Error, Diagnostic, Debug)]
#[error("parsing failed")]
#[diagnostic()]
pub struct ParseError {
    #[source_code]
    source_code: Arc<Source>,
    #[related]
    related: Vec<InnerError>,
}

#[derive(Diagnostic, Debug, Error)]
enum InnerError {
    #[error("expected token `{expected}`, but got `{actual}` instead")]
    #[diagnostic(code(parse::expected_token))]
    ExpectedToken {
        expected: TokenKind,
        actual: TokenKind,
        #[label("expected here")]
        span: SourceSpan,
    },

    #[error("expected token `{expected}`, but got to end the of the file instead")]
    #[diagnostic(code(parse::expected_token))]
    Eof {
        expected: TokenKind,
        #[label("expected here")]
        span: SourceSpan,
    },
}
