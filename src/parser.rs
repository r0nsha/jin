use miette::Diagnostic;
use thiserror::Error;
use ustr::ustr;

use crate::{
    ast::*,
    span::{SourceId, Span, Spanned},
    state::State,
    tokenize::{Token, TokenKind},
    util::ErrExt,
    CompilerResult,
};

pub fn parse(state: &State, source_id: SourceId, tokens: Vec<Token>) -> CompilerResult<Module> {
    let source = state.source_cache.get(source_id).unwrap();
    let name = QualifiedName::from_path(state.root_dir(), source.path()).unwrap();

    Parser::new(tokens)
        .parse(source_id, name)
        .map_err(|err| err.with_source_code(state))
}

#[derive(Debug)]
struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }
}

impl Parser {
    fn parse(mut self, source_id: SourceId, name: QualifiedName) -> ParseResult<Module> {
        let mut module = Module::new(source_id, name);

        while self.pos < self.tokens.len() - 1 {
            let binding = self.parse_binding()?;
            module.bindings.push(binding);
        }

        Ok(module)
    }

    fn parse_binding(&mut self) -> ParseResult<Binding> {
        if self.is(TokenKind::Fn) {
            let name_ident = self.expect_ident()?;
            let name_span = name_ident.span;
            let name = name_ident.as_ident();

            self.expect(TokenKind::OpenParen)?;
            // TODO: args
            self.expect(TokenKind::CloseParen)?;
            self.expect(TokenKind::Eq)?;

            let body = self.parse_fun_body()?;

            let span = name_span;

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
    fn is(&mut self, expected: TokenKind) -> bool {
        match self.token() {
            Some(tok) if tok.kind_eq(expected) => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    // TODO: is_any

    fn expect_ident(&mut self) -> ParseResult<Token> {
        self.expect(TokenKind::Ident(ustr("")))
    }

    fn expect(&mut self, expected: TokenKind) -> ParseResult<Token> {
        match self.token() {
            Some(tok) if tok.kind_eq(expected) => {
                self.advance();
                Ok(tok)
            }
            Some(tok) => Err(ParseError::ExpectedToken {
                expected,
                actual: tok.kind,
                span: tok.span,
            }),
            None => Err(ParseError::UnexpectedEof {
                expected,
                span: self.last_span(),
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

type ParseResult<T> = CompilerResult<T, ParseError>;

#[derive(Error, Diagnostic, Debug)]
enum ParseError {
    #[error("expected `{expected}`, got `{actual}` instead")]
    #[diagnostic(code(parse::expected_token))]
    ExpectedToken {
        expected: TokenKind,
        actual: TokenKind,
        #[label("found `{actual}` here")]
        span: Span,
    },
    #[error("expected `{expected}`, got end of file instead")]
    #[diagnostic(code(parse::unexpected_eof))]
    UnexpectedEof {
        expected: TokenKind,
        #[label("expected here")]
        span: Span,
    },
}

impl Spanned for ParseError {
    fn span(&self) -> Span {
        match self {
            ParseError::ExpectedToken { span, .. } => *span,
            ParseError::UnexpectedEof { span, .. } => *span,
        }
    }
}
