use ustr::ustr;

use crate::{
    common::QualifiedName,
    db::Database,
    diagnostics::{Diagnostic, Label},
    parse::{
        ast::*,
        tokenize::{Token, TokenKind},
    },
    span::{Source, SourceId, Span, Spanned},
};

pub(crate) fn parse(
    db: &Database,
    source: &Source,
    tokens: Vec<Token>,
) -> Result<Module, Diagnostic> {
    let source_id = source.id();
    let name = QualifiedName::from_path(db.root_dir(), source.path()).unwrap();
    let is_main = source_id == db.main_source().id();

    let module = Parser::new(tokens).parse(source_id, name, is_main)?;

    Ok(module)
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
    fn parse(
        mut self,
        source_id: SourceId,
        name: QualifiedName,
        is_main: bool,
    ) -> ParseResult<Module> {
        let mut module = Module::new(source_id, name, is_main);

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
            self.expect(TokenKind::CloseParen)?;
            self.expect(TokenKind::Eq)?;

            let body = self.parse_fun_body()?;

            let span = name_span;

            Ok(Binding {
                kind: BindingKind::Fun(Fun {
                    name,
                    body: Box::new(body),
                    span,
                }),
                span,
            })
        } else {
            let token = self.try_token()?;

            Err(ParseError::UnexpectedToken {
                expected: "fn".to_string(),
                actual: token.kind,
                span: token.span,
            })
        }
    }

    fn parse_fun_body(&mut self) -> ParseResult<Ast> {
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
            }))
        } else {
            let token = self.try_token()?;

            Err(ParseError::UnexpectedToken {
                expected: "an expression".to_string(),
                actual: token.kind,
                span: token.span,
            })
        }
    }

    fn parse_ret(&mut self) -> ParseResult<Ast> {
        let start = self.last_span();
        let value = self.parse_expr()?;
        let span = start.merge(value.span());

        Ok(Ast::Ret(Ret {
            value: Some(Box::new(value)),
            span,
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

    fn expect_ident(&mut self) -> ParseResult<Token> {
        self.expect(TokenKind::Ident(ustr("")))
    }

    fn expect(&mut self, expected: TokenKind) -> ParseResult<Token> {
        let tok = self.try_token()?;

        if tok.kind_eq(expected) {
            self.advance();
            Ok(tok)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: expected.to_string(),
                actual: tok.kind,
                span: tok.span,
            })
        }
    }

    fn token_kind(&self) -> Option<TokenKind> {
        self.token().map(|t| t.kind)
    }

    fn try_token(&self) -> ParseResult<Token> {
        self.token().ok_or_else(|| ParseError::UnexpectedEof {
            span: self.last_span(),
        })
    }

    fn token(&self) -> Option<Token> {
        self.tokens.get(self.pos).copied()
    }

    fn last_span(&self) -> Span {
        self.tokens[self.pos - 1].span
    }

    #[inline]
    fn advance(&mut self) {
        self.pos += 1;
    }
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
enum ParseError {
    UnexpectedToken {
        expected: String,
        actual: TokenKind,
        span: Span,
    },
    UnexpectedEof {
        span: Span,
    },
}

impl From<ParseError> for Diagnostic {
    fn from(err: ParseError) -> Self {
        match err {
            ParseError::UnexpectedToken {
                expected,
                actual,
                span,
            } => Diagnostic::error("parse::unexpected_token")
                .with_message(format!("expected `{expected}`, got `{actual}` instead"))
                .with_label(Label::primary(span).with_message(format!("found `{actual}` here"))),
            ParseError::UnexpectedEof { span } => Diagnostic::error("parse::unexpected_eof")
                .with_message(format!("unexpected end of file"))
                .with_label(Label::primary(span).with_message(format!("here"))),
        }
    }
}
