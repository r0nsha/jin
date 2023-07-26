use std::mem;

use ustr::ustr;

use crate::{
    ast::*,
    lexer::{Token, TokenKind},
    span::Span,
    CompilerResult,
};

pub fn parse(tokens: Vec<Token>) -> CompilerResult<Module> {
    Parser::new(tokens).parse()
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
    fn parse(&mut self) -> CompilerResult<Module> {
        let mut module = Module { bindings: vec![] };

        while self.pos < self.tokens.len() - 1 {
            let binding = self.parse_binding()?;
            module.bindings.push(binding);
        }

        Ok(module)
    }

    fn parse_binding(&mut self) -> CompilerResult<Binding> {
        if self.is(TokenKind::Fn) {
            let start = self.last_span();

            let name = self.expect_ident()?.as_ident();
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

    fn parse_fun_body(&mut self) -> CompilerResult<Ast> {
        // TODO: don't require curlies (need to impl block expressions)
        self.expect(TokenKind::OpenCurly)?;
        let body = self.parse_expr();
        self.expect(TokenKind::CloseCurly)?;
        body
    }

    fn parse_expr(&mut self) -> CompilerResult<Ast> {
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

    fn parse_ret(&mut self) -> CompilerResult<Ast> {
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

    fn expect_ident(&mut self) -> CompilerResult<Token> {
        self.expect(TokenKind::Ident(ustr("")))
    }

    fn expect(&mut self, expected: TokenKind) -> CompilerResult<Token> {
        match self.token() {
            Some(tok) if tok.kind_eq(expected) => {
                self.advance();
                Ok(tok)
            }
            Some(tok) => Err(diagnostics::expected_token(expected, tok.kind, tok.span)),
            None => Err(diagnostics::expected_token_eof(expected, self.last_span())),
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

mod diagnostics {
    use ariadne::{Label, ReportKind};

    use crate::{
        diagnostics::{create_report, CompilerReport},
        lexer::TokenKind,
        span::Span,
    };

    pub fn expected_token(expected: TokenKind, actual: TokenKind, span: Span) -> CompilerReport {
        create_report(ReportKind::Error, &span)
            .with_message(format!("expected `{expected}`, got `{actual}` instead"))
            .with_label(Label::new(span).with_message("expected here"))
            .finish()
    }

    pub fn expected_token_eof(expected: TokenKind, span: Span) -> CompilerReport {
        create_report(ReportKind::Error, &span)
            .with_message(format!("expected `{expected}`, got end of file instead"))
            .finish()
    }
}
