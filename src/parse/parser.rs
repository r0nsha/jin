use codespan_reporting::files::Files;
use ustr::ustr;

use crate::{
    common::QualifiedName,
    db::Database,
    diagnostics::{Diagnostic, Label},
    parse::{
        ast::*,
        token::{Token, TokenKind},
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

    let module =
        Parser::new(db.sources.get(source_id).expect("to exist"), tokens)
            .parse(source_id, name, is_main)?;

    Ok(module)
}

#[derive(Debug)]
struct Parser<'a> {
    source: &'a Source,
    tokens: Vec<Token>,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(source: &'a Source, tokens: Vec<Token>) -> Self {
        Self { source, tokens, pos: 0 }
    }
}

impl<'a> Parser<'a> {
    fn parse(
        mut self,
        source_id: SourceId,
        name: QualifiedName,
        is_main: bool,
    ) -> ParseResult<Module> {
        let mut module = Module::new(source_id, name, is_main);

        while self.pos < self.tokens.len() - 1 {
            module.top_levels.push(self.parse_top_level()?);
        }

        Ok(module)
    }

    fn parse_top_level(&mut self) -> ParseResult<TopLevel> {
        if self.is(TokenKind::Fn) {
            let name_ident = self.eat_ident()?;
            let name_span = name_ident.span;
            let name = name_ident.as_ident();

            let mut params = vec![];

            self.eat(TokenKind::OpenParen)?;

            while !self.is(TokenKind::CloseParen) {
                let name_ident = self.eat_ident()?;

                params.push(FunctionParam {
                    name: name_ident.as_ident(),
                    span: name_ident.span,
                })
            }

            self.eat(TokenKind::Eq)?;

            let body = self.parse_expr()?;

            Ok(TopLevel::Function(Function {
                name: name_ident.as_ident(),
                body: Box::new(body),
                params: vec![],
                span: name_ident.span,
            }))
        } else {
            let token = self.require()?;

            Err(ParseError::UnexpectedToken {
                expected: "fn".to_string(),
                actual: token.kind,
                span: token.span,
            })
        }
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        let start = self.last_span();
        let mut stmts = vec![];

        loop {
            if self.is(TokenKind::CloseCurly) {
                let span = start.merge(self.last_span());
                return Ok(Block { stmts, span });
            }

            stmts.push(self.parse_stmt()?);
        }
    }

    fn parse_stmt(&mut self) -> ParseResult<Statement> {
        if self.is(TokenKind::Return) {
            Ok(Statement::Return(self.parse_ret()?))
        } else {
            Ok(Statement::Expr(self.parse_expr()?))
        }
    }

    fn parse_expr(&mut self) -> ParseResult<Ast> {
        let token = self.require()?;

        let expr = match token.kind {
            TokenKind::OpenParen => {
                let end = self.eat(TokenKind::CloseParen)?.span;

                Ast::Lit(Lit {
                    kind: LitKind::Unit,
                    span: token.span.merge(end),
                })
            }
            TokenKind::OpenCurly => {
                let blk = self.parse_block()?;
                Ast::Block(blk)
            }
            TokenKind::Ident(ident) => {
                Ast::Name(Name { name: ident, span: token.span })
            }
            TokenKind::Int(value) => {
                Ast::Lit(Lit { kind: LitKind::Int(value), span: token.span })
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "an expression".to_string(),
                    actual: token.kind,
                    span: token.span,
                })
            }
        };

        self.parse_postfix(expr)
    }

    fn parse_postfix(&mut self, expr: Ast) -> ParseResult<Ast> {
        match self.token() {
            Some(tok) if self.are_on_same_line(expr.span(), tok.span) => {
                match &tok.kind {
                    TokenKind::OpenParen => {
                        self.advance();
                        let call = self.parse_call(expr)?;
                        Ok(Ast::Call(call))
                    }
                    _ => Ok(expr),
                }
            }
            _ => Ok(expr),
        }
    }

    fn parse_ret(&mut self) -> ParseResult<Return> {
        let start = self.last_span();
        let expr = self.parse_expr()?;
        let span = start.merge(expr.span());

        Ok(Return { expr: Some(Box::new(expr)), span })
    }

    fn parse_call(&mut self, callee: Ast) -> ParseResult<Call> {
        let close_paren = self.eat(TokenKind::CloseParen)?;

        let span = callee.span().merge(close_paren.span);

        Ok(Call { callee: Box::new(callee), span })
    }
}

impl<'a> Parser<'a> {
    fn are_on_same_line(&self, s1: Span, s2: Span) -> bool {
        let l1 = self
            .source
            .line_index(self.source.id(), s1.end() as usize)
            .unwrap();
        let l2 = self
            .source
            .line_index(self.source.id(), s2.start() as usize)
            .unwrap();

        l1 == l2
    }

    fn eat_ident(&mut self) -> ParseResult<Token> {
        self.eat(TokenKind::Ident(ustr("")))
    }

    fn eat(&mut self, expected: TokenKind) -> ParseResult<Token> {
        let tok = self.require()?;

        if tok.kind_eq(expected) {
            Ok(tok)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: expected.to_string(),
                actual: tok.kind,
                span: tok.span,
            })
        }
    }

    fn require(&mut self) -> ParseResult<Token> {
        if let Some(tok) = self.token() {
            self.advance();
            Ok(tok)
        } else {
            Err(ParseError::UnexpectedEof { span: self.last_span() })
        }
    }

    fn is(&mut self, expected: TokenKind) -> bool {
        match self.token() {
            Some(tok) if tok.kind_eq(expected) => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    #[allow(unused)]
    fn token_kind(&self) -> Option<TokenKind> {
        self.token().map(|t| t.kind)
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
    UnexpectedToken { expected: String, actual: TokenKind, span: Span },
    UnexpectedEof { span: Span },
}

impl From<ParseError> for Diagnostic {
    fn from(err: ParseError) -> Self {
        match err {
            ParseError::UnexpectedToken { expected, actual, span } => {
                Diagnostic::error("parse::unexpected_token")
                    .with_message(format!(
                        "expected {expected}, got {actual} instead"
                    ))
                    .with_label(
                        Label::primary(span)
                            .with_message(format!("found {actual} here")),
                    )
            }
            ParseError::UnexpectedEof { span } => {
                Diagnostic::error("parse::unexpected_eof")
                    .with_message("unexpected end of file")
                    .with_label(Label::primary(span).with_message("here"))
            }
        }
    }
}
