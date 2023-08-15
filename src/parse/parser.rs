use codespan_reporting::files::Files;

use crate::ast::{Binary, BinaryOp};
use crate::{
    ast::{
        token::{Token, TokenKind},
        Ast, Block, Call, Function, FunctionParam, Lit, LitKind, Module, Name, Return, Statement,
        TopLevel,
    },
    common::QualifiedName,
    db::Database,
    diagnostics::{Diagnostic, Label},
    span::{Source, SourceId, Span, Spanned},
};

pub fn parse(db: &Database, source: &Source, tokens: Vec<Token>) -> Result<Module, Diagnostic> {
    let source_id = source.id();
    let name = QualifiedName::from_path(db.root_dir(), source.path()).unwrap();
    let is_main = source_id == db.main_source().id();

    let module = Parser::new(db.sources.get(source_id).expect("to exist"), tokens)
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

        while !self.eof() {
            module.top_levels.push(self.parse_top_level()?);
        }

        Ok(module)
    }

    fn parse_top_level(&mut self) -> ParseResult<TopLevel> {
        if self.is(TokenKind::Fn) {
            let name_ident = self.eat(TokenKind::empty_ident())?;
            let params = self.parse_function_params()?;

            self.eat(TokenKind::Eq)?;

            let body = self.parse_expr()?;

            Ok(TopLevel::Function(Function {
                name: name_ident.as_ident(),
                body: Box::new(body),
                params,
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

    fn parse_function_params(&mut self) -> ParseResult<Vec<FunctionParam>> {
        self.parse_list(TokenKind::OpenParen, TokenKind::CloseParen, |_, tok| {
            Self::require_kind(tok, TokenKind::empty_ident())
                .map(|tok| FunctionParam { name: tok.as_ident(), span: tok.span })
        })
        .map(|(params, _)| params)
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
            Ok(Statement::Return(self.parse_return()?))
        } else {
            Ok(Statement::Expr(self.parse_expr()?))
        }
    }

    fn parse_expr(&mut self) -> ParseResult<Ast> {
        let expr = self.parse_operand()?;

        if self.eof() {
            Ok(expr)
        } else {
            self.parse_call(expr)
        }
    }

    fn parse_call(&mut self, expr: Ast) -> ParseResult<Ast> {
        if self.is_and_same_line(TokenKind::OpenParen, expr.span()) {
            let close_paren = self.eat(TokenKind::CloseParen)?;
            let span = expr.span().merge(close_paren.span);
            Ok(Ast::Call(Call { callee: Box::new(expr), span }))
        } else {
            self.parse_binary_factor(expr)
        }
    }

    fn parse_binary_factor(&mut self, left: Ast) -> ParseResult<Ast> {
        let tok = self.require()?;

        match tok.kind {
            TokenKind::Star | TokenKind::FwSlash | TokenKind::Percent => {
                self.parse_binary(left, tok)
            }
            _ => self.parse_binary_term(left),
        }
    }

    fn parse_binary_term(&mut self, left: Ast) -> ParseResult<Ast> {
        let tok = self.require()?;

        match tok.kind {
            TokenKind::Plus | TokenKind::Minus => self.parse_binary(left, tok),
            _ => self.parse_binary_bitshift(left),
        }
    }

    fn parse_binary_bitshift(&mut self, left: Ast) -> ParseResult<Ast> {
        let tok = self.require()?;

        match tok.kind {
            TokenKind::LtLt | TokenKind::GtGt => self.parse_binary(left, tok),
            _ => self.parse_binary_bitand(left),
        }
    }

    fn parse_binary_bitand(&mut self, left: Ast) -> ParseResult<Ast> {
        let tok = self.require()?;

        match tok.kind {
            TokenKind::Amp => self.parse_binary(left, tok),
            _ => self.parse_binary_bitxor(left),
        }
    }

    fn parse_binary_bitxor(&mut self, left: Ast) -> ParseResult<Ast> {
        let tok = self.require()?;

        match tok.kind {
            TokenKind::Caret => self.parse_binary(left, tok),
            _ => self.parse_binary_bitor(left),
        }
    }

    fn parse_binary_bitor(&mut self, left: Ast) -> ParseResult<Ast> {
        let tok = self.require()?;

        match tok.kind {
            TokenKind::Pipe => self.parse_binary(left, tok),
            _ => self.parse_cmp_eq(left),
        }
    }

    fn parse_cmp_eq(&mut self, left: Ast) -> ParseResult<Ast> {
        let tok = self.require()?;

        match tok.kind {
            TokenKind::EqEq | TokenKind::BangEq => self.parse_binary(left, tok),
            _ => self.parse_cmp_ord(left),
        }
    }

    fn parse_cmp_ord(&mut self, left: Ast) -> ParseResult<Ast> {
        let tok = self.require()?;

        match tok.kind {
            TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq => {
                self.parse_binary(left, tok)
            }
            _ => self.parse_and(left),
        }
    }

    fn parse_and(&mut self, left: Ast) -> ParseResult<Ast> {
        let tok = self.require()?;

        match tok.kind {
            TokenKind::AmpAmp => self.parse_binary(left, tok),
            _ => self.parse_or(left),
        }
    }

    fn parse_or(&mut self, left: Ast) -> ParseResult<Ast> {
        let tok = self.require()?;

        match tok.kind {
            TokenKind::PipePipe => self.parse_binary(left, tok),
            _ => self.parse_operand(),
        }
    }

    fn parse_binary(&mut self, left: Ast, tok: Token) -> ParseResult<Ast> {
        let right = self.parse_expr()?;
        let span = left.span().merge(right.span());

        Ok(Ast::Binary(Binary {
            left: Box::new(left),
            right: Box::new(right),
            op: BinaryOp::try_from(tok.kind).expect("to be a binary op"),
            span,
        }))
    }

    fn parse_return(&mut self) -> ParseResult<Return> {
        let start = self.last_span();
        let expr = self.parse_expr()?;
        let span = start.merge(expr.span());

        Ok(Return { expr: Some(Box::new(expr)), span })
    }

    fn parse_operand(&mut self) -> ParseResult<Ast> {
        let tok = self.eat_any()?;

        match tok.kind {
            TokenKind::OpenParen => {
                if self.is(TokenKind::CloseParen) {
                    Ok(Ast::Lit(Lit {
                        kind: LitKind::Unit,
                        span: tok.span.merge(self.last_span()),
                    }))
                } else {
                    let expr = self.parse_expr()?;
                    let end = self.eat(TokenKind::CloseParen)?.span;
                    Ok(expr.with_span(tok.span.merge(end)))
                }
            }
            TokenKind::OpenCurly => {
                let blk = self.parse_block()?;
                Ok(Ast::Block(blk))
            }
            TokenKind::Ident(ident) => Ok(Ast::Name(Name { name: ident, span: tok.span })),
            TokenKind::Int(value) => {
                Ok(Ast::Lit(Lit { kind: LitKind::Int(value), span: tok.span }))
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "an expression".to_string(),
                actual: tok.kind,
                span: tok.span,
            }),
        }
    }

    fn parse_list<T>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        mut f: impl FnMut(&mut Self, Token) -> Result<T, ParseError>,
    ) -> ParseResult<(Vec<T>, Span)> {
        let mut values = Vec::new();
        let open_tok = self.eat(open)?;

        loop {
            let tok = self.eat_any()?;

            if tok.kind_is(close) {
                return Ok((values, open_tok.span.merge(tok.span)));
            }

            values.push(f(self, tok)?);

            if !values.is_empty() && !self.peek_is(close) {
                self.eat(TokenKind::Comma)?;
            } else if self.peek_is(TokenKind::Comma) {
                self.next();
            }
        }
    }
}

impl<'a> Parser<'a> {
    fn eat(&mut self, expected: TokenKind) -> ParseResult<Token> {
        let tok = self.eat_any()?;
        Self::require_kind(tok, expected)
    }

    fn eat_any(&mut self) -> ParseResult<Token> {
        let tok = self.require()?;
        self.next();
        Ok(tok)
    }

    fn require(&mut self) -> ParseResult<Token> {
        self.token().ok_or_else(|| ParseError::UnexpectedEof { span: self.last_span() })
    }

    fn is(&mut self, expected: TokenKind) -> bool {
        self.is_predicate(|_, tok| tok.kind_is(expected))
    }

    fn is_and_same_line(&mut self, expected: TokenKind, span: Span) -> bool {
        self.is_predicate(|this, tok| {
            tok.kind_is(expected) && this.are_on_same_line(tok.span, span)
        })
    }

    fn is_predicate(&mut self, mut f: impl FnMut(&mut Self, Token) -> bool) -> bool {
        match self.token() {
            Some(tok) if f(self, tok) => {
                self.next();
                true
            }
            _ => false,
        }
    }

    fn are_on_same_line(&self, s1: Span, s2: Span) -> bool {
        let l1 = self.source.line_index(self.source.id(), s1.end() as usize).unwrap();
        let l2 = self.source.line_index(self.source.id(), s2.start() as usize).unwrap();

        l1 == l2
    }

    fn token(&self) -> Option<Token> {
        self.tokens.get(self.pos).copied()
    }

    fn peek<R: Default>(&self, f: impl FnOnce(Token) -> R) -> R {
        self.token().map(f).unwrap_or_default()
    }

    fn peek_is(&self, expected: TokenKind) -> bool {
        self.peek(|t| t.kind_is(expected))
    }

    fn last_span(&self) -> Span {
        self.tokens[self.pos - 1].span
    }

    #[inline]
    fn next(&mut self) {
        self.pos += 1;
    }

    #[inline]
    fn eof(&self) -> bool {
        self.pos == self.tokens.len() - 1
    }

    fn require_kind(tok: Token, expected: TokenKind) -> ParseResult<Token> {
        if tok.kind_is(expected) {
            Ok(tok)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: expected.to_string(),
                actual: tok.kind,
                span: tok.span,
            })
        }
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
                Self::error("parse::unexpected_token")
                    .with_message(format!("expected {expected}, got {actual} instead"))
                    .with_label(Label::primary(span).with_message("found here"))
            }
            ParseError::UnexpectedEof { span } => Self::error("parse::unexpected_eof")
                .with_message("unexpected end of file")
                .with_label(Label::primary(span).with_message("here")),
        }
    }
}
