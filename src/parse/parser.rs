use codespan_reporting::files::Files;

use crate::{
    ast::{
        token::{Token, TokenKind},
        Attr, AttrKind, Attrs, BinOp, CallArg, Expr, ExternLet, Fn, FnKind, FnParam, FnSig, Item,
        Let, LitKind, Module, NamePat, Pat, TyExpr, TyName, TyParam, UnOp,
    },
    common::{QPath, Word},
    db::Db,
    diagnostics::{Diagnostic, Label},
    span::{Source, SourceId, Span, Spanned},
};

pub fn parse(db: &Db, source: &Source, tokens: Vec<Token>) -> Result<Module, Diagnostic> {
    let name = QPath::from_path(db.root_dir(), source.path()).unwrap();
    let is_main = source.id() == db.main_source().id();
    let module = Parser::new(source, tokens).parse(source.id(), name, is_main)?;
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
    fn parse(mut self, source_id: SourceId, name: QPath, is_main: bool) -> ParseResult<Module> {
        let mut module = Module::new(source_id, name, is_main);

        while !self.eof() {
            module.items.push(self.parse_top_level()?);
        }

        Ok(module)
    }

    fn parse_top_level(&mut self) -> ParseResult<Item> {
        if let Some(item) = self.maybe_parse_item()? {
            Ok(item)
        } else {
            let token = self.require()?;
            Err(ParseError::UnexpectedToken {
                expected: "`fn` or `let`".to_string(),
                found: token.kind,
                span: token.span,
            })
        }
    }

    fn maybe_parse_item(&mut self) -> ParseResult<Option<Item>> {
        let attrs = self.parse_attrs()?;

        if self.is(TokenKind::Fn) {
            self.parse_fn(attrs).map(|f| Some(Item::Fn(f)))
        } else if self.is(TokenKind::Let) {
            if self.is(TokenKind::Extern) {
                self.parse_extern_let(attrs).map(|l| Some(Item::ExternLet(l)))
            } else {
                self.parse_let(attrs).map(|l| Some(Item::Let(l)))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_attrs(&mut self) -> ParseResult<Vec<Attr>> {
        let mut attrs = vec![];

        while self.is(TokenKind::At) {
            attrs.push(self.parse_attr()?);
        }

        Ok(attrs)
    }

    fn parse_attr(&mut self) -> ParseResult<Attr> {
        let (kind, span) = self.parse_attr_kind()?;

        let value = if self.is(TokenKind::OpenParen) {
            let value = self.parse_expr()?;
            self.eat(TokenKind::CloseParen)?;
            Some(value)
        } else {
            None
        };

        Ok(Attr { kind, value, span })
    }

    fn parse_attr_kind(&mut self) -> ParseResult<(AttrKind, Span)> {
        let ident = self.eat(TokenKind::empty_ident())?;
        let kind = AttrKind::try_from(ident.ident().as_str())
            .map_err(|()| ParseError::InvalidAttr(ident.word()))?;
        Ok((kind, ident.span))
    }

    fn parse_fn(&mut self, attrs: Attrs) -> ParseResult<Fn> {
        if self.is(TokenKind::Extern) {
            let name_ident = self.eat(TokenKind::empty_ident())?;
            let sig = self.parse_function_sig(name_ident.word())?;

            Ok(Fn { attrs, sig, kind: FnKind::Extern, span: name_ident.span })
        } else {
            let name_ident = self.eat(TokenKind::empty_ident())?;
            let sig = self.parse_function_sig(name_ident.word())?;

            self.eat(TokenKind::Eq)?;

            let expr = self.parse_expr()?;

            let body = match expr {
                Expr::Block { .. } => expr,
                _ => Expr::Block { span: expr.span(), exprs: vec![expr] },
            };

            Ok(Fn {
                attrs,
                sig,
                kind: FnKind::Bare { body: Box::new(body) },
                span: name_ident.span,
            })
        }
    }

    fn parse_let(&mut self, attrs: Attrs) -> ParseResult<Let> {
        let start = self.last_span();
        let pat = self.parse_pat()?;

        let ty_annot = if self.peek_is(TokenKind::Eq) { None } else { Some(self.parse_ty()?) };

        self.eat(TokenKind::Eq)?;

        let value = self.parse_expr()?;

        Ok(Let { attrs, pat, ty_annot, span: start.merge(value.span()), value: Box::new(value) })
    }

    fn parse_extern_let(&mut self, attrs: Attrs) -> ParseResult<ExternLet> {
        let start = self.last_span();
        let ident = self.eat(TokenKind::empty_ident())?;
        let ty_annot = self.parse_ty()?;
        let span = start.merge(ty_annot.span());
        Ok(ExternLet { attrs, word: ident.word(), ty_annot, span })
    }
    fn parse_pat(&mut self) -> ParseResult<Pat> {
        let tok = self.eat_any()?;

        match tok.kind {
            TokenKind::Ident(_) => Ok(Pat::Name(NamePat { word: tok.word() })),
            TokenKind::Underscore => Ok(Pat::Discard(tok.span)),
            _ => Err(ParseError::UnexpectedToken {
                expected: "a pattern".to_string(),
                found: tok.kind,
                span: tok.span,
            }),
        }
    }

    fn parse_function_sig(&mut self, name: Word) -> ParseResult<FnSig> {
        let ty_params = self.parse_optional_ty_params()?;
        let (params, _) = self.parse_function_params()?;

        let ret = match self.token() {
            Some(Token { kind: TokenKind::Eq | TokenKind::OpenCurly, .. }) => None,
            Some(tok) if !self.spans_are_on_same_line(tok.span, self.last_span()) => None,
            _ => Some(self.parse_ty()?),
        };

        Ok(FnSig { word: name, ty_params, params, ret })
    }

    fn parse_optional_ty_params(&mut self) -> ParseResult<Vec<TyParam>> {
        if self.peek_is(TokenKind::OpenBracket) {
            self.parse_ty_params().map(|(t, _)| t)
        } else {
            Ok(vec![])
        }
    }

    fn parse_ty_params(&mut self) -> ParseResult<(Vec<TyParam>, Span)> {
        self.parse_list(TokenKind::OpenBracket, TokenKind::CloseBracket, |this| {
            let ident = this.eat(TokenKind::empty_ident())?;
            Ok(TyParam { name: ident.word() })
        })
    }

    fn parse_optional_ty_args(&mut self) -> ParseResult<Option<Vec<TyExpr>>> {
        if self.peek_is(TokenKind::OpenBracket) {
            let args = self.parse_ty_args().map(|(t, _)| t)?;
            Ok(Some(args))
        } else {
            Ok(None)
        }
    }

    fn parse_ty_args(&mut self) -> ParseResult<(Vec<TyExpr>, Span)> {
        self.parse_list(TokenKind::OpenBracket, TokenKind::CloseBracket, Self::parse_ty)
    }

    fn parse_function_params(&mut self) -> ParseResult<(Vec<FnParam>, Span)> {
        self.parse_list(TokenKind::OpenParen, TokenKind::CloseParen, |this| {
            let ident = this.eat(TokenKind::empty_ident())?;
            let ty = this.parse_ty()?;
            Ok(FnParam { name: ident.word(), ty_annot: ty, span: ident.span })
        })
    }

    fn parse_block(&mut self) -> ParseResult<Expr> {
        let start = self.last_span();
        let mut stmts = vec![];

        loop {
            if self.is(TokenKind::CloseCurly) {
                let span = start.merge(self.last_span());
                return Ok(Expr::Block { exprs: stmts, span });
            }

            stmts.push(self.parse_stmt()?);
        }
    }

    fn parse_stmt(&mut self) -> ParseResult<Expr> {
        if let Some(item) = self.maybe_parse_item()? {
            Ok(Expr::Item(item))
        } else {
            self.parse_expr()
        }
    }

    fn parse_return(&mut self) -> ParseResult<Expr> {
        let start = self.last_span();

        let expr = match self.token() {
            Some(tok) if self.spans_are_on_same_line(start, tok.span) => {
                Some(Box::new(self.parse_expr()?))
            }
            _ => None,
        };

        let span = expr.as_ref().map_or(start, |e| start.merge(e.span()));

        Ok(Expr::Return { expr, span })
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        let mut expr_stack: Vec<Expr> = vec![];
        let mut op_stack: Vec<BinOp> = vec![];
        let mut last_precedence = usize::MAX;

        expr_stack.push(self.parse_operand()?);

        while !self.eof() {
            let Some(tok) = self.token() else {
                break;
            };

            let op = match BinOp::try_from(tok.kind).ok() {
                // For these specific operators, we check if they are on the same line as the last
                // expr, to avoid ambiguity with unary operators
                Some(BinOp::BitAnd | BinOp::Sub)
                    if !self.spans_are_on_same_line(
                        expr_stack.last().expect("to have an expr").span(),
                        tok.span,
                    ) =>
                {
                    break;
                }
                None => break,
                Some(op) => {
                    self.next();
                    op
                }
            };

            let rhs = self.parse_operand()?;

            let precedence = op.precedence();

            while precedence <= last_precedence && expr_stack.len() > 1 {
                let rhs = expr_stack.pop().unwrap();
                let op = op_stack.pop().unwrap();

                last_precedence = op.precedence();

                if last_precedence < precedence {
                    expr_stack.push(rhs);
                    op_stack.push(op);
                    break;
                }

                let lhs = expr_stack.pop().unwrap();
                let span = lhs.span().merge(rhs.span());

                expr_stack.push(Expr::Binary { lhs: Box::new(lhs), rhs: Box::new(rhs), op, span });
            }

            op_stack.push(op);
            expr_stack.push(rhs);

            last_precedence = precedence;
        }

        while expr_stack.len() > 1 {
            let rhs = expr_stack.pop().unwrap();
            let op = op_stack.pop().unwrap();
            let lhs = expr_stack.pop().unwrap();

            let span = lhs.span().merge(rhs.span());

            expr_stack.push(Expr::Binary { lhs: Box::new(lhs), op, rhs: Box::new(rhs), span });
        }

        Ok(expr_stack.into_iter().next().unwrap())
    }

    fn parse_operand(&mut self) -> ParseResult<Expr> {
        let term = self.parse_term()?;
        self.parse_postfix(term)
    }

    fn parse_term(&mut self) -> ParseResult<Expr> {
        let tok = self.eat_any()?;

        let expr = match tok.kind {
            TokenKind::Return => self.parse_return()?,
            TokenKind::If => self.parse_if()?,
            TokenKind::Minus => {
                let expr = self.parse_operand()?;

                Expr::Unary {
                    span: tok.span.merge(expr.span()),
                    expr: Box::new(expr),
                    op: UnOp::Neg,
                }
            }
            TokenKind::Bang => {
                let expr = self.parse_operand()?;

                Expr::Unary {
                    span: tok.span.merge(expr.span()),
                    expr: Box::new(expr),
                    op: UnOp::Not,
                }
            }
            TokenKind::OpenCurly => self.parse_block()?,
            TokenKind::True => Expr::Lit { kind: LitKind::Bool(true), span: tok.span },
            TokenKind::False => Expr::Lit { kind: LitKind::Bool(false), span: tok.span },
            TokenKind::Ident(..) => {
                let args = self.parse_optional_ty_args()?;
                Expr::Name { word: tok.word(), args, span: tok.span }
            }
            TokenKind::Str(value) => Expr::Lit { kind: LitKind::Str(value), span: tok.span },
            TokenKind::Int(value) => Expr::Lit { kind: LitKind::Int(value), span: tok.span },
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "an expression".to_string(),
                    found: tok.kind,
                    span: tok.span,
                })
            }
        };

        Ok(expr)
    }

    fn parse_ty(&mut self) -> ParseResult<TyExpr> {
        let tok = self.eat_any()?;

        let ty = match tok.kind {
            TokenKind::Star => {
                let pointee = self.parse_ty()?;
                let span = tok.span.merge(pointee.span());
                TyExpr::RawPtr(Box::new(pointee), span)
            }
            TokenKind::Ident(..) => {
                TyExpr::Name(TyName { word: tok.word(), args: vec![], span: tok.span })
            }
            TokenKind::Underscore => TyExpr::Hole(tok.span),
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "a type".to_string(),
                    found: tok.kind,
                    span: tok.span,
                })
            }
        };

        Ok(ty)
    }

    fn parse_if(&mut self) -> ParseResult<Expr> {
        let start = self.last_span();
        let cond = self.parse_expr()?;

        self.eat(TokenKind::OpenCurly)?;
        let then = self.parse_block()?;

        let otherwise = if self.is(TokenKind::Else) {
            if self.is(TokenKind::OpenCurly) {
                Some(Box::new(self.parse_block()?))
            } else if self.is(TokenKind::If) {
                Some(Box::new(self.parse_if()?))
            } else {
                let tok = self.require()?;

                return Err(ParseError::UnexpectedToken {
                    expected: "{ or `if`".to_string(),
                    found: tok.kind,
                    span: tok.span,
                });
            }
        } else {
            None
        };

        let span = start.merge(otherwise.as_ref().map_or(then.span(), |o| o.span()));

        Ok(Expr::If { cond: Box::new(cond), then: Box::new(then), otherwise, span })
    }

    fn parse_postfix(&mut self, expr: Expr) -> ParseResult<Expr> {
        let start = self.last_span();

        match self.token() {
            Some(tok) => match tok.kind {
                TokenKind::OpenParen if self.spans_are_on_same_line(start, tok.span) => {
                    self.parse_call(expr)
                }
                TokenKind::As => {
                    self.next();
                    let ty = self.parse_ty()?;
                    let span = expr.span().merge(ty.span());
                    Ok(Expr::Cast { expr: Box::new(expr), ty, span })
                }
                TokenKind::Dot => {
                    self.next();
                    let name_ident = self.eat(TokenKind::empty_ident())?;
                    let span = expr.span().merge(name_ident.span);
                    Ok(Expr::Member { expr: Box::new(expr), member: name_ident.word(), span })
                }
                // TokenKind::Dot => {
                //     self.next();
                //     let tok = self.require()?;
                //
                //     if self.is(TokenKind::As) {
                //         self.eat(TokenKind::OpenParen)?;
                //         let ty = self.parse_ty()?;
                //         let end = self.eat(TokenKind::CloseParen)?.span;
                //         let span = expr.span().merge(end);
                //         Ok(Expr::Cast(Cast { expr: Box::new(expr), ty, span }))
                //     } else {
                //         Err(ParseError::UnexpectedToken {
                //             expected: "`as`".to_owned(),
                //             // expected: "an identifier or `as`".to_owned(),
                //             found: tok.kind,
                //             span: tok.span,
                //         })
                //     }
                // }
                _ => Ok(expr),
            },
            _ => Ok(expr),
        }
    }

    fn parse_call(&mut self, expr: Expr) -> ParseResult<Expr> {
        let mut passed_named_arg = false;

        let (args, args_span) =
            self.parse_list(TokenKind::OpenParen, TokenKind::CloseParen, |this| {
                let arg = Parser::parse_arg(this)?;

                match &arg {
                    CallArg::Positional(expr) if passed_named_arg => {
                        return Err(ParseError::MixedArgs(expr.span()));
                    }
                    CallArg::Positional(_) => (),
                    CallArg::Named(..) => passed_named_arg = true,
                }

                Ok(arg)
            })?;

        let span = expr.span().merge(args_span);

        Ok(Expr::Call { callee: Box::new(expr), args, span })
    }

    fn parse_arg(&mut self) -> ParseResult<CallArg> {
        if self.is(TokenKind::empty_ident()) {
            let ident_tok = self.last_token();

            if self.is(TokenKind::Colon) {
                let expr = self.parse_expr()?;
                return Ok(CallArg::Named(ident_tok.word(), expr));
            }

            self.prev();
        }

        let expr = self.parse_expr()?;
        Ok(CallArg::Positional(expr))
    }

    fn parse_list<T>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        mut f: impl FnMut(&mut Self) -> Result<T, ParseError>,
    ) -> ParseResult<(Vec<T>, Span)> {
        let mut values = Vec::new();
        let open_tok = self.eat(open)?;

        loop {
            if self.is(close) {
                return Ok((values, open_tok.span.merge(self.last_span())));
            }

            values.push(f(self)?);

            if !values.is_empty() && !self.peek_is(close) {
                self.eat(TokenKind::Comma)?;
            } else if self.peek_is(TokenKind::Comma) {
                self.next();
            }
        }
    }
}

impl<'a> Parser<'a> {
    #[inline]
    fn eat(&mut self, expected: TokenKind) -> ParseResult<Token> {
        let tok = self.eat_any()?;
        Self::require_kind(tok, expected)
    }

    #[inline]
    fn eat_any(&mut self) -> ParseResult<Token> {
        let tok = self.require()?;
        self.next();
        Ok(tok)
    }

    #[inline]
    fn require(&mut self) -> ParseResult<Token> {
        self.token().ok_or_else(|| ParseError::UnexpectedEof(self.last_span()))
    }

    #[inline]
    fn is(&mut self, expected: TokenKind) -> bool {
        self.is_predicate(|_, tok| tok.kind_is(expected))
    }

    #[allow(unused)]
    #[inline]
    fn is_same_line(&mut self, expected: TokenKind) -> bool {
        self.is_predicate(|p, tok| {
            tok.kind_is(expected) && p.spans_are_on_same_line(p.last_span(), tok.span)
        })
    }

    #[inline]
    fn is_predicate(&mut self, mut f: impl FnMut(&mut Self, Token) -> bool) -> bool {
        match self.token() {
            Some(tok) if f(self, tok) => {
                self.next();
                true
            }
            _ => false,
        }
    }

    #[inline]
    fn spans_are_on_same_line(&self, s1: Span, s2: Span) -> bool {
        fn line_index(parser: &Parser, pos: u32) -> usize {
            parser.source.line_index(parser.source.id(), pos as usize).unwrap()
        }

        line_index(self, s1.end()) == line_index(self, s2.start())
    }

    #[inline]
    fn token(&self) -> Option<Token> {
        self.tokens.get(self.pos).copied()
    }

    #[inline]
    fn peek<R: Default>(&self, f: impl FnOnce(Token) -> R) -> R {
        self.token().map(f).unwrap_or_default()
    }

    #[inline]
    fn peek_is(&self, expected: TokenKind) -> bool {
        self.peek(|t| t.kind_is(expected))
    }

    #[inline]
    fn last_span(&self) -> Span {
        self.last_token().span
    }

    #[inline]
    fn last_token(&self) -> Token {
        self.tokens[self.pos - 1]
    }

    #[inline]
    fn next(&mut self) {
        self.pos += 1;
    }

    #[inline]
    fn prev(&mut self) {
        self.pos -= 1;
    }

    #[inline]
    fn eof(&self) -> bool {
        self.pos == self.tokens.len()
    }

    #[inline]
    fn require_kind(tok: Token, expected: TokenKind) -> ParseResult<Token> {
        if tok.kind_is(expected) {
            Ok(tok)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: expected.to_string(),
                found: tok.kind,
                span: tok.span,
            })
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
enum ParseError {
    UnexpectedToken { expected: String, found: TokenKind, span: Span },
    UnexpectedEof(Span),
    MixedArgs(Span),
    InvalidAttr(Word),
}

impl From<ParseError> for Diagnostic {
    fn from(err: ParseError) -> Self {
        match err {
            ParseError::UnexpectedToken { expected, found, span } => {
                Self::error("parse::unexpected_token")
                    .with_message(format!("expected {expected}, found {found}"))
                    .with_label(Label::primary(span).with_message("found here"))
            }
            ParseError::UnexpectedEof(span) => Self::error("parse::unexpected_eof")
                .with_message("unexpected end of file")
                .with_label(Label::primary(span).with_message("here")),
            ParseError::MixedArgs(span) => Self::error("parse::mixed_args")
                .with_message("positional arguments are not allowed after named arguments")
                .with_label(Label::primary(span).with_message("unexpected positional argument")),
            ParseError::InvalidAttr(word) => Self::error("parse::invalid_attr")
                .with_message("unknown attribute {word}")
                .with_label(Label::primary(word.span()).with_message("unknown attribute")),
        }
    }
}
