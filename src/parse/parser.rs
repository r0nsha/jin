use camino::Utf8Path;
use codespan_reporting::files::Files;

use crate::{
    ast::{
        token::{Token, TokenKind},
        Attr, AttrKind, Attrs, CallArg, Expr, ExternImport, ExternLet, Fn, FnKind, FnParam, FnSig,
        Item, Let, LitKind, Module, NamePat, Pat, TyDef, TyDefKind, TyParam,
    },
    db::{Db, ExternLib},
    diagnostics::{Diagnostic, Label},
    macros::create_bool_enum,
    middle::{BinOp, TyExpr, TyExprFn, TyExprName, UnOp},
    qpath::QPath,
    span::{Source, SourceId, Span, Spanned},
    word::Word,
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
            Err(unexpected_token_err("an item", token.kind, token.span))
        }
    }

    fn maybe_parse_item(&mut self) -> ParseResult<Option<Item>> {
        let attrs = self.parse_attrs()?;

        if self.is(TokenKind::Fn) {
            return self.parse_fn(attrs).map(|f| Some(Item::Fn(f)));
        }

        if self.is(TokenKind::Let) {
            return if self.is(TokenKind::Extern) {
                self.parse_extern_let(attrs).map(|l| Some(Item::ExternLet(l)))
            } else {
                self.parse_let(attrs).map(|l| Some(Item::Let(l)))
            };
        }

        if self.is(TokenKind::Type) {
            return self.parse_ty_def(attrs).map(|t| Some(Item::Type(t)));
        }

        if self.is(TokenKind::Import) {
            let start = self.last_span();

            self.eat(TokenKind::Extern)?;

            let path_tok = self.eat(TokenKind::empty_str())?;
            let path = path_tok.str_value();

            let relative_to = self.parent_path().unwrap();
            let lib = ExternLib::try_from_str(&path, relative_to).ok_or_else(|| {
                Diagnostic::error("check::path_not_found")
                    .with_message(format!("path `{path}` not found"))
                    .with_label(Label::primary(path_tok.span).with_message("not found"))
            })?;

            return Ok(Some(Item::ExternImport(ExternImport {
                attrs,
                lib,
                span: start.merge(path_tok.span),
            })));
        }

        if !attrs.is_empty() {
            let token = self.require()?;
            return Err(unexpected_token_err("an item after attribute", token.kind, token.span));
        }

        Ok(None)
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
        let attr_name = ident.str_value().as_str();

        let kind = AttrKind::try_from(attr_name).map_err(|()| {
            Diagnostic::error("parse::invalid_attr")
                .with_message("unknown attribute {attr_name}")
                .with_label(Label::primary(ident.span).with_message("unknown attribute"))
        })?;

        Ok((kind, ident.span))
    }

    fn parse_fn(&mut self, attrs: Attrs) -> ParseResult<Fn> {
        if self.is(TokenKind::Extern) {
            let name_ident = self.eat(TokenKind::empty_ident())?;
            let (sig, is_c_variadic) = self.parse_fn_sig(name_ident.word(), AllowTyParams::No)?;

            Ok(Fn { attrs, sig, kind: FnKind::Extern { is_c_variadic }, span: name_ident.span })
        } else {
            let name_ident = self.eat(TokenKind::empty_ident())?;
            let (sig, is_c_variadic) = self.parse_fn_sig(name_ident.word(), AllowTyParams::Yes)?;

            if is_c_variadic {
                return Err(Diagnostic::error("parse::invaild_c_varargs")
                    .with_message("non extern function cannot use c varargs")
                    .with_label(Label::primary(name_ident.span).with_message("here")));
            }

            self.eat(TokenKind::OpenCurly)?;
            let body = self.parse_block()?;

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

        let ty_expr = self.is_and(TokenKind::Colon, |this, _| this.parse_ty()).transpose()?;
        self.eat(TokenKind::Eq)?;

        let value = self.parse_expr()?;

        Ok(Let { attrs, pat, ty_expr, span: start.merge(value.span()), value: Box::new(value) })
    }

    fn parse_extern_let(&mut self, attrs: Attrs) -> ParseResult<ExternLet> {
        let start = self.last_span();
        let ident = self.eat(TokenKind::empty_ident())?;
        self.eat(TokenKind::Colon)?;
        let ty_expr = self.parse_ty()?;
        let span = start.merge(ty_expr.span());
        Ok(ExternLet { attrs, word: ident.word(), ty_expr, span })
    }

    fn parse_ty_def(&mut self, attrs: Attrs) -> ParseResult<TyDef> {
        let start = self.last_span();
        let ident = self.eat(TokenKind::empty_ident())?;
        let kind = self.parse_ty_def_kind()?;
        let span = start.merge(self.last_span());
        Ok(TyDef { attrs, word: ident.word(), kind, span })
    }

    fn parse_ty_def_kind(&mut self) -> ParseResult<TyDefKind> {
        if self.is(TokenKind::Extern) {
            todo!("extern struct")
        } else if self.is(TokenKind::OpenParen) {
            todo!("regular struct")
        } else {
            let tok = self.require()?;
            Err(unexpected_token_err("( or `extern`", tok.kind, tok.span))
        }
    }

    fn parse_pat(&mut self) -> ParseResult<Pat> {
        let tok = self.eat_any()?;

        match tok.kind {
            TokenKind::Ident(_) => Ok(Pat::Name(NamePat { word: tok.word() })),
            TokenKind::Underscore => Ok(Pat::Discard(tok.span)),
            _ => Err(unexpected_token_err("a pattern", tok.kind, tok.span)),
        }
    }

    fn parse_fn_sig(
        &mut self,
        name: Word,
        allow_ty_params: AllowTyParams,
    ) -> ParseResult<(FnSig, bool)> {
        let ty_params = if allow_ty_params == AllowTyParams::Yes {
            self.parse_optional_ty_params()?
        } else {
            vec![]
        };

        let (params, is_c_variadic) = self.parse_fn_params()?;
        let ret = self.is_and(TokenKind::Arrow, |this, _| this.parse_ty()).transpose()?;

        Ok((FnSig { word: name, ty_params, params, ret }, is_c_variadic))
    }

    fn parse_optional_ty_params(&mut self) -> ParseResult<Vec<TyParam>> {
        self.parse_list_optional(TokenKind::OpenBracket, TokenKind::CloseBracket, |this| {
            let ident = this.eat(TokenKind::empty_ident())?;
            Ok(TyParam { word: ident.word() })
        })
        .map(|(t, _)| t)
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

    fn parse_fn_params(&mut self) -> ParseResult<(Vec<FnParam>, bool)> {
        let mut params = vec![];

        if self.is(TokenKind::OpenParen) {
            loop {
                if self.is(TokenKind::DotDot) {
                    self.eat(TokenKind::CloseParen)?;
                    return Ok((params, true));
                }

                if self.is(TokenKind::CloseParen) {
                    break;
                }

                let ident = self.eat(TokenKind::empty_ident())?;
                self.eat(TokenKind::Colon)?;
                let ty_expr = self.parse_ty()?;
                params.push(FnParam { name: ident.word(), ty_expr, span: ident.span });

                if !params.is_empty() && !self.peek_is(TokenKind::CloseParen) {
                    self.eat(TokenKind::Comma)?;
                } else if self.peek_is(TokenKind::Comma) {
                    self.next();
                }
            }
        }

        Ok((params, false))
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
        if self.is(TokenKind::Let) {
            let let_ = self.parse_let(Attrs::new())?;
            Ok(Expr::Let(let_))
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

    // TODO: gotta clean this implementation a bit
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
                Some(BinOp::BitAnd | BinOp::Sub)
                    if !self.spans_are_on_same_line(
                        expr_stack.last().expect("to have an expr").span(),
                        tok.span,
                    ) =>
                {
                    // For these specific operators, we check if they are on the same line as the last
                    // expr, to avoid ambiguity with unary operators
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
        let atom = self.parse_atom()?;
        self.parse_postfix(atom)
    }

    fn parse_atom(&mut self) -> ParseResult<Expr> {
        let tok = self.eat_any()?;

        let expr = match tok.kind {
            TokenKind::Return => self.parse_return()?,
            TokenKind::If => self.parse_if()?,
            TokenKind::Loop => self.parse_loop()?,
            TokenKind::Break => Expr::Break { span: tok.span },
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
            TokenKind::Int(value) => Expr::Lit {
                kind: LitKind::Int(value.replace('_', "").parse().expect("to be a valid integer")),
                span: tok.span,
            },
            TokenKind::Float(value) => Expr::Lit {
                kind: LitKind::Float(value.replace('_', "").parse().expect("to be a valid float")),
                span: tok.span,
            },
            TokenKind::Str(value) => Expr::Lit { kind: LitKind::Str(value), span: tok.span },
            _ => return Err(unexpected_token_err("an expression", tok.kind, tok.span)),
        };

        Ok(expr)
    }

    fn parse_ty(&mut self) -> ParseResult<TyExpr> {
        let tok = self.eat_any()?;

        let ty = match tok.kind {
            TokenKind::Fn => {
                let fty = self.parse_fn_ty()?;
                TyExpr::Fn(fty)
            }
            TokenKind::Star => {
                let pointee = self.parse_ty()?;
                let span = tok.span.merge(pointee.span());
                TyExpr::RawPtr(Box::new(pointee), span)
            }
            TokenKind::OpenCurly => {
                let end = self.eat(TokenKind::CloseCurly)?;
                TyExpr::Unit(tok.span.merge(end.span))
            }
            TokenKind::Ident(..) => {
                TyExpr::Name(TyExprName { word: tok.word(), args: vec![], span: tok.span })
            }
            TokenKind::Underscore => TyExpr::Hole(tok.span),
            _ => return Err(unexpected_token_err("a type", tok.kind, tok.span)),
        };

        Ok(ty)
    }

    fn parse_fn_ty(&mut self) -> ParseResult<TyExprFn> {
        let start = self.last_span();
        let (params, is_c_variadic) = self.parse_fn_ty_params()?;
        let ret =
            self.is_and(TokenKind::Arrow, |this, _| this.parse_ty()).transpose()?.map(Box::new);
        Ok(TyExprFn { params, ret, is_c_variadic, span: start.merge(self.last_span()) })
    }

    fn parse_fn_ty_params(&mut self) -> ParseResult<(Vec<TyExpr>, bool)> {
        let mut params = vec![];

        if self.is(TokenKind::OpenParen) {
            loop {
                if self.is(TokenKind::DotDot) {
                    self.eat(TokenKind::CloseParen)?;
                    return Ok((params, true));
                }

                if self.is(TokenKind::CloseParen) {
                    break;
                }

                params.push(self.parse_ty()?);

                if !params.is_empty() && !self.peek_is(TokenKind::CloseParen) {
                    self.eat(TokenKind::Comma)?;
                } else if self.peek_is(TokenKind::Comma) {
                    self.next();
                }
            }
        }

        Ok((params, false))
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

                return Err(unexpected_token_err("{ or `if`", tok.kind, tok.span));
            }
        } else {
            None
        };

        let span = start.merge(otherwise.as_ref().map_or(then.span(), |o| o.span()));

        Ok(Expr::If { cond: Box::new(cond), then: Box::new(then), otherwise, span })
    }

    fn parse_loop(&mut self) -> ParseResult<Expr> {
        let start = self.last_span();

        let cond = if self.is(TokenKind::If) { Some(Box::new(self.parse_expr()?)) } else { None };

        self.eat(TokenKind::OpenCurly)?;
        let expr = self.parse_block()?;

        let span = start.merge(expr.span());

        Ok(Expr::Loop { cond, expr: Box::new(expr), span })
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
                    Ok(Expr::Cast { expr: Box::new(expr), ty_expr: ty, span })
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
                //         Err(unexpected_token_err(
                //             "`as`",
                //             tok.kind,
                //             tok.span,
                //         ))
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
                        return Err(Diagnostic::error("parse::mixed_args")
                            .with_message(
                                "positional arguments are not allowed after named arguments",
                            )
                            .with_label(
                                Label::primary(expr.span())
                                    .with_message("unexpected positional argument"),
                            ));
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
        mut f: impl FnMut(&mut Self) -> Result<T, Diagnostic>,
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

    fn parse_list_optional<T>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        f: impl FnMut(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<(Vec<T>, Span)> {
        if self.peek_is(open) {
            self.parse_list(open, close, f)
        } else {
            Ok((vec![], self.last_span()))
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
        self.token().ok_or_else(|| {
            Diagnostic::error("parse::unexpected_eof")
                .with_message("unexpected end of file")
                .with_label(Label::primary(self.last_span()).with_message("here"))
        })
    }

    #[inline]
    fn is_and<T>(
        &mut self,
        expected: TokenKind,
        mut f: impl FnMut(&mut Self, Token) -> T,
    ) -> Option<T> {
        match self.token() {
            Some(tok) if tok.kind_is(expected) => {
                self.next();
                Some(f(self, tok))
            }
            _ => None,
        }
    }

    #[inline]
    fn is(&mut self, expected: TokenKind) -> bool {
        self.is_predicate(|_, tok| tok.kind_is(expected))
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
            Err(unexpected_token_err(&expected.to_string(), tok.kind, tok.span))
        }
    }

    #[inline]
    fn parent_path(&self) -> Option<&Utf8Path> {
        self.source.path().parent()
    }
}

#[inline]
fn unexpected_token_err(expected: &str, found: TokenKind, span: Span) -> Diagnostic {
    Diagnostic::error("parse::unexpected_token")
        .with_message(format!("expected {expected}, found {found}"))
        .with_label(Label::primary(span).with_message("found here"))
}

type ParseResult<T> = Result<T, Diagnostic>;

create_bool_enum!(AllowTyParams);
