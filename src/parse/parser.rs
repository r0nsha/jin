use std::ops::ControlFlow;

use camino::{Utf8Path, Utf8PathBuf};
use codespan_reporting::files::Files;
use rustc_hash::FxHashSet;

use crate::{
    ast::{
        token::{Token, TokenKind},
        Attr, AttrKind, Attrs, CallArg, Expr, ExternImport, ExternLet, Fn,
        FnKind, FnParam, FnSig, Item, Let, LitKind, Module, StructTyDef,
        StructTyField, TyDef, TyDefKind, TyParam,
    },
    db::{Db, DefId, ExternLib, StructKind},
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    macros::create_bool_enum,
    middle::{
        BinOp, Mutability, NamePat, Pat, TyExpr, TyExprFn, TyExprName, UnOp,
        Vis,
    },
    parse::errors,
    qpath::QPath,
    span::{Source, SourceId, Span, Spanned},
    ty::TyKind,
    word::Word,
};

pub fn parse(
    db: &Db,
    source: &Source,
    tokens: Vec<Token>,
) -> DiagnosticResult<(Module, FxHashSet<Utf8PathBuf>)> {
    let name =
        QPath::from_path(&db.main_package().root_path, source.path()).unwrap();
    let is_main = source.id() == db.main_source().id();

    let mut parser = Parser::new(db, source, tokens);
    let module = parser.parse(source.id(), name, is_main)?;

    Ok((module, parser.imported_module_paths))
}

#[derive(Debug)]
pub(super) struct Parser<'a> {
    pub(super) db: &'a Db,
    pub(super) source: &'a Source,
    pub(super) tokens: Vec<Token>,
    pub(super) pos: usize,
    pub(super) imported_module_paths: FxHashSet<Utf8PathBuf>,
}

impl<'a> Parser<'a> {
    fn new(db: &'a Db, source: &'a Source, tokens: Vec<Token>) -> Self {
        Self {
            db,
            source,
            tokens,
            pos: 0,
            imported_module_paths: FxHashSet::default(),
        }
    }

    fn parse(
        &mut self,
        source_id: SourceId,
        name: QPath,
        is_main: bool,
    ) -> DiagnosticResult<Module> {
        let mut module = Module::new(source_id, name, is_main);

        while !self.eof() {
            module.items.push(self.parse_top_level()?);
        }

        Ok(module)
    }

    fn parse_top_level(&mut self) -> DiagnosticResult<Item> {
        if let Some(item) = self.maybe_parse_item()? {
            Ok(item)
        } else {
            let token = self.require()?;
            Err(errors::unexpected_token_err("an item", token.kind, token.span))
        }
    }

    fn maybe_parse_item(&mut self) -> DiagnosticResult<Option<Item>> {
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
            return self.parse_tydef(attrs).map(|t| Some(Item::Type(t)));
        }

        if self.is(TokenKind::Import) {
            let start = self.last_span();

            if self.is(TokenKind::Extern) {
                return self
                    .parse_extern_import(&attrs, start)
                    .map(|i| Some(Item::ExternImport(i)));
            }

            return self
                .parse_import(&attrs, start)
                .map(|i| Some(Item::Import(i)));
        }

        if !attrs.is_empty() {
            let token = self.require()?;
            return Err(errors::unexpected_token_err(
                "an item after attribute",
                token.kind,
                token.span,
            ));
        }

        Ok(None)
    }

    fn parse_extern_import(
        &mut self,
        attrs: &[Attr],
        start: Span,
    ) -> DiagnosticResult<ExternImport> {
        let path_tok = self.eat(TokenKind::empty_str())?;
        let path = path_tok.str_value();
        let relative_to = self.parent_path().unwrap();

        let lib =
            ExternLib::try_from_str(&path, relative_to).ok_or_else(|| {
                errors::path_not_found(path.as_str(), path_tok.span)
            })?;

        Ok(ExternImport {
            attrs: attrs.to_owned(),
            lib,
            span: start.merge(path_tok.span),
        })
    }

    fn parse_attrs(&mut self) -> DiagnosticResult<Vec<Attr>> {
        let mut attrs = vec![];

        while self.is(TokenKind::At) {
            attrs.push(self.parse_attr()?);
        }

        Ok(attrs)
    }

    fn parse_attr(&mut self) -> DiagnosticResult<Attr> {
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

    fn parse_attr_kind(&mut self) -> DiagnosticResult<(AttrKind, Span)> {
        let ident = self.eat_ident()?;
        let attr_name = ident.str_value().as_str();

        let kind = AttrKind::try_from(attr_name).map_err(|()| {
            Diagnostic::error()
                .with_message("unknown attribute {attr_name}")
                .with_label(
                    Label::primary(ident.span)
                        .with_message("unknown attribute"),
                )
        })?;

        Ok((kind, ident.span))
    }

    fn parse_fn(&mut self, attrs: Attrs) -> DiagnosticResult<Fn> {
        if self.is(TokenKind::Extern) {
            let name_ident = self.eat_ident()?;
            let vis = self.parse_vis();
            let (sig, is_c_variadic) = self.parse_fn_sig(
                name_ident.word(),
                AllowTyParams::No,
                RequireRetTy::Yes,
            )?;

            Ok(Fn {
                attrs,
                vis,
                sig,
                kind: FnKind::Extern { is_c_variadic },
                span: name_ident.span,
            })
        } else {
            let name_ident = self.eat_ident()?;
            let vis = self.parse_vis();
            let (sig, is_c_variadic) = self.parse_fn_sig(
                name_ident.word(),
                AllowTyParams::Yes,
                RequireRetTy::Yes,
            )?;

            if is_c_variadic {
                return Err(Diagnostic::error()
                    .with_message("non extern function cannot use c varargs")
                    .with_label(
                        Label::primary(name_ident.span).with_message("here"),
                    ));
            }

            self.eat(TokenKind::Eq)?;
            let body = self.parse_expr()?;

            Ok(Fn {
                attrs,
                vis,
                sig,
                kind: FnKind::Bare { body: Box::new(body) },
                span: name_ident.span,
            })
        }
    }

    fn parse_let(&mut self, attrs: Attrs) -> DiagnosticResult<Let> {
        let start = self.last_span();
        let pat = self.parse_pat()?;

        let ty_expr = self
            .is_and(TokenKind::Colon, |this, _| this.parse_ty())
            .transpose()?;
        self.eat(TokenKind::Eq)?;

        let value = self.parse_expr()?;

        Ok(Let {
            attrs,
            pat,
            ty_expr,
            span: start.merge(value.span()),
            value: Box::new(value),
        })
    }

    fn parse_extern_let(
        &mut self,
        attrs: Attrs,
    ) -> DiagnosticResult<ExternLet> {
        let start = self.last_span();

        let mutability = self.parse_mutability();
        let ident = self.eat_ident()?;
        let vis = self.parse_vis();

        self.eat(TokenKind::Colon)?;
        let ty_expr = self.parse_ty()?;

        let span = start.merge(ty_expr.span());

        Ok(ExternLet {
            attrs,
            mutability,
            vis,
            word: ident.word(),
            ty_expr,
            span,
        })
    }

    fn parse_tydef(&mut self, attrs: Attrs) -> DiagnosticResult<TyDef> {
        let start = self.last_span();

        let ident = self.eat_ident()?;
        let vis = self.parse_vis();

        let ty_params = self.parse_optional_ty_params()?;

        let kind = self.parse_tydef_kind()?;
        let span = start.merge(self.last_span());

        Ok(TyDef { attrs, word: ident.word(), vis, ty_params, kind, span })
    }

    fn parse_tydef_kind(&mut self) -> DiagnosticResult<TyDefKind> {
        if self.is(TokenKind::Extern) {
            self.parse_tydef_struct(StructKind::Extern)
        } else if self.peek_is(TokenKind::OpenParen) {
            self.parse_tydef_struct(StructKind::Ref)
        } else {
            let tok = self.require()?;
            Err(errors::unexpected_token_err(
                "( or `extern`",
                tok.kind,
                tok.span,
            ))
        }
    }

    fn parse_tydef_struct(
        &mut self,
        kind: StructKind,
    ) -> DiagnosticResult<TyDefKind> {
        let (fields, _) = self.parse_list(
            TokenKind::OpenParen,
            TokenKind::CloseParen,
            |this| {
                let ident = this.eat_ident()?;
                let vis = this.parse_vis();
                this.eat(TokenKind::Colon)?;
                let ty_expr = this.parse_ty()?;
                Ok(ControlFlow::Continue(StructTyField {
                    name: ident.word(),
                    vis,
                    ty_expr,
                }))
            },
        )?;

        Ok(TyDefKind::Struct(StructTyDef { kind, fields }))
    }

    fn parse_pat(&mut self) -> DiagnosticResult<Pat> {
        let mutability = self.parse_mutability();
        let tok = self.eat_any()?;

        match tok.kind {
            TokenKind::Ident(_) => {
                let vis = self.parse_vis();

                Ok(Pat::Name(NamePat {
                    id: DefId::INVALID,
                    word: tok.word(),
                    vis,
                    mutability,
                    ty: TyKind::Unknown.into(),
                }))
            }
            TokenKind::Underscore => Ok(Pat::Discard(tok.span)),
            _ => Err(errors::unexpected_token_err(
                "a pattern",
                tok.kind,
                tok.span,
            )),
        }
    }

    fn parse_mutability(&mut self) -> Mutability {
        self.parse_optional_mutability().unwrap_or_default()
    }

    fn parse_optional_mutability(&mut self) -> Option<Mutability> {
        if self.is(TokenKind::Mut) {
            Some(Mutability::Mut)
        } else if self.is(TokenKind::Imm) {
            Some(Mutability::Imm)
        } else {
            None
        }
    }

    pub fn parse_vis(&mut self) -> Vis {
        if self.is(TokenKind::Star) {
            Vis::Public
        } else {
            Vis::Private
        }
    }

    fn parse_fn_sig(
        &mut self,
        name: Word,
        allow_ty_params: AllowTyParams,
        require_ret_ty: RequireRetTy,
    ) -> DiagnosticResult<(FnSig, bool)> {
        let ty_params = if allow_ty_params == AllowTyParams::Yes {
            self.parse_optional_ty_params()?
        } else {
            vec![]
        };

        let (params, is_c_variadic) = self.parse_fn_params()?;
        let ret = match require_ret_ty {
            RequireRetTy::Yes => Some(self.parse_ty()?),
            RequireRetTy::No(delimeter) => {
                self.is_and(delimeter, |this, _| this.parse_ty()).transpose()?
            }
        };

        Ok((FnSig { word: name, ty_params, params, ret }, is_c_variadic))
    }

    fn parse_optional_ty_params(&mut self) -> DiagnosticResult<Vec<TyParam>> {
        self.parse_list_optional(
            TokenKind::OpenBracket,
            TokenKind::CloseBracket,
            |this| {
                let ident = this.eat_ident()?;
                Ok(ControlFlow::Continue(TyParam { word: ident.word() }))
            },
        )
        .map(|(t, _)| t)
    }

    fn parse_optional_ty_args(
        &mut self,
    ) -> DiagnosticResult<Option<Vec<TyExpr>>> {
        if self.peek_is(TokenKind::OpenBracket) {
            let args = self.parse_ty_args().map(|(t, _)| t)?;
            Ok(Some(args))
        } else {
            Ok(None)
        }
    }

    fn parse_ty_args(&mut self) -> DiagnosticResult<(Vec<TyExpr>, Span)> {
        self.parse_list(
            TokenKind::OpenBracket,
            TokenKind::CloseBracket,
            |this| this.parse_ty().map(ControlFlow::Continue),
        )
    }

    fn parse_fn_params(&mut self) -> DiagnosticResult<(Vec<FnParam>, bool)> {
        let mut is_c_variadic = false;

        let (params, _) = self.parse_list(
            TokenKind::OpenParen,
            TokenKind::CloseParen,
            |this| {
                if this.is(TokenKind::DotDot) {
                    is_c_variadic = true;
                    return Ok(ControlFlow::Break(()));
                }

                let pat = this.parse_pat()?;
                this.eat(TokenKind::Colon)?;
                let ty_expr = this.parse_ty()?;

                Ok(ControlFlow::Continue(FnParam {
                    span: pat.span(),
                    pat,
                    ty_expr,
                }))
            },
        )?;

        Ok((params, is_c_variadic))
    }

    fn parse_block(&mut self) -> DiagnosticResult<Expr> {
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

    fn parse_stmt(&mut self) -> DiagnosticResult<Expr> {
        if self.is(TokenKind::Let) {
            let let_ = self.parse_let(Attrs::new())?;
            Ok(Expr::Let(let_))
        } else {
            self.parse_expr()
        }
    }

    fn parse_return(&mut self) -> DiagnosticResult<Expr> {
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

    fn parse_expr(&mut self) -> DiagnosticResult<Expr> {
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

                expr_stack.push(Expr::Binary {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op,
                    span,
                });
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

            expr_stack.push(Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
                span,
            });
        }

        Ok(expr_stack.into_iter().next().unwrap())
    }

    fn parse_operand(&mut self) -> DiagnosticResult<Expr> {
        let atom = self.parse_atom()?;
        self.parse_postfix(atom)
    }

    fn parse_atom(&mut self) -> DiagnosticResult<Expr> {
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
            TokenKind::Amp => {
                let mutability = self.parse_mutability();
                let expr = self.parse_operand()?;

                Expr::Unary {
                    span: tok.span.merge(expr.span()),
                    expr: Box::new(expr),
                    op: UnOp::Ref(mutability),
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
            TokenKind::True => {
                Expr::Lit { kind: LitKind::Bool(true), span: tok.span }
            }
            TokenKind::False => {
                Expr::Lit { kind: LitKind::Bool(false), span: tok.span }
            }
            TokenKind::Ident(..) => {
                let ty_args = self.parse_optional_ty_args()?;
                Expr::Name { word: tok.word(), ty_args, span: tok.span }
            }
            TokenKind::Int(value) => Expr::Lit {
                kind: LitKind::Int(
                    value
                        .replace('_', "")
                        .parse()
                        .expect("to be a valid integer"),
                ),
                span: tok.span,
            },
            TokenKind::Float(value) => Expr::Lit {
                kind: LitKind::Float(
                    value
                        .replace('_', "")
                        .parse()
                        .expect("to be a valid float"),
                ),
                span: tok.span,
            },
            TokenKind::Str(value) => {
                Expr::Lit { kind: LitKind::Str(value), span: tok.span }
            }
            _ => {
                return Err(errors::unexpected_token_err(
                    "an expression",
                    tok.kind,
                    tok.span,
                ))
            }
        };

        Ok(expr)
    }

    fn parse_ty(&mut self) -> DiagnosticResult<TyExpr> {
        let tok = self.eat_any()?;

        let ty = match tok.kind {
            TokenKind::Fn => {
                let fty = self.parse_fn_ty()?;
                TyExpr::Fn(fty)
            }
            TokenKind::Amp => {
                let mutability = self.parse_mutability();
                let inner = self.parse_ty()?;
                let span = tok.span.merge(inner.span());
                TyExpr::Ref(Box::new(inner), mutability, span)
            }
            TokenKind::Star => {
                let pointee = self.parse_ty()?;
                let span = tok.span.merge(pointee.span());
                TyExpr::RawPtr(Box::new(pointee), span)
            }
            TokenKind::Ident(..) => TyExpr::Name(TyExprName {
                word: tok.word(),
                ty_args: vec![],
                span: tok.span,
            }),
            TokenKind::Underscore => TyExpr::Hole(tok.span),
            _ => {
                return Err(errors::unexpected_token_err(
                    "a type", tok.kind, tok.span,
                ))
            }
        };

        Ok(ty)
    }

    fn parse_fn_ty(&mut self) -> DiagnosticResult<TyExprFn> {
        let start = self.last_span();
        let (params, is_c_variadic) = self.parse_fn_ty_params()?;
        let ret = self.parse_ty()?;

        Ok(TyExprFn {
            params,
            ret: Box::new(ret),
            is_c_variadic,
            span: start.merge(self.last_span()),
        })
    }

    fn parse_fn_ty_params(&mut self) -> DiagnosticResult<(Vec<TyExpr>, bool)> {
        let mut is_c_variadic = false;

        let (params, _) = self.parse_list(
            TokenKind::OpenParen,
            TokenKind::CloseParen,
            |this| {
                if this.is(TokenKind::DotDot) {
                    is_c_variadic = true;
                    return Ok(ControlFlow::Break(()));
                }

                this.parse_ty().map(ControlFlow::Continue)
            },
        )?;

        Ok((params, is_c_variadic))
    }

    fn parse_if(&mut self) -> DiagnosticResult<Expr> {
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

                return Err(errors::unexpected_token_err(
                    "{ or `if`",
                    tok.kind,
                    tok.span,
                ));
            }
        } else {
            None
        };

        let span =
            start.merge(otherwise.as_ref().map_or(then.span(), |o| o.span()));

        Ok(Expr::If {
            cond: Box::new(cond),
            then: Box::new(then),
            otherwise,
            span,
        })
    }

    fn parse_loop(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();

        let cond = if self.is(TokenKind::If) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };

        self.eat(TokenKind::OpenCurly)?;
        let expr = self.parse_block()?;

        let span = start.merge(expr.span());

        Ok(Expr::Loop { cond, expr: Box::new(expr), span })
    }

    fn parse_postfix(&mut self, mut expr: Expr) -> DiagnosticResult<Expr> {
        while let Some(tok) = self.token() {
            expr = match tok.kind {
                TokenKind::OpenParen => self.parse_call(expr)?,
                TokenKind::As => {
                    self.next();
                    let ty = self.parse_ty()?;
                    let span = expr.span().merge(ty.span());
                    Expr::Cast { expr: Box::new(expr), ty_expr: ty, span }
                }
                TokenKind::Dot => {
                    self.next();
                    let name = self.eat_ident()?.word();

                    let ty_args = self.parse_optional_ty_args()?;

                    if ty_args.is_some() || self.peek_is(TokenKind::OpenParen) {
                        let (args, args_span) = self.parse_call_args()?;
                        let span = expr.span().merge(args_span);
                        Expr::MethodCall {
                            expr: Box::new(expr),
                            method: name,
                            ty_args,
                            args,
                            span,
                        }
                    } else {
                        let span = expr.span().merge(name.span());
                        Expr::Field { expr: Box::new(expr), field: name, span }
                    }
                }
                TokenKind::Eq => {
                    self.next();
                    let rhs = self.parse_expr()?;
                    let span = expr.span().merge(rhs.span());

                    Expr::Assign {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                        op: None,
                        span,
                    }
                }
                _ => {
                    if let Some(op) = BinOp::from_assign_op(tok.kind) {
                        // OpAssign (x += 1)
                        self.next();

                        let rhs = self.parse_expr()?;
                        let span = expr.span().merge(rhs.span());

                        Expr::Assign {
                            lhs: Box::new(expr),
                            rhs: Box::new(rhs),
                            op: Some(op),
                            span,
                        }
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(expr)
    }

    fn parse_call(&mut self, expr: Expr) -> DiagnosticResult<Expr> {
        let (args, args_span) = self.parse_call_args()?;
        let span = expr.span().merge(args_span);
        Ok(Expr::Call { callee: Box::new(expr), args, span })
    }

    fn parse_call_args(&mut self) -> DiagnosticResult<(Vec<CallArg>, Span)> {
        let mut passed_named_arg = false;

        self.parse_list(TokenKind::OpenParen, TokenKind::CloseParen, |this| {
            let arg = Parser::parse_arg(this)?;

            match &arg {
                CallArg::Positional(expr) if passed_named_arg => {
                    return Err(Diagnostic::error()
                        .with_message(
                            "positional arguments are not allowed after named \
                             arguments",
                        )
                        .with_label(
                            Label::primary(expr.span())
                                .with_message("unexpected positional argument"),
                        ));
                }
                CallArg::Positional(_) => (),
                CallArg::Named(..) => passed_named_arg = true,
            }

            Ok(ControlFlow::Continue(arg))
        })
    }

    fn parse_arg(&mut self) -> DiagnosticResult<CallArg> {
        if self.is_ident() {
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

    pub(super) fn parse_list<T>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        mut f: impl FnMut(&mut Self) -> DiagnosticResult<ControlFlow<(), T>>,
    ) -> DiagnosticResult<(Vec<T>, Span)> {
        let mut values = Vec::new();
        let open_tok = self.eat(open)?;

        while !self.is(close) {
            match f(self)? {
                ControlFlow::Continue(v) => values.push(v),
                ControlFlow::Break(()) => {
                    self.eat(close)?;
                    break;
                }
            }

            if !values.is_empty() && !self.peek_is(close) {
                self.eat(TokenKind::Comma)?;
            } else if self.peek_is(TokenKind::Comma) {
                self.next();
            }
        }

        Ok((values, open_tok.span.merge(self.last_span())))
    }

    pub(super) fn parse_list_optional<T>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        f: impl FnMut(&mut Self) -> DiagnosticResult<ControlFlow<(), T>>,
    ) -> DiagnosticResult<(Vec<T>, Span)> {
        if self.peek_is(open) {
            self.parse_list(open, close, f)
        } else {
            Ok((vec![], self.last_span()))
        }
    }
}

impl<'a> Parser<'a> {
    #[inline]
    pub(super) fn eat(
        &mut self,
        expected: TokenKind,
    ) -> DiagnosticResult<Token> {
        let tok = self.eat_any()?;
        Self::require_kind(tok, expected)
    }

    #[inline]
    pub(super) fn eat_ident(&mut self) -> DiagnosticResult<Token> {
        self.eat(TokenKind::empty_ident())
    }

    #[inline]
    pub(super) fn eat_any(&mut self) -> DiagnosticResult<Token> {
        let tok = self.require()?;
        self.next();
        Ok(tok)
    }

    #[inline]
    pub(super) fn require(&mut self) -> DiagnosticResult<Token> {
        self.token().ok_or_else(|| {
            Diagnostic::error()
                .with_message("unexpected end of file")
                .with_label(
                    Label::primary(self.last_span()).with_message("here"),
                )
        })
    }

    #[inline]
    pub(super) fn is_and<T>(
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
    pub(super) fn is(&mut self, expected: TokenKind) -> bool {
        self.is_predicate(|_, tok| tok.kind_is(expected))
    }

    #[inline]
    pub(super) fn is_ident(&mut self) -> bool {
        self.is(TokenKind::empty_ident())
    }

    #[inline]
    pub(super) fn is_predicate(
        &mut self,
        mut f: impl FnMut(&mut Self, Token) -> bool,
    ) -> bool {
        match self.token() {
            Some(tok) if f(self, tok) => {
                self.next();
                true
            }
            _ => false,
        }
    }

    #[inline]
    pub(super) fn spans_are_on_same_line(&self, s1: Span, s2: Span) -> bool {
        fn line_index(parser: &Parser, pos: u32) -> usize {
            parser.source.line_index(parser.source.id(), pos as usize).unwrap()
        }

        line_index(self, s1.end()) == line_index(self, s2.start())
    }

    #[inline]
    pub(super) fn token(&self) -> Option<Token> {
        self.tokens.get(self.pos).copied()
    }

    #[inline]
    pub(super) fn peek<R: Default>(&self, f: impl FnOnce(Token) -> R) -> R {
        self.token().map(f).unwrap_or_default()
    }

    #[inline]
    pub(super) fn peek_is(&self, expected: TokenKind) -> bool {
        self.peek(|t| t.kind_is(expected))
    }

    #[inline]
    pub(super) fn last_span(&self) -> Span {
        self.last_token().span
    }

    #[inline]
    pub(super) fn last_token(&self) -> Token {
        self.tokens[self.pos - 1]
    }

    #[inline]
    pub(super) fn next(&mut self) {
        self.pos += 1;
    }

    #[inline]
    pub(super) fn prev(&mut self) {
        self.pos -= 1;
    }

    #[inline]
    pub(super) fn eof(&self) -> bool {
        self.pos == self.tokens.len()
    }

    #[inline]
    pub(super) fn require_kind(
        tok: Token,
        expected: TokenKind,
    ) -> DiagnosticResult<Token> {
        if tok.kind_is(expected) {
            Ok(tok)
        } else {
            Err(errors::unexpected_token_err(
                &expected.to_string(),
                tok.kind,
                tok.span,
            ))
        }
    }

    #[inline]
    pub(super) fn parent_path(&self) -> Option<&Utf8Path> {
        self.source.path().parent()
    }
}

create_bool_enum!(AllowTyParams);

#[derive(Debug, Clone, Copy, PartialEq)]
enum RequireRetTy {
    Yes,
    #[allow(dead_code)]
    No(TokenKind),
}
