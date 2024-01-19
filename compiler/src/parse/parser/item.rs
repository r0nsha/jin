use std::ops::ControlFlow;

use crate::{
    ast::{
        token::{Token, TokenKind},
        Attr, AttrKind, Attrs, ExternLet, Fn, FnKind, FnParam, FnSig, Item,
        Let,
    },
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::{CallConv, TyExpr},
    parse::{
        errors,
        parser::{AllowOmitParens, Parser, RequireSigTy},
    },
    span::{Span, Spanned},
    word::Word,
};

impl<'a> Parser<'a> {
    pub(super) fn maybe_parse_item(
        &mut self,
    ) -> DiagnosticResult<Option<Item>> {
        let attrs = self.parse_attrs()?;

        if self.is(TokenKind::Fn) {
            return self.parse_fn_item(attrs).map(Some);
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
                .parse_import(attrs, start)
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

    fn parse_fn_item(&mut self, attrs: Attrs) -> DiagnosticResult<Item> {
        if self.is(TokenKind::Extern) {
            let fun = self.parse_extern_fn(attrs)?;
            return Ok(Item::Fn(fun));
        }

        let name = self.eat_ident()?;

        if self.is(TokenKind::Dot) {
            let fnname = self.eat_ident()?;
            let fun = self.parse_bare_fn(attrs, fnname)?;
            Ok(Item::Assoc(name.word(), Box::new(Item::Fn(fun))))
        } else {
            let fun = self.parse_bare_fn(attrs, name)?;
            Ok(Item::Fn(fun))
        }
    }

    fn parse_extern_fn(&mut self, attrs: Attrs) -> DiagnosticResult<Fn> {
        let callconv = self.eat(TokenKind::empty_str())?;
        let callconv_str = callconv.str_value().as_str();
        let callconv = CallConv::try_from(callconv_str).map_err(|()| {
            Diagnostic::error()
                .with_message(format!(
                    "unknown calling convention `{callconv_str}`"
                ))
                .with_label(
                    Label::primary(callconv.span)
                        .with_message("unknown calling convention"),
                )
        })?;
        let name = self.eat_ident()?;
        let vis = self.parse_vis();
        let (sig, is_c_variadic) =
            self.parse_fn_sig(name.word(), RequireSigTy::Yes)?;

        Ok(Fn {
            attrs,
            vis,
            sig,
            kind: FnKind::Extern { callconv, is_c_variadic },
            span: name.span,
        })
    }

    fn parse_bare_fn(
        &mut self,
        attrs: Attrs,
        name: Token,
    ) -> DiagnosticResult<Fn> {
        let vis = self.parse_vis();
        let (sig, is_c_variadic) =
            self.parse_fn_sig(name.word(), RequireSigTy::Yes)?;

        if is_c_variadic {
            return Err(errors::invalid_c_variadic(name.span));
        }

        self.eat(TokenKind::Eq)?;
        let body = self.parse_expr()?;

        Ok(Fn {
            attrs,
            vis,
            sig,
            kind: FnKind::Bare { body: Box::new(body) },
            span: name.span,
        })
    }

    pub(super) fn parse_fn_sig(
        &mut self,
        word: Word,
        require_sig_ty: RequireSigTy,
    ) -> DiagnosticResult<(FnSig, bool)> {
        let ty_params = self.parse_optional_ty_params()?;

        let (params, ret, is_c_variadic) =
            self.parse_fn_sig_helper(AllowOmitParens::No, require_sig_ty)?;

        Ok((FnSig { word, ty_params, params, ret }, is_c_variadic))
    }

    pub(super) fn parse_fn_sig_helper(
        &mut self,
        allow_omit_parens: AllowOmitParens,
        require_sig_ty: RequireSigTy,
    ) -> DiagnosticResult<(Vec<FnParam>, Option<TyExpr>, bool)> {
        let (params, is_c_variadic) = if allow_omit_parens.into()
            && !self.peek_is(TokenKind::OpenParen)
        {
            (vec![], false)
        } else {
            self.parse_fn_params(require_sig_ty)?
        };

        let ret = match require_sig_ty {
            RequireSigTy::Yes => Some(self.parse_ty()?),
            RequireSigTy::No(delimeter) => {
                if self.peek_is(delimeter) {
                    None
                } else {
                    Some(self.parse_ty()?)
                }
            }
        };

        Ok((params, ret, is_c_variadic))
    }

    fn parse_fn_params(
        &mut self,
        require_sig_ty: RequireSigTy,
    ) -> DiagnosticResult<(Vec<FnParam>, bool)> {
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

                let ty_expr = if require_sig_ty == RequireSigTy::Yes {
                    this.eat(TokenKind::Colon)?;
                    Some(this.parse_ty()?)
                } else if this.is(TokenKind::Colon) {
                    Some(this.parse_ty()?)
                } else {
                    None
                };

                Ok(ControlFlow::Continue(FnParam {
                    span: pat.span(),
                    pat,
                    ty_expr,
                }))
            },
        )?;

        Ok((params, is_c_variadic))
    }

    fn parse_attrs(&mut self) -> DiagnosticResult<Attrs> {
        let mut attrs = Attrs::new();

        while self.is(TokenKind::At) {
            let (kind, span) = self.parse_attr_kind()?;
            attrs.push(Attr { kind, span });
        }

        Ok(attrs)
    }

    fn parse_attr_kind(&mut self) -> DiagnosticResult<(AttrKind, Span)> {
        let start = self.last_span();
        let ident = self.eat_ident()?;
        let span = start.merge(ident.span);
        let name = ident.str_value().as_str();

        let kind = AttrKind::try_from(name).map_err(|()| {
            Diagnostic::error()
                .with_message(format!("unknown attribute `{name}`"))
                .with_label(
                    Label::primary(span).with_message("unknown attribute"),
                )
        })?;

        Ok((kind, span))
    }

    pub(super) fn parse_let(&mut self, attrs: Attrs) -> DiagnosticResult<Let> {
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
}
