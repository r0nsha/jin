use std::ops::ControlFlow;

use compiler_helpers::create_bool_enum;

use compiler_ast::{
    Attr, AttrArgs, AttrId, Attrs, ExternLet, Fn, FnKind, FnParam, FnSig, Item, ItemKind, Let,
    LetKind,
};
use compiler_core::{
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::CallConv,
    span::{Span, Spanned},
    word::Word,
};

use crate::{
    errors,
    parser::{Parser, RequireSigTy},
    token::{Kw, Token, TokenKind},
};

impl<'a> Parser<'a> {
    pub(super) fn parse_item(&mut self) -> DiagnosticResult<Item> {
        let kind = self.parse_item_kind()?;
        Ok(Item { loc: self.loc(), kind })
    }

    fn parse_item_kind(&mut self) -> DiagnosticResult<ItemKind> {
        let attrs = self.parse_attrs()?;

        if self.is_kw(Kw::Fn) {
            return self.parse_fn_item(attrs);
        }

        if self.is_kw(Kw::Let) {
            return if self.is_kw(Kw::Extern) {
                self.parse_extern_let(attrs).map(ItemKind::ExternLet)
            } else {
                self.parse_let(attrs, LetKind::Let, AllowVis::Yes, RequireTy::Yes)
                    .map(ItemKind::Let)
            };
        }

        if self.is_kw(Kw::Const) {
            return self
                .parse_let(attrs, LetKind::Const, AllowVis::Yes, RequireTy::Yes)
                .map(ItemKind::Let);
        }

        if self.is_kw(Kw::Type) {
            return self.parse_tydef(attrs).map(ItemKind::Type);
        }

        if self.is_kw(Kw::Mod) {
            let start = self.last_span();

            if self.is_kw(Kw::Extern) {
                return self.parse_extern_import(&attrs, start).map(ItemKind::ExternImport);
            }

            return self.parse_mod(&attrs).map(ItemKind::Import);
        }

        if self.is_kw(Kw::Use) {
            return self.parse_import(&attrs).map(ItemKind::Import);
        }

        if !attrs.is_empty() {
            return Err(self.unexpected_token("an item after attribute"));
        }

        Err(self.unexpected_token("an item"))
    }

    fn parse_fn_item(&mut self, attrs: Attrs) -> DiagnosticResult<ItemKind> {
        if self.is_kw(Kw::Extern) {
            let fun = self.parse_extern_fn(attrs)?;
            return Ok(ItemKind::Fn(fun));
        }

        let name = self.eat_ident()?;

        if self.is(TokenKind::Dot) {
            let fn_name = self.eat_ident()?;
            let fun = self.parse_bare_fn(attrs, fn_name)?;
            return Ok(ItemKind::Assoc(name.word(), Box::new(ItemKind::Fn(fun))));
        }

        let fun = self.parse_bare_fn(attrs, name)?;

        Ok(ItemKind::Fn(fun))
    }

    fn parse_extern_fn(&mut self, attrs: Attrs) -> DiagnosticResult<Fn> {
        let callconv = self.parse_callconv()?;
        let name = self.eat_ident()?;
        let vis = self.parse_vis();
        let (sig, is_c_variadic) = self.parse_fn_sig(name.word(), RequireSigTy::Yes)?;

        Ok(Fn {
            attrs,
            vis,
            sig,
            kind: FnKind::Extern { callconv, is_c_variadic },
            span: name.span,
        })
    }

    pub(super) fn parse_callconv(&mut self) -> DiagnosticResult<CallConv> {
        let callconv = self.eat_str_lit()?;
        let callconv_str = callconv.as_str();
        CallConv::try_from(callconv_str).map_err(|()| {
            Diagnostic::error(format!("unknown calling convention `{callconv_str}`"))
                .with_label(Label::primary(callconv.span(), "unknown calling convention"))
        })
    }

    fn parse_bare_fn(&mut self, attrs: Attrs, name: Token) -> DiagnosticResult<Fn> {
        let vis = self.parse_vis();
        let (sig, is_c_variadic) = self.parse_fn_sig(name.word(), RequireSigTy::Yes)?;

        if is_c_variadic {
            return Err(errors::invalid_c_variadic(name.span));
        }

        self.eat(TokenKind::Eq)?;
        let body = self.parse_expr()?;

        Ok(Fn { attrs, vis, sig, kind: FnKind::Bare { body: Box::new(body) }, span: name.span })
    }

    pub(super) fn parse_fn_sig(
        &mut self,
        word: Word,
        require_sig_ty: RequireSigTy,
    ) -> DiagnosticResult<(FnSig, bool)> {
        let tparams = self.parse_optional_tparams()?;
        let (params, is_c_variadic) = self.parse_fn_params(require_sig_ty)?;
        let ret = if require_sig_ty == RequireSigTy::Yes || self.is_ty_start() {
            Some(self.parse_ty()?)
        } else {
            None
        };
        Ok((FnSig { word, tparams, params, ret }, is_c_variadic))
    }

    pub(super) fn parse_fn_params(
        &mut self,
        require_sig_ty: RequireSigTy,
    ) -> DiagnosticResult<(Vec<FnParam>, bool)> {
        let mut is_c_variadic = false;

        let (params, _) = self.parse_list(TokenKind::OpenParen, TokenKind::CloseParen, |this| {
            if this.is(TokenKind::DotDot) {
                is_c_variadic = true;
                return Ok(ControlFlow::Break(()));
            }

            let pat = this.parse_pat(IsFnParam::Yes, AllowVis::No)?;

            let ty_expr = if require_sig_ty == RequireSigTy::Yes {
                this.eat(TokenKind::Colon)?;
                Some(this.parse_ty()?)
            } else if this.is(TokenKind::Colon) {
                Some(this.parse_ty()?)
            } else {
                None
            };

            Ok(ControlFlow::Continue(FnParam { span: pat.span(), pat, ty_expr }))
        })?;

        Ok((params, is_c_variadic))
    }

    fn parse_attrs(&mut self) -> DiagnosticResult<Attrs> {
        let mut attrs = Attrs::new();

        while self.is(TokenKind::At) {
            let (id, span) = self.parse_attr_id()?;
            let args = self.parse_attr_args(id)?;
            self.skip_semi();
            attrs.push(Attr { id, args, span });
        }

        Ok(attrs)
    }

    fn parse_attr_id(&mut self) -> DiagnosticResult<(AttrId, Span)> {
        let start = self.last_span();
        let ident = self.eat_ident()?;
        let span = start.merge(ident.span);
        let name = ident.str_value().as_str();

        let id = AttrId::try_from(name).map_err(|()| {
            Diagnostic::error(format!("unknown attribute `{name}`"))
                .with_label(Label::primary(span, "unknown attribute"))
        })?;

        Ok((id, span))
    }

    fn parse_attr_args(&mut self, id: AttrId) -> DiagnosticResult<AttrArgs> {
        match id {
            AttrId::Builtin => {
                self.eat(TokenKind::OpenParen)?;
                let name = self.eat_str_lit()?;
                self.eat(TokenKind::CloseParen)?;
                Ok(AttrArgs::Builtin(name))
            }
        }
    }

    pub(super) fn parse_let(
        &mut self,
        attrs: Attrs,
        kind: LetKind,
        allow_vis: AllowVis,
        require_ty: RequireTy,
    ) -> DiagnosticResult<Let> {
        let start = self.last_span();
        let pat = self.parse_pat(IsFnParam::No, allow_vis)?;

        let ty_expr = if self.is(TokenKind::Colon) {
            Some(self.parse_ty()?)
        } else if require_ty == RequireTy::No {
            None
        } else {
            return Err(self.unexpected_token(":"));
        };

        self.eat(TokenKind::Eq)?;

        let value = self.parse_expr()?;

        Ok(Let {
            attrs,
            kind,
            pat,
            ty_expr,
            span: start.merge(value.span()),
            value: Box::new(value),
        })
    }

    fn parse_extern_let(&mut self, attrs: Attrs) -> DiagnosticResult<ExternLet> {
        let start = self.last_span();

        let mutability = self.parse_mutability();
        let ident = self.eat_ident()?;
        let vis = self.parse_vis();
        self.eat(TokenKind::Colon)?;
        let ty_expr = self.parse_ty()?;

        let span = start.merge(ty_expr.span());
        Ok(ExternLet { attrs, mutability, vis, word: ident.word(), ty_expr, span })
    }
}

create_bool_enum!(RequireTy);
create_bool_enum!(AllowVis);
create_bool_enum!(IsFnParam);
