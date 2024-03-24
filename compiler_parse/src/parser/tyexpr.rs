use std::ops::ControlFlow;

use compiler_core::{
    diagnostics::DiagnosticResult,
    middle::{CallConv, TyExpr, TyExprFn},
    span::Spanned,
    word::Word,
};

use crate::{
    errors,
    parser::Parser,
    token::{Kw, TokenKind},
};

impl<'a> Parser<'a> {
    pub(super) fn parse_ty(&mut self) -> DiagnosticResult<TyExpr> {
        let tok = self.eat_any("a type")?;

        let ty = match tok.kind {
            TokenKind::Ident(..) => self.parse_ty_path(tok.word())?,
            TokenKind::Amp => {
                let mutability = self.parse_mutability();
                let inner = self.parse_ty()?;
                let span = tok.span.merge(inner.span());
                TyExpr::Ref(Box::new(inner), mutability, span)
            }
            TokenKind::OpenBrack => {
                self.eat(TokenKind::CloseBrack)?;
                let inner = self.parse_ty()?;
                let span = tok.span.merge(inner.span());
                TyExpr::Slice(Box::new(inner), span)
            }
            TokenKind::Kw(Kw::Fn) => {
                let fty = self.parse_fn_ty()?;
                TyExpr::Fn(fty)
            }
            TokenKind::OpenParen => {
                let ty = self.parse_ty()?;
                let close = self.eat(TokenKind::CloseParen)?;
                let span = ty.span().merge(close.span);
                TyExpr::Group(Box::new(ty), span)
            }
            TokenKind::Underscore => TyExpr::Hole(tok.span),
            _ => return Err(errors::unexpected_token_err("a type", tok.kind, tok.span)),
        };

        Ok(ty)
    }

    pub(super) fn is_ty_start(&self) -> bool {
        // These pattern must stay in sync with `parse_ty`
        matches!(
            self.token().map(|t| t.kind),
            Some(
                TokenKind::Ident(..)
                    | TokenKind::Amp
                    | TokenKind::OpenBrack
                    | TokenKind::Kw(Kw::Fn)
                    | TokenKind::OpenCurly
                    | TokenKind::OpenParen
                    | TokenKind::Underscore,
            )
        )
    }

    fn parse_ty_path(&mut self, word: Word) -> DiagnosticResult<TyExpr> {
        let start_span = word.span();
        let mut path = vec![word];

        while self.is(TokenKind::Dot) {
            path.push(self.eat_ident()?.word());
        }

        let targs = self.parse_optional_targs()?;
        let span = start_span.merge(self.last_span());

        Ok(TyExpr::Path(path, targs, span))
    }

    fn parse_fn_ty(&mut self) -> DiagnosticResult<TyExprFn> {
        let start = self.last_span();
        let (is_extern, callconv) = if self.is_kw(Kw::Extern) {
            let callconv = self.parse_callconv()?;
            (true, callconv)
        } else {
            (false, CallConv::default())
        };
        let (params, is_c_variadic) = self.parse_fn_tparams()?;
        let ret = Box::new(self.parse_ty()?);

        Ok(TyExprFn {
            params,
            ret,
            is_extern,
            is_c_variadic,
            callconv,
            span: start.merge(self.last_span()),
        })
    }

    fn parse_fn_tparams(&mut self) -> DiagnosticResult<(Vec<TyExpr>, bool)> {
        let mut is_c_variadic = false;

        let (params, _) = self.parse_list(TokenKind::OpenParen, TokenKind::CloseParen, |this| {
            if this.is(TokenKind::DotDot) {
                is_c_variadic = true;
                return Ok(ControlFlow::Break(()));
            }

            this.parse_ty().map(ControlFlow::Continue)
        })?;

        Ok((params, is_c_variadic))
    }

    pub(super) fn parse_targs(&mut self) -> DiagnosticResult<Vec<TyExpr>> {
        self.parse_list(TokenKind::OpenBrack, TokenKind::CloseBrack, |this| {
            this.parse_ty().map(ControlFlow::Continue)
        })
        .map(|(t, _)| t)
    }

    pub(super) fn parse_optional_targs(&mut self) -> DiagnosticResult<Option<Vec<TyExpr>>> {
        if self.peek_is(TokenKind::OpenBrack) {
            Ok(Some(self.parse_targs()?))
        } else {
            Ok(None)
        }
    }
}
