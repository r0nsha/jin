use std::ops::ControlFlow;

use crate::{
    ast::token::TokenKind,
    diagnostics::DiagnosticResult,
    middle::{CallConv, TyExpr, TyExprFn},
    parse::{errors, parser::Parser},
    span::Spanned,
    word::Word,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_ty(&mut self) -> DiagnosticResult<TyExpr> {
        let tok = self.eat_any()?;

        let ty = match tok.kind {
            TokenKind::Fn => {
                let fty = self.parse_fn_ty()?;
                TyExpr::Fn(fty)
            }
            TokenKind::OpenBracket => {
                let inner = self.parse_ty()?;
                self.eat(TokenKind::CloseBracket)?;
                let span = tok.span.merge(inner.span());
                TyExpr::Slice(Box::new(inner), span)
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
            TokenKind::Ident(..) => self.parse_ty_path(tok.word())?,
            TokenKind::Underscore => TyExpr::Hole(tok.span),
            _ => return Err(errors::unexpected_token_err("a type", tok.kind, tok.span)),
        };

        Ok(ty)
    }

    fn parse_ty_path(&mut self, word: Word) -> DiagnosticResult<TyExpr> {
        let start_span = word.span();
        let mut path = vec![word];

        while self.is(TokenKind::Dot) {
            path.push(self.eat_ident()?.word());
        }

        let targs = self.parse_optional_ty_args()?;
        let span = start_span.merge(self.last_span());

        Ok(TyExpr::Path(path, targs, span))
    }

    fn parse_fn_ty(&mut self) -> DiagnosticResult<TyExprFn> {
        let start = self.last_span();
        let (is_extern, callconv) = if self.is(TokenKind::Extern) {
            let callconv = self.parse_callconv()?;
            (true, callconv)
        } else {
            (false, CallConv::default())
        };
        let (params, is_c_variadic) = self.parse_fn_ty_params()?;
        let ret = self.parse_ty()?;

        Ok(TyExprFn {
            params,
            ret: Box::new(ret),
            is_extern,
            is_c_variadic,
            callconv,
            span: start.merge(self.last_span()),
        })
    }

    fn parse_fn_ty_params(&mut self) -> DiagnosticResult<(Vec<TyExpr>, bool)> {
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

    pub(super) fn parse_optional_ty_args(&mut self) -> DiagnosticResult<Option<Vec<TyExpr>>> {
        if !self.peek(|t| {
            t.kind_is(TokenKind::OpenBracket)
                && self.spans_are_on_same_line(self.last_span(), t.span)
        }) {
            return Ok(None);
        }

        let args = self
            .parse_list(TokenKind::OpenBracket, TokenKind::CloseBracket, |this| {
                this.parse_ty().map(ControlFlow::Continue)
            })
            .map(|(t, _)| t)?;

        Ok(Some(args))
    }
}
