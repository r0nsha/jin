use std::ops::ControlFlow;

use crate::{
    ast::{
        token::TokenKind, Attrs, StructTyDef, StructTyField, TyDef, TyDefKind, UnionTyDef,
        UnionVariant, UnionVariantField,
    },
    db::{StructKind, UnionKind},
    diagnostics::DiagnosticResult,
    parse::{errors, parser::Parser},
};

impl<'a> Parser<'a> {
    pub(super) fn parse_tydef(&mut self, attrs: Attrs) -> DiagnosticResult<TyDef> {
        let start = self.last_span();

        let ident = self.eat_ident()?;
        let vis = self.parse_vis();

        let ty_params = self.parse_optional_ty_params()?;

        let kind = self.parse_tydef_kind()?;
        let span = start.merge(self.last_span());

        Ok(TyDef { attrs, word: ident.word(), vis, ty_params, kind, span })
    }

    fn parse_tydef_kind(&mut self) -> DiagnosticResult<TyDefKind> {
        if self.peek_is(TokenKind::OpenParen) {
            self.parse_tydef_struct(StructKind::Ref)
        } else if self.peek_is(TokenKind::OpenCurly) {
            self.parse_tydef_union(UnionKind::Value)
        } else if self.is(TokenKind::Ref) {
            self.parse_tydef_union(UnionKind::Ref)
        } else if self.is_specific_ident("value") {
            self.parse_tydef_struct(StructKind::Value)
        } else {
            let tok = self.require()?;
            Err(errors::unexpected_token_err("(, {, `ref` or `value`", tok.kind, tok.span))
        }
    }

    fn parse_tydef_struct(&mut self, kind: StructKind) -> DiagnosticResult<TyDefKind> {
        let (fields, _) = self.parse_list(TokenKind::OpenParen, TokenKind::CloseParen, |this| {
            let ident = this.eat_ident()?;
            let vis = this.parse_vis();
            this.eat(TokenKind::Colon)?;
            let ty_expr = this.parse_ty()?;
            Ok(ControlFlow::Continue(StructTyField { name: ident.word(), vis, ty_expr }))
        })?;

        Ok(TyDefKind::Struct(StructTyDef { kind, fields }))
    }

    fn parse_tydef_union(&mut self, kind: UnionKind) -> DiagnosticResult<TyDefKind> {
        let (variants, _) =
            self.parse_list(TokenKind::OpenCurly, TokenKind::CloseCurly, |this| {
                let ident = this.eat_ident()?;

                let fields = if this.peek_is(TokenKind::OpenParen) {
                    let (fields, _) =
                        this.parse_list(TokenKind::OpenParen, TokenKind::CloseParen, |this| {
                            let ident = this.eat_ident()?;
                            this.eat(TokenKind::Colon)?;
                            let ty_expr = this.parse_ty()?;
                            Ok(ControlFlow::Continue(UnionVariantField {
                                name: ident.word(),
                                ty_expr,
                            }))
                        })?;

                    fields
                } else {
                    vec![]
                };

                Ok(ControlFlow::Continue(UnionVariant { name: ident.word(), fields }))
            })?;

        Ok(TyDefKind::Union(UnionTyDef { kind, variants }))
    }
}
