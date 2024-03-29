use std::ops::ControlFlow;
use std::rc::Rc;

use compiler_ast::{
    AliasTyDef, Attrs, StructTyDef, StructTyField, TyDef, TyDefKind, UnionTyDef, UnionVariant,
    UnionVariantField,
};
use compiler_core::{
    db::{StructKind, UnionKind},
    diagnostics::DiagnosticResult,
};

use crate::parser::WEAK_KW_REC;
use crate::{
    parser::Parser,
    token::{Kw, TokenKind},
};

impl<'a> Parser<'a> {
    pub(super) fn parse_tydef(&mut self, attrs: Attrs) -> DiagnosticResult<TyDef> {
        let start = self.last_span();

        let ident = self.eat_ident()?;
        let vis = self.parse_vis();
        let tparams = self.parse_optional_tparams()?;
        let kind = self.parse_tydef_kind()?;

        let span = start.merge(self.last_span());
        Ok(TyDef { attrs, word: ident.word(), vis, tparams, kind, span })
    }

    fn parse_tydef_kind(&mut self) -> DiagnosticResult<TyDefKind> {
        if self.peek_is(TokenKind::OpenParen) {
            self.parse_tydef_struct(StructKind::Ref)
        } else if self.is_kw(Kw::Extern) {
            self.parse_tydef_struct(StructKind::Value)
        } else if self.peek_is(TokenKind::OpenCurly) {
            self.parse_tydef_union(UnionKind::Value)
        } else if self.is_weak_kw(WEAK_KW_REC) {
            self.parse_tydef_union(UnionKind::Ref)
        } else if self.is(TokenKind::Eq) {
            self.parse_tydef_alias()
        } else {
            Err(self.unexpected_token("(, {, `ref` or `extern`"))
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
        self.parse_list_with_sep(
            TokenKind::OpenCurly,
            TokenKind::CloseCurly,
            TokenKind::Semi(false),
            |this| this.parse_tydef_union_variant().map(ControlFlow::Continue),
        )
        .map(|(variants, _)| TyDefKind::Union(UnionTyDef { kind, variants }))
    }

    fn parse_tydef_union_variant(&mut self) -> DiagnosticResult<UnionVariant> {
        let ident = self.eat_ident()?;

        let fields = if self.peek_is(TokenKind::OpenParen) {
            let (fields, _) =
                self.parse_list(TokenKind::OpenParen, TokenKind::CloseParen, |this| {
                    let ident = this.eat_ident()?;
                    this.eat(TokenKind::Colon)?;
                    let ty_expr = this.parse_ty()?;
                    Ok(ControlFlow::Continue(UnionVariantField { name: ident.word(), ty_expr }))
                })?;

            fields
        } else {
            vec![]
        };

        Ok(UnionVariant { name: ident.word(), fields })
    }

    fn parse_tydef_alias(&mut self) -> DiagnosticResult<TyDefKind> {
        let ty = self.parse_ty()?;
        Ok(TyDefKind::Alias(AliasTyDef { ty: Rc::new(ty) }))
    }
}
