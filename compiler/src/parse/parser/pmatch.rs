use std::ops::ControlFlow;

use ustr::ustr;

use crate::{
    ast::{Expr, MatchArm, MatchPat, MatchPatAdt, Subpat},
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::Mutability,
    parse::{parser::Parser, token::TokenKind},
    span::Spanned,
    word::Word,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_match(&mut self) -> DiagnosticResult<Expr> {
        let start = self.last_span();
        let expr = self.parse_expr()?;

        let (arms, _) = self.parse_list_with_sep(
            TokenKind::OpenCurly,
            TokenKind::CloseCurly,
            TokenKind::Semi(false),
            |this| this.parse_match_arm().map(ControlFlow::Continue),
        )?;

        let span = start.merge(self.last_span());

        Ok(Expr::Match { expr: Box::new(expr), arms, span })
    }

    fn parse_match_arm(&mut self) -> DiagnosticResult<MatchArm> {
        let pat = self.parse_match_pat()?;

        let guard =
            self.is_and(TokenKind::If, |this, _| this.parse_expr()).transpose()?.map(Box::new);

        self.eat(TokenKind::Arrow)?;
        let expr = self.parse_expr()?;

        Ok(MatchArm { pat, guard, expr: Box::new(expr) })
    }

    fn parse_match_pat(&mut self) -> DiagnosticResult<MatchPat> {
        self.skip(TokenKind::Pipe);

        let pat = self.parse_match_pat_atom()?;

        if self.peek_is(TokenKind::Pipe) {
            let start_span = pat.span();
            let mut pats = vec![pat];

            while self.is(TokenKind::Pipe) {
                pats.push(self.parse_match_pat_atom()?);
            }

            Ok(MatchPat::Or(pats, start_span.merge(self.last_span())))
        } else {
            Ok(pat)
        }
    }

    fn parse_match_pat_atom(&mut self) -> DiagnosticResult<MatchPat> {
        if let Some(mutability) = self.parse_optional_mutability() {
            let word = self.eat_ident()?.word();
            Ok(MatchPat::Name(word, mutability))
        } else if self.is(TokenKind::Dot) {
            let word = self.eat_ident()?.word();
            self.parse_match_pat_inferred_variant(word)
        } else if self.is_ident() {
            let start_word = self.last_token().word();

            if self.peek_is(TokenKind::OpenParen) {
                self.parse_match_pat_adt_name(start_word)
            } else if self.is(TokenKind::Dot) {
                self.parse_match_pat_adt_path(start_word)
            } else {
                Ok(MatchPat::Name(start_word, Mutability::Imm))
            }
        } else if self.is(TokenKind::Underscore) {
            Ok(MatchPat::Wildcard(self.last_span()))
        } else if self.is(TokenKind::OpenCurly) {
            let start_span = self.last_span();
            let last_span = self.eat(TokenKind::CloseCurly)?.span;
            Ok(MatchPat::Unit(start_span.merge(last_span)))
        } else if self.is(TokenKind::Minus) {
            let start_span = self.last_span();
            let tok = self.eat(TokenKind::Int(ustr("")))?;
            let value: i128 = Self::int_lit(&tok.str_value());
            Ok(MatchPat::Int(-value, start_span.merge(tok.span)))
        } else if self.is(TokenKind::Int(ustr(""))) {
            let tok = self.last_token();
            let value = Self::int_lit(&tok.str_value());
            Ok(MatchPat::Int(value, tok.span))
        } else if self.is(TokenKind::empty_str()) {
            let tok = self.last_token();
            Ok(MatchPat::Str(tok.str_value(), tok.span))
        } else if self.is(TokenKind::True) {
            Ok(MatchPat::Bool(true, self.last_span()))
        } else if self.is(TokenKind::False) {
            Ok(MatchPat::Bool(false, self.last_span()))
        } else {
            Err(self.unexpected_token("a pattern"))
        }
    }

    fn parse_match_pat_inferred_variant(&mut self, word: Word) -> DiagnosticResult<MatchPat> {
        let path = vec![word];
        let (subpats, is_exhaustive) = self.parse_match_adt_subpats_optional()?;
        let span = word.span().merge(self.last_span());
        Ok(MatchPat::Adt(MatchPatAdt {
            path,
            subpats,
            is_exhaustive,
            is_inferred_variant: true,
            span,
        }))
    }

    fn parse_match_pat_adt_name(&mut self, word: Word) -> DiagnosticResult<MatchPat> {
        let path = vec![word];
        let (subpats, is_exhaustive) = self.parse_match_adt_subpats()?;
        let span = word.span().merge(self.last_span());
        Ok(MatchPat::Adt(MatchPatAdt {
            path,
            subpats,
            is_exhaustive,
            is_inferred_variant: false,
            span,
        }))
    }

    fn parse_match_pat_adt_path(&mut self, start_word: Word) -> DiagnosticResult<MatchPat> {
        let mut path = vec![start_word];

        path.push(self.eat_ident()?.word());
        while self.is(TokenKind::Dot) {
            path.push(self.eat_ident()?.word());
        }

        let (subpats, is_exhaustive) = self.parse_match_adt_subpats_optional()?;
        let span = start_word.span().merge(self.last_span());

        Ok(MatchPat::Adt(MatchPatAdt {
            path,
            subpats,
            is_exhaustive,
            is_inferred_variant: false,
            span,
        }))
    }

    fn parse_match_adt_subpats_optional(&mut self) -> DiagnosticResult<(Vec<Subpat>, bool)> {
        if self.peek_is(TokenKind::OpenParen) {
            self.parse_match_adt_subpats()
        } else {
            Ok((vec![], true))
        }
    }

    fn parse_match_adt_subpats(&mut self) -> DiagnosticResult<(Vec<Subpat>, bool)> {
        let mut is_exhaustive = true;
        let mut passed_named_pat = false;

        let (subpats, _) =
            self.parse_list(TokenKind::OpenParen, TokenKind::CloseParen, |this| {
                if this.is(TokenKind::DotDot) {
                    is_exhaustive = false;
                    return Ok(ControlFlow::Break(()));
                }

                let subpat = this.parse_match_adt_subpat()?;

                match subpat {
                    Subpat::Positional(_) if passed_named_pat => {
                        return Err(Diagnostic::error(
                            "positional patterns are not allowed after named patterns",
                        )
                        .with_label(Label::primary(
                            subpat.span(),
                            "unexpected positional pattern",
                        )));
                    }
                    Subpat::Positional(_) => (),
                    Subpat::Named(_, _) => passed_named_pat = true,
                }

                Ok(ControlFlow::Continue(subpat))
            })?;

        Ok((subpats, is_exhaustive))
    }

    fn parse_match_adt_subpat(&mut self) -> DiagnosticResult<Subpat> {
        if self.is_ident() {
            let name = self.last_token().word();

            if self.is(TokenKind::Colon) {
                let pat = self.parse_match_pat()?;
                return Ok(Subpat::Named(name, pat));
            }

            self.back();
        }

        let pat = self.parse_match_pat()?;
        Ok(Subpat::Positional(pat))
    }
}
