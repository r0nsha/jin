use std::ops::ControlFlow;

use compiler_ast::{Expr, MatchArm, MatchPat, MatchPatAdt, MatchSubpat};
use compiler_core::{
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::Mutability,
    span::Spanned,
    word::Word,
};

use crate::{
    parser::Parser,
    token::{Kw, TokenKind},
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

        let guard = self
            .is_and(TokenKind::Kw(Kw::If), |this, _| this.parse_expr())
            .transpose()?
            .map(Box::new);

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
        let tok = self.eat_any("a pattern")?;

        match tok.kind {
            TokenKind::Ident(id) => {
                let word = Word::new(id, tok.span);

                if self.peek_is(TokenKind::OpenParen) {
                    self.parse_match_pat_adt_name(word)
                } else if self.is(TokenKind::Dot) {
                    self.parse_match_pat_adt_path(word)
                } else {
                    Ok(MatchPat::Name(word, Mutability::Imm))
                }
            }
            TokenKind::Underscore => Ok(MatchPat::Wildcard(tok.span)),
            TokenKind::OpenCurly => {
                let last_span = self.eat(TokenKind::CloseCurly)?.span;
                Ok(MatchPat::Unit(tok.span.merge(last_span)))
            }
            TokenKind::Minus => {
                let int_tok = self.eat(TokenKind::Int(0))?;
                Ok(MatchPat::Int(-int_tok.int_value(), tok.span.merge(int_tok.span)))
            }
            TokenKind::Int(value) => Ok(MatchPat::Int(value, tok.span)),
            TokenKind::StrOpen => {
                self.back();
                let s = self.eat_str_lit()?;
                Ok(MatchPat::Str(s.name(), s.span()))
            }
            TokenKind::Kw(Kw::True) => Ok(MatchPat::Bool(true, tok.span)),
            TokenKind::Kw(Kw::False) => Ok(MatchPat::Bool(false, tok.span)),
            _ => {
                self.back();

                if let Some(mutability) = self.parse_optional_mutability() {
                    let word = self.eat_ident()?.word();
                    Ok(MatchPat::Name(word, mutability))
                } else {
                    Err(self.unexpected_token("a pattern"))
                }
            }
        }
    }

    fn parse_match_pat_adt_name(&mut self, word: Word) -> DiagnosticResult<MatchPat> {
        let path = vec![word];
        let (subpats, is_exhaustive) = self.parse_match_adt_subpats()?;
        let span = word.span().merge(self.last_span());
        Ok(MatchPat::Adt(MatchPatAdt { path, subpats, is_exhaustive, span }))
    }

    fn parse_match_pat_adt_path(&mut self, start_word: Word) -> DiagnosticResult<MatchPat> {
        let mut path = vec![start_word];

        path.push(self.eat_ident()?.word());
        while self.is(TokenKind::Dot) {
            path.push(self.eat_ident()?.word());
        }

        let (subpats, is_exhaustive) = self.parse_match_adt_subpats()?;
        let span = start_word.span().merge(self.last_span());

        Ok(MatchPat::Adt(MatchPatAdt { path, subpats, is_exhaustive, span }))
    }

    fn parse_match_adt_subpats(&mut self) -> DiagnosticResult<(Option<Vec<MatchSubpat>>, bool)> {
        if !self.peek_is(TokenKind::OpenParen) {
            return Ok((None, true));
        }

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
                    MatchSubpat::Positional(_) if passed_named_pat => {
                        return Err(Diagnostic::error(
                            "positional patterns are not allowed after named patterns",
                        )
                        .with_label(Label::primary(
                            subpat.span(),
                            "unexpected positional pattern",
                        )));
                    }
                    MatchSubpat::Positional(_) => (),
                    MatchSubpat::Named(_, _) => passed_named_pat = true,
                }

                Ok(ControlFlow::Continue(subpat))
            })?;

        Ok((Some(subpats), is_exhaustive))
    }

    fn parse_match_adt_subpat(&mut self) -> DiagnosticResult<MatchSubpat> {
        if self.is_ident() {
            let name = self.last_token().word();

            if self.is(TokenKind::Colon) {
                let pat = self.parse_match_pat()?;
                return Ok(MatchSubpat::Named(name, pat));
            }

            self.back();
        }

        let pat = self.parse_match_pat()?;
        Ok(MatchSubpat::Positional(pat))
    }
}
