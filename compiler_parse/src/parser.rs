mod expr;
mod import;
mod item;
mod pmatch;
mod tydef;
mod tyexpr;

use std::ops::ControlFlow;

use camino::{Utf8Path, Utf8PathBuf};
use compiler_ast::{Items, TyParam};
use compiler_core::{
    db::Db,
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::{Mutability, Vis},
    span::{Source, Span},
    word::Word,
};
use compiler_data_structures::index_vec::IndexVec;
use compiler_helpers::create_bool_enum;
use rustc_hash::FxHashSet;
use ustr::ustr;

use crate::{
    errors,
    token::{Kw, Token, TokenKind},
};

const SEMI: TokenKind = TokenKind::Semi(false);

const WEAK_KW_REC: &str = "rec";

pub fn parse(
    db: &Db,
    source: &Source,
    is_package_main: bool,
    tokens: Vec<Token>,
) -> DiagnosticResult<(Items, FxHashSet<Utf8PathBuf>)> {
    let mut parser = Parser::new(db, source, tokens, is_package_main);
    let items = parser.parse()?;
    Ok((items, parser.submodule_paths))
}

#[derive(Debug)]
pub(super) struct Parser<'a> {
    pub(super) db: &'a Db,
    pub(super) source: &'a Source,
    pub(super) tokens: Vec<Token>,
    pub(super) is_package_root: bool,
    pub(super) pos: usize,
    pub(super) submodule_paths: FxHashSet<Utf8PathBuf>,
}

impl<'a> Parser<'a> {
    fn new(db: &'a Db, source: &'a Source, tokens: Vec<Token>, is_package_root: bool) -> Self {
        Self { db, source, tokens, is_package_root, pos: 0, submodule_paths: FxHashSet::default() }
    }

    fn parse(&mut self) -> DiagnosticResult<Items> {
        let mut items = IndexVec::new();

        while !self.eof() {
            let item = self.parse_item()?;
            self.eat_semi()?;
            items.push(item);
        }

        Ok(items)
    }

    pub(super) fn parse_mutability(&mut self) -> Mutability {
        self.parse_optional_mutability().unwrap_or_default()
    }

    pub(super) fn parse_optional_mutability(&mut self) -> Option<Mutability> {
        if self.is_kw(Kw::Mut) {
            Some(Mutability::Mut)
        } else if self.is_kw(Kw::Imm) {
            Some(Mutability::Imm)
        } else {
            None
        }
    }

    pub(super) fn parse_vis(&mut self) -> Vis {
        if self.is(TokenKind::Star) {
            Vis::Public
        } else {
            Vis::Private
        }
    }

    fn parse_optional_tparams(&mut self) -> DiagnosticResult<Vec<TyParam>> {
        self.parse_list_optional(TokenKind::OpenBrack, TokenKind::CloseBrack, |this| {
            let ident = this.eat_ident()?;
            Ok(ControlFlow::Continue(TyParam { word: ident.word() }))
        })
        .map(|(t, _)| t)
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

    pub(super) fn parse_list<T>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        f: impl FnMut(&mut Self) -> DiagnosticResult<ControlFlow<(), T>>,
    ) -> DiagnosticResult<(Vec<T>, Span)> {
        self.parse_list_with_sep(open, close, TokenKind::Comma, f)
    }

    pub(super) fn parse_list_with_sep<T>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        sep: TokenKind,
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
                self.eat(sep)?;
            } else if self.peek_is(sep) {
                self.next();
            }
        }

        Ok((values, open_tok.span.merge(self.last_span())))
    }

    #[inline]
    pub(super) fn eat_semi(&mut self) -> DiagnosticResult<()> {
        self.eat(SEMI)?;
        self.skip_semi();
        Ok(())
    }

    #[inline]
    pub(super) fn skip_semi(&mut self) {
        while self.is(SEMI) {}
    }

    #[inline]
    pub(super) fn eat_ident(&mut self) -> DiagnosticResult<Token> {
        self.eat(TokenKind::empty_ident())
    }

    #[inline]
    pub(super) fn eat_str_lit(&mut self) -> DiagnosticResult<Word> {
        let open = self.eat(TokenKind::StrOpen)?.span;
        let lit = self.eat(TokenKind::StrText(ustr("")))?.str_value();
        let close = self.eat(TokenKind::StrClose)?.span;
        Ok(Word::new(lit, open.merge(close)))
    }

    #[inline]
    pub(super) fn eat(&mut self, expected: TokenKind) -> DiagnosticResult<Token> {
        self.token()
            .ok_or_else(|| {
                Diagnostic::error(format!("expected {expected}, found end of file"))
                    .with_label(Label::primary(self.last_span(), "here"))
            })
            .and_then(|tok| {
                self.next();
                Self::require_kind(tok, expected)
            })
    }

    #[inline]
    pub(super) fn eat_any(&mut self, expected: &str) -> DiagnosticResult<Token> {
        let tok = self.require(expected)?;
        self.next();
        Ok(tok)
    }

    #[inline]
    pub(super) fn unexpected_token(&mut self, expected: &str) -> Diagnostic {
        match self.require(expected) {
            Ok(tok) => errors::unexpected_token_err(expected, tok.kind, tok.span),
            Err(diag) => diag,
        }
    }

    #[inline]
    fn require(&mut self, expected: &str) -> DiagnosticResult<Token> {
        self.token().ok_or_else(|| {
            Diagnostic::error(format!("expected {expected}, found end of file"))
                .with_label(Label::primary(self.last_span(), "found here"))
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
    pub(super) fn skip(&mut self, expected: TokenKind) -> bool {
        self.is(expected)
    }

    #[inline]
    pub(super) fn is(&mut self, expected: TokenKind) -> bool {
        self.is_predicate(|_, tok| tok.kind_is(expected))
    }

    #[inline]
    pub(super) fn is_kw(&mut self, expected: Kw) -> bool {
        self.is_predicate(|_, tok| matches!(tok.kind, TokenKind::Kw(kw) if kw == expected))
    }

    #[inline]
    pub(super) fn is_ident(&mut self) -> bool {
        self.is(TokenKind::empty_ident())
    }

    #[inline]
    pub(super) fn is_weak_kw(&mut self, kw: &str) -> bool {
        self.is_predicate(|_, tok| matches!(tok.kind, TokenKind::Ident(id) if id == kw))
    }

    #[inline]
    pub(super) fn is_predicate(&mut self, mut f: impl FnMut(&mut Self, Token) -> bool) -> bool {
        match self.token() {
            Some(tok) if f(self, tok) => {
                self.next();
                true
            }
            _ => false,
        }
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
    pub(super) fn back(&mut self) {
        self.pos -= 1;
    }

    #[inline]
    pub(super) fn eof(&self) -> bool {
        self.pos == self.tokens.len()
    }

    #[inline]
    pub(super) fn require_kind(tok: Token, expected: TokenKind) -> DiagnosticResult<Token> {
        if tok.kind_is(expected) {
            Ok(tok)
        } else {
            Err(errors::unexpected_token_err(&expected.to_string(), tok.kind, tok.span))
        }
    }

    #[inline]
    pub(super) fn parent_path(&self) -> Option<&Utf8Path> {
        self.source.path().parent()
    }
}

create_bool_enum!(RequireSigTy);
