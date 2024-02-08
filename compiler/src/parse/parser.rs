mod expr;
mod import;
mod item;
mod tydef;
mod tyexpr;

use std::{ops::ControlFlow, str::FromStr};

use camino::{Utf8Path, Utf8PathBuf};
use codespan_reporting::files::Files;
use rustc_hash::FxHashSet;
use ustr::Ustr;

use crate::{
    ast::{Item, Module, TyParam},
    db::Db,
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    macros::create_bool_enum,
    middle::{Mutability, Vis},
    parse::{
        errors,
        token::{Token, TokenKind},
    },
    qpath::QPath,
    span::{Source, SourceId, Span},
};

pub fn parse(
    db: &Db,
    package: Ustr,
    source: &Source,
    tokens: Vec<Token>,
) -> DiagnosticResult<(Module, FxHashSet<Utf8PathBuf>)> {
    let name = QPath::from_path(&db.package(package).root_path, source.path()).unwrap();

    let mut parser = Parser::new(db, source, tokens);
    let is_main = db.is_main_package(package) && source.id() == db.package(package).main_source_id;
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
        Self { db, source, tokens, pos: 0, imported_module_paths: FxHashSet::default() }
    }

    fn parse(
        &mut self,
        source_id: SourceId,
        name: QPath,
        is_main: bool,
    ) -> DiagnosticResult<Module> {
        let mut module = Module::new(source_id, name, is_main);

        while !self.eof() {
            let item = self.parse_item()?;
            module.items.push(item);
        }

        Ok(module)
    }

    fn parse_item(&mut self) -> DiagnosticResult<Item> {
        if let Some(item) = self.maybe_parse_item()? {
            Ok(item)
        } else {
            Err(self.unexpected_token("an item"))
        }
    }

    pub(super) fn parse_mutability(&mut self) -> Mutability {
        self.parse_optional_mutability().unwrap_or_default()
    }

    pub(super) fn parse_optional_mutability(&mut self) -> Option<Mutability> {
        if self.is(TokenKind::Mut) {
            Some(Mutability::Mut)
        } else if self.is(TokenKind::Imm) {
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

    fn parse_optional_ty_params(&mut self) -> DiagnosticResult<Vec<TyParam>> {
        self.parse_list_optional(TokenKind::OpenBracket, TokenKind::CloseBracket, |this| {
            let ident = this.eat_ident()?;
            Ok(ControlFlow::Continue(TyParam { word: ident.word() }))
        })
        .map(|(t, _)| t)
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

    #[inline]
    pub(super) fn eat(&mut self, expected: TokenKind) -> DiagnosticResult<Token> {
        let tok = self.eat_any()?;
        Self::require_kind(tok, expected)
    }

    #[inline]
    pub(super) fn eat_ident(&mut self) -> DiagnosticResult<Token> {
        self.eat(TokenKind::empty_ident())
    }

    pub(super) fn int_lit<F>(value: &str) -> F
    where
        F: FromStr,
        <F as FromStr>::Err: core::fmt::Debug,
    {
        value.replace('_', "").parse().expect("to be a valid integer")
    }

    #[inline]
    pub(super) fn eat_any(&mut self) -> DiagnosticResult<Token> {
        let tok = self.require()?;
        self.next();
        Ok(tok)
    }

    #[inline]
    pub(super) fn unexpected_token(&mut self, expected: &str) -> Diagnostic {
        match self.require() {
            Ok(tok) => errors::unexpected_token_err(expected, tok.kind, tok.span),
            Err(diag) => diag,
        }
    }

    #[inline]
    fn require(&mut self) -> DiagnosticResult<Token> {
        self.token().ok_or_else(|| {
            Diagnostic::error("unexpected end of file")
                .with_label(Label::primary(self.last_span(), "here"))
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
    pub(super) fn is_ident(&mut self) -> bool {
        self.is(TokenKind::empty_ident())
    }

    #[allow(unused)]
    #[inline]
    pub(super) fn is_specific_ident(&mut self, s: &str) -> bool {
        self.is_predicate(|_, tok| matches!(tok.kind, TokenKind::Ident(ident) if ident == s))
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

create_bool_enum!(AllowOmitParens);
create_bool_enum!(RequireSigTy);
