use camino::Utf8PathBuf;
use ustr::Ustr;

use crate::{
    ast::{token::TokenKind, Attr, Import, ImportName, ImportSymbol},
    diagnostics::{Diagnostic, Label},
    parse::{
        errors,
        parser::{ParseResult, Parser},
    },
    qpath::QPath,
    span::{Span, Spanned},
};

impl<'a> Parser<'a> {
    pub fn parse_import(&mut self, attrs: &[Attr], start: Span) -> ParseResult<Import> {
        let root = self.eat_ident()?.word();

        let mut qpath = QPath::from(root);
        let mut symbols = None;
        let mut path_span = root.span();

        while self.is(TokenKind::Dot) {
            if self.is_ident() {
                let tok = self.last_token();
                qpath.push(tok.str_value());
                path_span = path_span.merge(tok.span);
            } else if self.peek_is(TokenKind::OpenCurly) {
                symbols = Some(self.parse_import_symbols()?);
                break;
            } else {
                let tok = self.require()?;
                return Err(errors::unexpected_token_err("an identifier or {", tok.kind, tok.span));
            }
        }

        let alias = if symbols.is_none() && self.is(TokenKind::As) {
            let alias = self.eat_ident()?.word();
            Some(alias)
        } else {
            None
        };

        let path = self.search_import_path(&qpath, path_span)?;
        self.imported_module_paths.insert(path.clone());

        Ok(Import {
            attrs: attrs.to_owned(),
            path,
            path_span,
            qpath,
            alias,
            symbols,
            span: start.merge(self.last_span()),
        })
    }

    fn search_import_path(&self, qpath: &QPath, span: Span) -> ParseResult<Utf8PathBuf> {
        let package_name = qpath.root();
        let path = self.search_package_root(package_name);

        if let Some(mut path) = path {
            path.extend(qpath.iter().map(Ustr::as_str));
            path.set_extension("jin");

            if path.exists() {
                Ok(path)
            } else {
                Err(Diagnostic::error()
                    .with_message(format!(
                        "could not find module `{qpath}` in package `{package_name}`"
                    ))
                    .with_label(Label::primary(span)))
            }
        } else {
            Err(Diagnostic::error()
                .with_message(format!("could not find package `{package_name}`"))
                .with_label(Label::primary(span)))
        }
    }

    fn search_package_root(&self, name: Ustr) -> Option<Utf8PathBuf> {
        self.db.packages.get(&name).map(|pkg| pkg.root_path.clone())
    }

    fn parse_import_symbols(&mut self) -> ParseResult<Vec<ImportSymbol>> {
        self.parse_list(TokenKind::OpenCurly, TokenKind::CloseCurly, Self::parse_import_node)
            .map(|(l, _)| l)
    }

    fn parse_import_node(&mut self) -> ParseResult<ImportSymbol> {
        if self.is(TokenKind::Star) {
            Ok(ImportSymbol::Glob(self.last_span()))
        } else {
            let name = self.parse_import_name()?;
            Ok(ImportSymbol::Name(name))
        }
    }

    fn parse_import_name(&mut self) -> ParseResult<ImportName> {
        let word = self.eat_ident()?.word();

        let alias = if self.is(TokenKind::As) {
            let alias = self.eat_ident()?.word();
            Some(alias)
        } else {
            None
        };

        Ok(ImportName { word, alias })
    }
}
