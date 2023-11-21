use camino::Utf8PathBuf;
use ustr::Ustr;

use crate::{
    ast::{token::TokenKind, Attr, Import, ImportName, ImportSymbol},
    diagnostics::{Diagnostic, Label},
    parse::parser::{ParseResult, Parser},
    qpath::QPath,
    span::{Span, Spanned},
    word::Word,
};

impl<'a> Parser<'a> {
    pub fn parse_import(&mut self, attrs: &[Attr], start: Span) -> ParseResult<Import> {
        let root = self.eat_ident()?.word();
        let (qpath, path_span) = self.parse_import_qpath(root)?;
        let path = self.search_import_path(&qpath, path_span)?;

        let (alias, symbols) = if self.is(TokenKind::As) {
            let alias = self.eat_ident()?.word();
            (Some(alias), None)
        } else if self.peek_is(TokenKind::OpenCurly) {
            let symbols = self.parse_import_symbols()?;
            (None, Some(symbols))
        } else {
            (None, None)
        };

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

    fn parse_import_qpath(&mut self, root: Word) -> ParseResult<(QPath, Span)> {
        let start = root.span();
        let mut qpath = QPath::from(root);

        while self.is(TokenKind::Dot) && !self.peek_is(TokenKind::OpenCurly) {
            let seg = self.eat_ident()?.str_value();
            qpath.push(seg);
        }

        Ok((qpath, start.merge(self.last_span())))
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
