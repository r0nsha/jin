use camino::Utf8PathBuf;
use ustr::Ustr;

use crate::{
    ast::{token::TokenKind, Attr, Import},
    diagnostics::{Diagnostic, Label},
    parse::parser::{ParseResult, Parser},
    qpath::QPath,
    span::{Span, Spanned},
    word::Word,
};

impl<'a> Parser<'a> {
    pub fn parse_import(&mut self, attrs: &[Attr], start: Span) -> ParseResult<Import> {
        let root = self.eat(TokenKind::empty_ident())?.word();
        let (qpath, span) = self.parse_import_qpath(root)?;
        let path = self.search_import_path(&qpath, span)?;

        self.imported_module_paths.insert(path.clone());

        Ok(Import { attrs: attrs.to_owned(), path, qpath, span: start.merge(self.last_span()) })
    }

    // fn parse_import_node(&mut self) -> ParseResult<ImportNode> {
    //     if self.is(TokenKind::Star) {
    //         Ok(ImportNode::Glob(self.last_span()))
    //     } else {
    //         let name = self.parse_import_name()?;
    //         Ok(ImportNode::Name(name))
    //     }
    // }

    // fn parse_import_name(&mut self) -> ParseResult<ImportName> {
    //     let word = self.eat(TokenKind::empty_ident())?.word();
    //
    //     let (alias, vis, import_path) = if self.is(TokenKind::As) {
    //         let alias = self.eat(TokenKind::empty_ident())?.word();
    //         let vis = self.parse_vis();
    //         (Some(alias), vis, ImportPath::None)
    //     } else {
    //         let vis = self.parse_vis();
    //         (None, vis, self.parse_import_path()?)
    //     };
    //
    //     Ok(ImportName { word, vis, alias, import_path })
    // }

    fn parse_import_qpath(&mut self, root: Word) -> ParseResult<(QPath, Span)> {
        let start = root.span();
        let mut qpath = QPath::from(root);

        while self.is(TokenKind::Dot) {
            let seg = self.eat(TokenKind::empty_ident())?.str_value();
            qpath.push(seg);
        }

        Ok((qpath, start.merge(self.last_span())))
    }

    // fn parse_import_group(&mut self) -> ParseResult<Vec<ImportNode>> {
    //     self.parse_list(TokenKind::OpenCurly, TokenKind::CloseCurly, |this| {
    //         this.parse_import_node()
    //     })
    //     .map(|(l, _)| l)
    // }

    fn search_import_path(&self, qpath: &QPath, span: Span) -> ParseResult<Utf8PathBuf> {
        let mut search_notes = vec![];

        let path = self.search_package_root(qpath.root(), &mut search_notes);

        if let Some(mut path) = path {
            path.extend(qpath.iter().map(Ustr::as_str));
            path.set_extension("jin");
            if path.exists() {
                return Ok(path);
            }
        }

        Err(Diagnostic::error()
            .with_message(format!("could not find module or library `{qpath}`"))
            .with_label(Label::primary(span))
            .with_notes(search_notes))
    }

    fn search_package_root(
        &self,
        name: Ustr,
        search_notes: &mut Vec<String>,
    ) -> Option<Utf8PathBuf> {
        if let Some(pkg) = self.db.packages.get(&name) {
            Some(pkg.root_path.clone())
        } else {
            search_notes.push(format!("searched package: {name}"));
            None
        }
    }
}
