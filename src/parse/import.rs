use camino::{Utf8Path, Utf8PathBuf};
use path_absolutize::Absolutize as _;

use crate::{
    ast::{token::TokenKind, Attr, Import, ImportName, ImportNode},
    diagnostics::{Diagnostic, Label},
    parse::parser::{ParseResult, Parser},
    span::{Span, Spanned},
    word::Word,
};

impl<'a> Parser<'a> {
    pub fn parse_import(&mut self, attrs: &[Attr], start: Span) -> ParseResult<Import> {
        let root = self.parse_import_name()?;

        let path = self.search_import_path(root.word)?;
        self.imported_module_paths.insert(path.clone());

        Ok(Import { attrs: attrs.to_owned(), path, root, span: start.merge(self.last_span()) })
    }

    fn parse_import_name(&mut self) -> ParseResult<ImportName> {
        let word = self.eat_ident()?.word();

        let (alias, vis, node) = if self.is(TokenKind::As) {
            let alias = self.eat_ident()?.word();
            let vis = self.parse_vis();
            (Some(alias), vis, None)
        } else {
            let vis = self.parse_vis();
            let node = if self.is(TokenKind::Dot) { Some(self.parse_import_node()?) } else { None };
            (None, vis, node)
        };

        Ok(ImportName { word, vis, alias, node })
    }

    fn parse_import_node(&mut self) -> ParseResult<ImportNode> {
        // if self.is(TokenKind::Star) {
        //     Ok(ImportNode::Glob(self.last_span()))
        // } else
        if self.peek_is(TokenKind::OpenCurly) {
            self.parse_import_group()
        } else {
            let name = self.parse_import_name()?;
            Ok(ImportNode::Name(Box::new(name)))
        }
    }

    fn parse_import_group(&mut self) -> ParseResult<ImportNode> {
        self.parse_list(TokenKind::OpenCurly, TokenKind::CloseCurly, Self::parse_import_node)
            .map(|(nodes, _)| ImportNode::Group(nodes))
    }

    fn search_import_path(&self, name: Word) -> ParseResult<Utf8PathBuf> {
        let mut search_notes = vec![];

        let path = self
            .search_module_in_subdir(name, &mut search_notes)
            .or_else(|| self.search_package(name, &mut search_notes));

        path.ok_or_else(|| {
            Diagnostic::error()
                .with_message(format!("could not find module or library `{name}`"))
                .with_label(Label::primary(name.span()))
                .with_notes(search_notes)
        })
    }

    fn search_module_in_subdir(
        &self,
        name: Word,
        search_notes: &mut Vec<String>,
    ) -> Option<Utf8PathBuf> {
        let path = Utf8Path::new(&name.name()).with_extension("jin");
        let relative_to = self.source.path().with_extension("");

        let absolute_path: Utf8PathBuf = path
            .as_std_path()
            .absolutize_from(relative_to.as_std_path())
            .ok()?
            .to_path_buf()
            .try_into()
            .expect("path to be utf8");

        search_notes.push(format!("searched path: {absolute_path}"));

        absolute_path.exists().then_some(absolute_path)
    }

    fn search_package(&self, name: Word, search_notes: &mut Vec<String>) -> Option<Utf8PathBuf> {
        if let Some(pkg) = self.db.packages.get(&name.name()) {
            let sources = self.db.sources.borrow();
            let source = sources.get(pkg.main_source_id).unwrap();
            Some(source.path().to_path_buf())
        } else {
            search_notes.push(format!("searched package: {name}"));
            None
        }
    }
}
