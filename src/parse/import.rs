use camino::{Utf8Path, Utf8PathBuf};
use path_absolutize::Absolutize;

use crate::{
    ast::{token::TokenKind, Attr, Import, ImportName, ImportNode, ImportPath},
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

        if path == self.source.path() {
            return Err(Diagnostic::error()
                .with_message(format!("module `{}` cannot import itself", root.word))
                .with_label(Label::primary(root.word.span()).with_message("here")));
        }

        Ok(Import { attrs: attrs.to_owned(), path, root, span: start.merge(self.last_span()) })
    }

    fn parse_import_node(&mut self) -> ParseResult<ImportNode> {
        if self.is(TokenKind::Star) {
            Ok(ImportNode::Glob(self.last_span()))
        } else {
            let name = self.parse_import_name()?;
            Ok(ImportNode::Name(name))
        }
    }

    fn parse_import_name(&mut self) -> ParseResult<ImportName> {
        let word = self.eat(TokenKind::empty_ident())?.word();

        let (alias, vis, import_path) = if self.is(TokenKind::As) {
            let alias = self.eat(TokenKind::empty_ident())?.word();
            let vis = self.parse_vis();
            (Some(alias), vis, ImportPath::None)
        } else {
            let vis = self.parse_vis();
            (None, vis, self.parse_import_path()?)
        };

        Ok(ImportName { word, vis, alias, import_path })
    }

    fn parse_import_path(&mut self) -> ParseResult<ImportPath> {
        if self.is(TokenKind::Dot) {
            if self.peek_is(TokenKind::OpenCurly) {
                let group = self.parse_import_group()?;
                Ok(ImportPath::Group(group))
            } else {
                let node = self.parse_import_node()?;
                Ok(ImportPath::Node(Box::new(node)))
            }
        } else {
            Ok(ImportPath::None)
        }
    }

    fn parse_import_group(&mut self) -> ParseResult<Vec<ImportNode>> {
        self.parse_list(TokenKind::OpenCurly, TokenKind::CloseCurly, |this| {
            this.parse_import_node()
        })
        .map(|(l, _)| l)
    }

    fn search_import_path(&self, name: Word) -> ParseResult<Utf8PathBuf> {
        let mut search_notes = vec![];

        let path = self
            .search_module_in_subdir(name, &mut search_notes)
            .or_else(|| self.search_module_in_curr_dir(name, &mut search_notes))
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

    fn search_module_in_curr_dir(
        &self,
        name: Word,
        search_notes: &mut Vec<String>,
    ) -> Option<Utf8PathBuf> {
        let path = Utf8Path::new(&name.name()).with_extension("jin");
        let relative_to = self.parent_path().unwrap();

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
