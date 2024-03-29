use std::ops::ControlFlow;

use camino::{Utf8Path, Utf8PathBuf};
use compiler_ast::{Attrs, ExternImport, Import, ImportTree};
use compiler_core::{
    db::ExternLib,
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::IsUfcs,
    span::{Span, Spanned},
    word::Word,
};
use path_absolutize::Absolutize as _;

use crate::{
    errors,
    parser::Parser,
    token::{Kw, TokenKind},
};

impl<'a> Parser<'a> {
    pub(super) fn parse_mod(&mut self, attrs: &Attrs) -> DiagnosticResult<Import> {
        let start = self.last_span();
        let root = self.eat_ident()?.word();

        let module_path = self.search_submodule(root)?;
        self.submodule_paths.insert(module_path.clone());

        let alias = if self.is_kw(Kw::As) { Some(self.eat_ident()?.word()) } else { None };
        let vis = self.parse_vis();
        let tree = ImportTree::Name(root, alias, vis);

        Ok(Import { attrs: attrs.clone(), module_path, tree, span: start.merge(self.last_span()) })
    }

    pub(super) fn parse_import(&mut self, attrs: &Attrs) -> DiagnosticResult<Import> {
        let start = self.last_span();
        let root = self.eat_ident()?.word();
        let module_path = self.search_package(root)?;
        let tree = self.parse_import_tree_name(root)?;
        Ok(Import { attrs: attrs.clone(), module_path, tree, span: start.merge(self.last_span()) })
    }

    fn parse_import_tree_name(&mut self, name: Word) -> DiagnosticResult<ImportTree> {
        if self.is(TokenKind::Dot) {
            let next = if self.is_ident() {
                self.parse_import_tree_name(self.last_token().word())
            } else if self.peek_is(TokenKind::OpenParen) {
                self.parse_import_path_group()
            } else {
                Err(self.unexpected_token("an identifier or {"))
            }?;

            Ok(ImportTree::Path(name, Box::new(next)))
        } else if self.is_kw(Kw::As) {
            let alias = self.eat_ident()?.word();
            let vis = self.parse_vis();
            Ok(ImportTree::Name(name, Some(alias), vis))
        } else {
            let vis = self.parse_vis();
            Ok(ImportTree::Name(name, None, vis))
        }
    }

    fn parse_import_path_group(&mut self) -> DiagnosticResult<ImportTree> {
        self.parse_list(TokenKind::OpenParen, TokenKind::CloseParen, |this| {
            this.parse_import_group_tree().map(ControlFlow::Continue)
        })
        .map(|(imports, _)| ImportTree::Group(imports))
    }

    fn parse_import_group_tree(&mut self) -> DiagnosticResult<ImportTree> {
        if self.is(TokenKind::Star) {
            let vis = self.parse_vis();
            Ok(ImportTree::Glob(IsUfcs::No, vis, self.last_span()))
        } else if self.is(TokenKind::QuestionMark) {
            let vis = self.parse_vis();
            Ok(ImportTree::Glob(IsUfcs::Yes, vis, self.last_span()))
        } else if self.is_ident() {
            self.parse_import_tree_name(self.last_token().word())
        } else {
            Err(self.unexpected_token("an identifier . * or ?"))
        }
    }

    fn search_package(&self, name: Word) -> DiagnosticResult<Utf8PathBuf> {
        if let Some(pkg) = self.db.packages.get(&name.name()) {
            let source = self.db.sources.get(pkg.main_source_id).unwrap();
            Ok(source.path().to_path_buf())
        } else {
            Err(Diagnostic::error(format!("could not find package `{name}`"))
                .with_label(Label::primary(name.span(), "not found")))
        }
    }

    fn search_submodule(&self, name: Word) -> DiagnosticResult<Utf8PathBuf> {
        let mut search_notes = vec![];

        let path = self
            .is_package_root
            .then(|| self.search_module_in_currdir(name, &mut search_notes))
            .flatten()
            .or_else(|| self.search_module_in_subdir(name, &mut search_notes));

        path.ok_or_else(|| {
            Diagnostic::error(format!("could not find module `{name}`"))
                .with_label(Label::primary(name.span(), "not found"))
                .with_notes(search_notes)
        })
    }

    fn search_module_in_currdir(
        &self,
        name: Word,
        search_notes: &mut Vec<String>,
    ) -> Option<Utf8PathBuf> {
        self.source
            .path()
            .with_extension("")
            .parent()
            .and_then(|dir| self.search_module_in_dir(dir, name, search_notes))
    }

    fn search_module_in_subdir(
        &self,
        name: Word,
        search_notes: &mut Vec<String>,
    ) -> Option<Utf8PathBuf> {
        let dir = self.source.path().with_extension("");
        self.search_module_in_dir(&dir, name, search_notes)
    }

    fn search_module_in_dir(
        &self,
        dir: &Utf8Path,
        name: Word,
        search_notes: &mut Vec<String>,
    ) -> Option<Utf8PathBuf> {
        let path = Utf8Path::new(&name.name()).with_extension("jin");

        let absolute_path: Utf8PathBuf = path
            .as_std_path()
            .absolutize_from(dir.as_std_path())
            .ok()?
            .to_path_buf()
            .try_into()
            .expect("path to be utf8");

        search_notes.push(format!("searched path: {absolute_path}"));

        absolute_path.exists().then_some(absolute_path)
    }

    pub(super) fn parse_extern_import(
        &mut self,
        attrs: &Attrs,
        start: Span,
    ) -> DiagnosticResult<ExternImport> {
        let path = self.eat_str_lit()?;
        let relative_to = self.parent_path().unwrap();

        let lib = ExternLib::try_from_str(path.as_str(), relative_to)
            .ok_or_else(|| errors::path_not_found(path.as_str(), path.span()))?;

        Ok(ExternImport { attrs: attrs.to_owned(), lib, span: start.merge(path.span()) })
    }
}
