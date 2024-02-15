use std::ops::ControlFlow;

use camino::{Utf8Path, Utf8PathBuf};
use path_absolutize::Absolutize as _;

use crate::{
    ast::{Attrs, ExternImport, Import, ImportTree},
    db::ExternLib,
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::{IsUfcs, Vis},
    parse::{
        errors,
        parser::Parser,
        token::{Kw, TokenKind},
    },
    span::{Span, Spanned},
    word::Word,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_import(
        &mut self,
        attrs: &Attrs,
        vis: Vis,
        start: Span,
    ) -> DiagnosticResult<Import> {
        let root = self.eat_ident()?.word();
        let module_path = self.search_import_path(root)?;
        self.imported_module_paths.insert(module_path.clone());
        let tree = self.parse_import_tree(root)?;
        Ok(Import {
            attrs: attrs.clone(),
            vis,
            module_path,
            tree,
            span: start.merge(self.last_span()),
        })
    }

    fn parse_import_tree(&mut self, name: Word) -> DiagnosticResult<ImportTree> {
        if self.is(TokenKind::Dot) {
            let next = self.parse_import_tree_cont()?;
            Ok(ImportTree::Path(name, Box::new(next)))
        } else if self.is_kw(Kw::As) {
            let alias = self.eat_ident()?.word();
            Ok(ImportTree::Name(name, Some(alias)))
        } else {
            Ok(ImportTree::Name(name, None))
        }
    }

    fn parse_import_tree_cont(&mut self) -> DiagnosticResult<ImportTree> {
        if self.is(TokenKind::Star) {
            Ok(ImportTree::Glob(IsUfcs::No, self.last_span()))
        } else if self.is(TokenKind::QuestionMark) {
            Ok(ImportTree::Glob(IsUfcs::Yes, self.last_span()))
        } else if self.is_ident() {
            self.parse_import_tree(self.last_token().word())
        } else if self.peek_is(TokenKind::OpenCurly) {
            self.parse_import_group()
        } else {
            Err(self.unexpected_token("an identifier . ( * or ?"))
        }
    }

    fn parse_import_group(&mut self) -> DiagnosticResult<ImportTree> {
        self.parse_list(TokenKind::OpenCurly, TokenKind::CloseCurly, |this| {
            this.parse_import_tree_cont().map(ControlFlow::Continue)
        })
        .map(|(imports, _)| ImportTree::Group(imports))
    }

    fn search_import_path(&self, name: Word) -> DiagnosticResult<Utf8PathBuf> {
        let mut search_notes = vec![];

        let path = self
            .is_package_root
            .then(|| self.search_module_in_currdir(name, &mut search_notes))
            .flatten()
            .or_else(|| self.search_module_in_subdir(name, &mut search_notes))
            .or_else(|| self.search_package(name, &mut search_notes));

        path.ok_or_else(|| {
            Diagnostic::error(format!("could not find module or library `{name}`"))
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

    pub(super) fn parse_extern_import(
        &mut self,
        attrs: &Attrs,
        start: Span,
    ) -> DiagnosticResult<ExternImport> {
        let path_tok = self.eat(TokenKind::empty_str())?;
        let path = path_tok.str_value();
        let relative_to = self.parent_path().unwrap();

        let lib = ExternLib::try_from_str(&path, relative_to)
            .ok_or_else(|| errors::path_not_found(path.as_str(), path_tok.span))?;

        Ok(ExternImport { attrs: attrs.to_owned(), lib, span: start.merge(path_tok.span) })
    }
}
