use std::ops::ControlFlow;

use camino::{Utf8Path, Utf8PathBuf};
use path_absolutize::Absolutize as _;

use crate::{
    ast::{token::TokenKind, Attrs, ExternImport, Import, ImportKind, UnqualifiedImport},
    db::ExternLib,
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::IsUfcs,
    parse::{errors, parser::Parser},
    span::{Span, Spanned},
    word::Word,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_import(&mut self, attrs: &Attrs, start: Span) -> DiagnosticResult<Import> {
        let root = self.eat_ident()?.word();
        let module_path = self.search_import_path(root)?;
        self.imported_module_paths.insert(module_path.clone());

        let (path, kind) = self.parse_import_path_and_kind(root)?;

        Ok(Import {
            attrs: attrs.clone(),
            module_path,
            path,
            kind,
            span: start.merge(self.last_span()),
        })
    }

    fn parse_import_path_and_kind(
        &mut self,
        root: Word,
    ) -> DiagnosticResult<(Vec<Word>, ImportKind)> {
        let mut path = vec![root];

        if self.is(TokenKind::As) {
            let alias = self.eat_ident()?.word();
            let vis = self.parse_vis();
            return Ok((path, ImportKind::Qualified(Some(alias), vis)));
        }

        while self.is(TokenKind::Dot) {
            // let tok = self.token();
            //
            // match tok{
            //     _=>{
            //
            //     }
            // }
            if self.is_ident() {
                path.push(self.last_token().word());
            } else if self.is(TokenKind::As) {
                let alias = self.eat_ident()?.word();
                let vis = self.parse_vis();
                return Ok((path, ImportKind::Qualified(Some(alias), vis)));
            } else if self.peek_is(TokenKind::OpenCurly) {
                let imports = self.parse_unqualified_imports()?;
                return Ok((path, ImportKind::Unqualified(imports)));
            } else if self.is(TokenKind::Star) {
                return Ok((
                    path,
                    ImportKind::Unqualified(vec![UnqualifiedImport::Glob(
                        IsUfcs::No,
                        self.last_span(),
                    )]),
                ));
            } else if self.is(TokenKind::QuestionMark) {
                return Ok((
                    path,
                    ImportKind::Unqualified(vec![UnqualifiedImport::Glob(
                        IsUfcs::Yes,
                        self.last_span(),
                    )]),
                ));
            } else {
                let tok = self.require()?;
                return Err(errors::unexpected_token_err(
                    "identifier, {, * or ?",
                    tok.kind,
                    tok.span,
                ));
            }
        }

        let vis = self.parse_vis();
        Ok((path, ImportKind::Qualified(None, vis)))
    }

    fn parse_unqualified_imports(&mut self) -> DiagnosticResult<Vec<UnqualifiedImport>> {
        self.parse_list(TokenKind::OpenCurly, TokenKind::CloseCurly, |this| {
            this.parse_unqualified_import().map(ControlFlow::Continue)
        })
        .map(|(imports, _)| imports)
    }

    fn parse_unqualified_import(&mut self) -> DiagnosticResult<UnqualifiedImport> {
        let tok = self.eat_any()?;

        match tok.kind {
            TokenKind::Ident(_) => {
                let name = tok.word();
                let alias =
                    if self.is(TokenKind::As) { Some(self.eat_ident()?.word()) } else { None };
                let vis = self.parse_vis();
                Ok(UnqualifiedImport::Name(name, alias, vis))
            }
            TokenKind::Star => Ok(UnqualifiedImport::Glob(IsUfcs::No, tok.span)),
            TokenKind::QuestionMark => Ok(UnqualifiedImport::Glob(IsUfcs::Yes, tok.span)),
            _ => Err(errors::unexpected_token_err("identifier, * or ?", tok.kind, tok.span)),
        }
    }

    fn search_import_path(&self, name: Word) -> DiagnosticResult<Utf8PathBuf> {
        let mut search_notes = vec![];

        let path = self
            .search_module_in_subdir(name, &mut search_notes)
            .or_else(|| self.search_package(name, &mut search_notes));

        path.ok_or_else(|| {
            Diagnostic::error(format!("could not find module or library `{name}`"))
                .with_label(Label::primary(name.span(), "not found"))
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
