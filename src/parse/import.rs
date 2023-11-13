use camino::{Utf8Path, Utf8PathBuf};
use path_absolutize::Absolutize;

use crate::{
    ast::{token::TokenKind, Attr, Import},
    diagnostics::{Diagnostic, Label},
    parse::parser::{ParseResult, Parser},
    span::{Span, Spanned},
    word::Word,
};

impl<'a> Parser<'a> {
    pub fn parse_import(&mut self, attrs: &[Attr], start: Span) -> ParseResult<Import> {
        let mod_name = self.eat(TokenKind::empty_ident())?.word();
        let vis = self.parse_vis();

        let absolute_path = self.search_import_path(mod_name)?;
        self.imported_module_paths.insert(absolute_path.clone());

        if absolute_path == self.source.path() {
            return Err(Diagnostic::error("parse::import_self")
                .with_message(format!("module `{mod_name}` cannot import itself"))
                .with_label(Label::primary(mod_name.span()).with_message("here")));
        }

        Ok(Import {
            attrs: attrs.to_owned(),
            path: absolute_path,
            word: mod_name,
            vis,
            span: start.merge(mod_name.span()),
        })
    }

    fn search_import_path(&self, name: Word) -> ParseResult<Utf8PathBuf> {
        let mut search_notes = vec![];

        let path = self
            .search_module_in_subdir(name, &mut search_notes)
            .or_else(|| self.search_module_in_curr_dir(name, &mut search_notes))
            .or_else(|| self.search_package(name, &mut search_notes));

        path.ok_or_else(|| {
            Diagnostic::error("parse::module_not_found")
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
