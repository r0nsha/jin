use pretty::RcDoc;

use crate::db::Database;

use super::*;

pub(crate) fn print(db: &Database, mir: &Mir) {
    let doc = RcDoc::intersperse(mir.functions.iter().map(|f| f.to_doc(db)), RcDoc::line());

    println!("{}", doc.pretty(80));
}

trait ToDoc<'db, 'd> {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()>;
}

impl<'db, 'd> ToDoc<'db, 'd> for Function {
    fn to_doc(&self, db: &'db Database) -> RcDoc<'d, ()> {
        RcDoc::text("fn...")
    }
}
