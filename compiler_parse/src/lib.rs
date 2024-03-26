mod errors;
mod lexer;
mod parser;
mod token;

use camino::{Utf8Path, Utf8PathBuf};
use compiler_ast::{Ast, Module};
use compiler_core::db::ModuleId;
use compiler_core::{
    db::Db,
    diagnostics::DiagnosticResult,
    middle::{BinOp, CmpOp},
    span::SourceId,
};
use rustc_hash::FxHashSet;
use ustr::Ustr;

use crate::token::TokenKind;

pub fn parse(db: &mut Db, root_file: &Utf8Path) -> anyhow::Result<Ast> {
    let mut ast = Ast::new();

    // Std package
    let root_std_file = compiler_helpers::current_exe_dir().join("std/std.jin");
    let (std_package, _) = db.create_package(&root_std_file)?;
    db.std_package_name.set(std_package);
    parse_package(db, &mut ast, std_package);

    // Main package
    let (main_package, _) = db.create_package(root_file)?;
    db.set_main_package(main_package);
    parse_package(db, &mut ast, main_package);

    Ok(ast)
}

fn parse_package(db: &mut Db, ast: &mut Ast, package: Ustr) {
    parse_module(db, package, ast, db.package(package).main_source_id, None);
}

fn parse_module(
    db: &mut Db,
    package: Ustr,
    ast: &mut Ast,
    source_id: SourceId,
    parent: Option<ModuleId>,
) {
    fn inner(
        db: &mut Db,
        package: Ustr,
        source_id: SourceId,
        parent: Option<ModuleId>,
    ) -> DiagnosticResult<(Module, FxHashSet<Utf8PathBuf>)> {
        let is_package_main = source_id == db.package(package).main_source_id;
        let is_main = db.is_main_package(package) && is_package_main;

        let (mut module, submodule_paths) = {
            let source = db.sources.get(source_id).unwrap();
            let tokens = lexer::tokenize(source)?;
            parser::parse(db, package, source, is_package_main, tokens)?
        };

        module.id = db.add_module(package, module.source, module.name.clone(), parent);

        if is_main {
            db.main_module.set(module.id);
        }

        db.source_to_module.insert(source_id, module.id);

        Ok((module, submodule_paths))
    }

    match inner(db, package, source_id, parent) {
        Ok((module, submodule_paths)) => {
            let id = module.id;
            ast.modules.push(module);

            for path in submodule_paths {
                if db.sources.find_by_path(&path).is_some() {
                    continue;
                }

                let package = db.find_package_by_path(&path).expect("to be part of a package").name;
                let source_id = db.sources.load_file(path).expect("import path to exist");
                parse_module(db, package, ast, source_id, Some(id));
            }
        }
        Err(diag) => db.diagnostics.add(diag),
    }
}

pub(crate) fn bin_op_from_assign_op(tk: TokenKind) -> Option<BinOp> {
    Some(match tk {
        TokenKind::StarEq => BinOp::Mul,
        TokenKind::FwSlashEq => BinOp::Div,
        TokenKind::PercentEq => BinOp::Rem,
        TokenKind::PlusEq => BinOp::Add,
        TokenKind::MinusEq => BinOp::Sub,
        TokenKind::LtLtEq => BinOp::Shl,
        TokenKind::GtGtEq => BinOp::Shr,
        TokenKind::AmpEq => BinOp::BitAnd,
        TokenKind::CaretEq => BinOp::BitXor,
        TokenKind::PipeEq => BinOp::BitOr,
        _ => return None,
    })
}

impl TryFrom<TokenKind> for BinOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        let op = match value {
            TokenKind::EqEq => Self::Cmp(CmpOp::Eq),
            TokenKind::BangEq => Self::Cmp(CmpOp::Ne),
            TokenKind::Star => Self::Mul,
            TokenKind::FwSlash => Self::Div,
            TokenKind::Percent => Self::Rem,
            TokenKind::Plus => Self::Add,
            TokenKind::Minus => Self::Sub,
            TokenKind::Lt => Self::Cmp(CmpOp::Lt),
            TokenKind::LtLt => Self::Shl,
            TokenKind::LtEq => Self::Cmp(CmpOp::Le),
            TokenKind::Gt => Self::Cmp(CmpOp::Gt),
            TokenKind::GtGt => Self::Shr,
            TokenKind::GtEq => Self::Cmp(CmpOp::Ge),
            TokenKind::Amp => Self::BitAnd,
            TokenKind::AmpAmp => Self::And,
            TokenKind::Caret => Self::BitXor,
            TokenKind::Pipe => Self::BitOr,
            TokenKind::PipePipe => Self::Or,
            _ => return Err(()),
        };

        Ok(op)
    }
}
