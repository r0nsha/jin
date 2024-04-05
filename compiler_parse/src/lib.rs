mod errors;
mod lexer;
mod parser;
mod token;

use std::fs;

use camino::{Utf8Path, Utf8PathBuf};
use compiler_ast::{Ast, Item};
use compiler_core::{
    db::{Db, ModuleId},
    diagnostics::DiagnosticResult,
    middle::{BinOp, CmpOp},
    span::SourceId,
};
use ustr::Ustr;

use crate::token::TokenKind;

pub fn parse(db: &mut Db, root_file: &Utf8Path) -> anyhow::Result<Ast> {
    let mut ast = Ast::new();

    // Std package
    let root_std_file = compiler_helpers::current_exe_dir().join("std/_.jin");
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
    parse_dir(db, package, ast, db.package(package).root_path.clone(), None);
}

fn parse_dir(
    db: &mut Db,
    package: Ustr,
    ast: &mut Ast,
    dir: Utf8PathBuf,
    parent: Option<ModuleId>,
) {
    let files: Vec<_> = fs::read_dir(&dir)
        .unwrap()
        .filter_map(Result::ok)
        .filter_map(|entry| {
            let path = entry.path();
            if path.extension().map_or(false, |e| e == "jin") && path.is_file() {
                Some(Utf8PathBuf::from_path_buf(path).unwrap())
            } else {
                None
            }
        })
        .collect();

    // Skip directories that have no source files
    if files.is_empty() {
        return;
    }

    let module_id = db.add_module(package, dir.clone(), parent);
    let mut items = Vec::<Item>::new();

    // Parse this directory's source files
    for file in files {
        let source_id = db.sources.load_file(file).unwrap();
        db.source_to_module.insert(source_id, module_id);

        if let Err(diag) = parse_file(db, package, module_id, source_id, &mut items) {
            db.diagnostics.add(diag)
        }
    }

    // Parse subdirectories
    let dirs = fs::read_dir(&dir).unwrap().filter_map(Result::ok).filter_map(|entry| {
        let path = entry.path();
        if path.is_dir() {
            Some(Utf8PathBuf::from_path_buf(path).unwrap())
        } else {
            None
        }
    });

    for dir in dirs {
        parse_dir(db, package, ast, dir, Some(module_id));
    }

    ast.items.extend(items);
}

fn parse_file(
    db: &mut Db,
    package: Ustr,
    module_id: ModuleId,
    source_id: SourceId,
    items: &mut Vec<Item>,
) -> DiagnosticResult<()> {
    let is_package_main = source_id == db.package(package).main_source_id;
    let is_main = db.is_main_package(package) && is_package_main;

    let file_items = {
        let source = db.sources.get(source_id).unwrap();
        let tokens = lexer::tokenize(source)?;
        parser::parse(db, source, module_id, tokens)?
    };

    items.extend(file_items);

    if is_main {
        db.main_module.set(module_id);
    }

    Ok(())
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
