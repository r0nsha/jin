mod errors;
mod lexer;
mod parser;
mod token;

use camino::{Utf8Path, Utf8PathBuf};
use compiler_core::middle::{BinOp, CmpOp};
use rustc_hash::FxHashSet;
use ustr::{ustr, Ustr};

use compiler_core::{
    ast::{Ast, Module},
    db::{Db, ModuleInfo},
    diagnostics::DiagnosticResult,
    span::SourceId,
};

use crate::token::TokenKind;

pub fn parse(db: &mut Db, root_file: &Utf8Path) -> anyhow::Result<Ast> {
    let mut ast = Ast::new();

    // Std
    let root_std_file = compiler_helpers::current_exe_dir().join("std/std.jin");
    let (std_package, _) = db.create_package(ustr("std"), &root_std_file)?;
    db.std_package_name.set(std_package);
    parse_package(db, &mut ast, std_package);

    // Main package
    let root_file_stem = root_file.file_stem().unwrap();
    let (main_package, _) = db.create_package(ustr(root_file_stem), root_file)?;
    db.set_main_package(main_package);
    parse_package(db, &mut ast, main_package);

    Ok(ast)
}

fn parse_package(db: &mut Db, ast: &mut Ast, package: Ustr) {
    parse_module(db, package, ast, db.package(package).main_source_id);
}

fn parse_module(db: &mut Db, package: Ustr, ast: &mut Ast, source_id: SourceId) {
    match parse_module_inner(db, package, source_id) {
        Ok((module, imported_module_paths)) => {
            ast.modules.push(module);

            for path in imported_module_paths {
                parse_module_from_path(db, path, ast);
            }
        }
        Err(diag) => db.diagnostics.add(diag),
    }
}

fn parse_module_inner(
    db: &mut Db,
    package: Ustr,
    source_id: SourceId,
) -> DiagnosticResult<(Module, FxHashSet<Utf8PathBuf>)> {
    let (mut module, paths) = {
        let source = db.sources.get(source_id).unwrap();
        let tokens = lexer::tokenize(source)?;
        parser::parse(db, package, source, tokens)?
    };

    module.id =
        ModuleInfo::alloc(db, package, module.source, module.name.clone(), module.is_main());

    Ok((module, paths))
}

fn parse_module_from_path(db: &mut Db, path: Utf8PathBuf, ast: &mut Ast) {
    if db.sources.find_by_path(&path).is_none() {
        let package = db.find_package_by_path(&path).expect("to be part of a package").name;
        let source_id = db.sources.load_file(path).expect("import path to exist");
        parse_module(db, package, ast, source_id);
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
