mod errors;
mod lexer;
mod parser;
mod token;

use camino::{Utf8Path, Utf8PathBuf};
use compiler_ast::{Ast, Module};
use compiler_core::db::ModuleId;
use compiler_core::qpath::QPath;
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
    let root_std_file = compiler_helpers::current_exe_dir().join("std/main.jin");
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
    parse_module(db, package, ast, db.package(package).root_path.clone(), None);
}

fn parse_module(
    db: &mut Db,
    package: Ustr,
    ast: &mut Ast,
    dir: Utf8PathBuf,
    parent: Option<ModuleId>,
) {
    let module_id = create_module(db, package, &dir, parent);
    let mut module = Module::new(module_id);

    let files: Vec<_> = std::fs::read_dir(&dir)
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

    for file in files {
        let source_id = db.sources.load_file(file).unwrap();
        db.source_to_module.insert(source_id, module_id);

        match parse_file(db, package, source_id, &mut module) {
            Ok(submodule_paths) => {
                for path in submodule_paths {
                    if db.sources.find_by_path(&path).is_some() {
                        continue;
                    }

                    parse_module(db, package, ast, path, Some(module.id));
                }
            }
            Err(diag) => db.diagnostics.add(diag),
        }
    }

    ast.modules.push(module);
}

fn create_module(db: &mut Db, package: Ustr, dir: &Utf8Path, parent: Option<ModuleId>) -> ModuleId {
    let package_root = &db.package(package).root_path;
    let name = QPath::from_path(package_root, dir).unwrap();
    db.add_module(package, dir.to_path_buf(), name, parent)
}

fn parse_file(
    db: &mut Db,
    package: Ustr,
    source_id: SourceId,
    module: &mut Module,
) -> DiagnosticResult<FxHashSet<Utf8PathBuf>> {
    let is_package_main = source_id == db.package(package).main_source_id;
    let is_main = db.is_main_package(package) && is_package_main;

    let (items, submodule_paths) = {
        let source = db.sources.get(source_id).unwrap();
        let tokens = lexer::tokenize(source)?;
        parser::parse(db, source, tokens)?
    };

    module.items.extend(items);

    if is_main {
        db.main_module.set(module.id);
    }

    Ok(submodule_paths)
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
