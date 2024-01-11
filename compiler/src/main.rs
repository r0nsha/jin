#![feature(iterator_try_collect)]

mod ast;
mod cgen;
mod counter;
mod db;
mod diagnostics;
mod hir;
mod macros;
mod mangle;
mod middle;
mod mir;
mod parse;
mod qpath;
mod span;
mod subst;
mod sym;
mod target;
mod ty;
mod typeck;
mod word;

use std::fs;

use camino::{Utf8Path, Utf8PathBuf};
use clap::{Parser, Subcommand};

use crate::{
    db::{
        build_options::{BuildOptions, EmitOption},
        Db,
    },
    target::TargetPlatform,
};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[allow(clippy::struct_excessive_bools)]
struct Cli {
    #[command(subcommand)]
    cmd: Commands,

    #[arg(global = true, long, default_value_t = false)]
    timings: bool,

    #[arg(global = true, long, value_enum)]
    emit: Vec<EmitOption>,

    #[arg(global = true, long)]
    out_dir: Option<String>,
}

#[derive(Subcommand)]
enum Commands {
    Build { file: Utf8PathBuf },
}

macro_rules! expect {
    ($db: expr) => {
        if $db.diagnostics.any() {
            return;
        }
    };
}

fn main() {
    color_eyre::install().expect("color_eyre::install to work");

    let cli = Cli::parse();

    let build_options = BuildOptions::new(
        cli.timings,
        cli.emit,
        cli.out_dir,
        TargetPlatform::current().expect("Current platform is not supported"),
    );

    match cli.cmd {
        Commands::Build { file } => {
            let mut db = Db::new(build_options);
            build(&mut db, &file);
        }
    }
}

#[allow(clippy::similar_names)]
fn build(db: &mut Db, root_file: &Utf8Path) {
    // Parse the entire module tree into an Ast
    let ast =
        match db.time("Parse", |db| parse::parse_module_tree(db, root_file)) {
            Ok(ast) => ast,
            Err(err) => {
                eprintln!("{err}");
                return;
            }
        };

    expect!(db);

    // Create the output directory
    fs::create_dir_all(db.output_dir())
        .expect("failed creating build directory");

    db.emit_file(EmitOption::Ast, |_, file| ast.pretty_print(file))
        .expect("emitting ast failed");

    // Resolve all root symbols into their corresponding id's
    let hir = match db.time("Type checking", |db| typeck::typeck(db, &ast)) {
        Ok(hir) => hir,
        Err(diag) => {
            db.diagnostics.emit(diag);
            return;
        }
    };
    expect!(db);

    db.emit_file(EmitOption::Hir, |db, file| hir.pretty_print(db, file))
        .expect("emitting hir failed");

    // Lower HIR to MIR
    let mut mir = db.time("Hir -> Mir", |db| mir::lower(db, &hir));
    expect!(db);

    mir.dbg();

    // Specialize polymorphic MIR
    db.time("Mir Specialization", |db| mir::specialize(db, &mut mir));

    db.emit_file(EmitOption::Mir, |db, file| mir.pretty_print(db, file))
        .expect("emitting mir failed");

    // Generate C code from Mir
    cgen::codegen(db, &mir);

    db.print_timings();
}
