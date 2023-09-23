#![warn(
    clippy::correctness,
    clippy::suspicious,
    clippy::style,
    clippy::complexity,
    clippy::perf,
    clippy::pedantic
)]
#![allow(
    clippy::similar_names,
    clippy::module_name_repetitions,
    clippy::too_many_lines,
    clippy::missing_const_for_fn,
    clippy::cast_possible_truncation,
    clippy::wildcard_imports
)]
#![feature(iterator_try_collect)]

mod ast;
mod common;
mod db;
mod diagnostics;
mod hir;
mod llvm;
mod macros;
mod name_resolution;
mod parse;
mod span;
mod subst;
mod sym;
mod tir;
mod ty;
mod typeck;

use std::{fs, path::PathBuf};

use anyhow::Result;
use clap::{Parser, Subcommand};

use crate::{
    common::target::TargetPlatform,
    db::{
        build_options::{BuildOptions, EmitOption},
        Db,
    },
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
    Build { file: PathBuf },
}

macro_rules! expect {
    ($db: expr) => {
        if $db.diagnostics.any() {
            return;
        }
    };
}

fn main() -> Result<()> {
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
            let mut db = Db::new(build_options, &file)?;
            build(&mut db);
            Ok(())
        }
    }
}

fn build(db: &mut Db) {
    // Create the output directory
    fs::create_dir_all(db.output_dir()).expect("failed creating build directory");

    // Parse the entire module tree into an Ast
    db.time.start("parse");
    let ast = parse::parse_module_tree(db);
    expect!(db);

    db.emit_file(EmitOption::Ast, |_, file| ast.pretty_print(file)).expect("emitting ast failed");

    // Resolve all root symbols into their corresponding id's
    db.time.start("name resolution");
    let mut hir = match name_resolution::resolve(db, &ast) {
        Ok(hir) => hir,
        Err(diag) => {
            db.diagnostics.emit(diag);
            return;
        }
    };
    expect!(db);

    // Type check pass
    db.time.start("type check");
    if let Err(diag) = typeck::typeck(db, &mut hir) {
        db.diagnostics.emit(diag);
    }
    db.time.stop();
    expect!(db);

    db.emit_file(EmitOption::Hir, |db, file| hir.pretty_print(db, file))
        .expect("emitting hir failed");

    // Lower to TIR, includes monomorphization
    db.time.start("hir -> tir");
    let tir = tir::lower(db, &hir);
    db.time.stop();
    expect!(db);

    db.emit_file(EmitOption::Tir, |db, file| tir.pretty_print(db, file))
        .expect("emitting tir failed");

    llvm::codegen(db, &tir);

    db.time.print();
}
