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
mod cgen;
mod counter;
mod db;
mod diagnostics;
mod hir;
mod index_vec;
mod macros;
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

use anyhow::Result;
use camino::Utf8PathBuf;
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
    fs::create_dir_all(db.output_dir())
        .expect("failed creating build directory");

    // Parse the entire module tree into an Ast
    db.time.start("parse");
    let ast = parse::parse_module_tree(db);
    expect!(db);

    db.emit_file(EmitOption::Ast, |_, file| ast.pretty_print(file))
        .expect("emitting ast failed");

    // Resolve all root symbols into their corresponding id's
    db.time.start("typeck");
    let hir = match typeck::typeck(db, &ast) {
        Ok(hir) => hir,
        Err(diag) => {
            db.diagnostics.emit(diag);
            return;
        }
    };
    db.time.stop();
    expect!(db);

    db.emit_file(EmitOption::Hir, |db, file| hir.pretty_print(db, file))
        .expect("emitting hir failed");

    // Lower to MIR, includes monomorphization
    db.time.start("hir -> mir");
    let mir = mir::lower(db, &hir);
    db.time.stop();
    expect!(db);

    db.emit_file(EmitOption::Mir, |db, file| mir.pretty_print(db, file))
        .expect("emitting mir failed");

    // Generate C code from Mir
    cgen::codegen(db, &mir);

    db.time.print();
}
