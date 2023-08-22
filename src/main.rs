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
    clippy::missing_const_for_fn
)]

mod ast;
mod common;
mod db;
mod diagnostics;
mod hir;
mod llvm;
mod mir;
mod parse;
mod passes;
mod span;
mod ty;

use std::path::{Path, PathBuf};

use clap::{Parser, Subcommand};
use common::time::time;
use db::{BuildOptions, Database};

use crate::{common::target::TargetPlatform, db::EmitOption};

macro_rules! bail_on_errors {
    ($db: expr) => {
        if $db.diagnostics.any() {
            return;
        }
    };
}

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[allow(clippy::struct_excessive_bools)]
struct Cli {
    #[command(subcommand)]
    cmd: Commands,

    #[arg(global = true, long, default_value_t = false)]
    timings: bool,

    #[arg(global = true, long)]
    emit: Vec<EmitOption>,
}

#[derive(Subcommand)]
enum Commands {
    Build { file: PathBuf },
    Run { file: PathBuf },
}

fn main() {
    color_eyre::install().unwrap();

    let cli = Cli::parse();

    let build_options = BuildOptions::new(
        cli.timings,
        cli.emit,
        TargetPlatform::current().expect("Current platform is not supported"),
    );

    match cli.cmd {
        Commands::Build { file } => build(build_options, &file),
        Commands::Run { file: _ } => {
            todo!();

            // if let Some(output_file) = build(build_options, file) {
            // let _ = Command::new(output_file).spawn();
            // }
        }
    }
}

fn build(build_options: BuildOptions, file: &Path) {
    let mut db = Database::new(build_options, file).unwrap();
    build_inner(&mut db);

    if db.diagnostics.any() {
        db.print_diagnostics();
    }
}

fn build_inner(db: &mut Database) {
    let print_times = db.build_options().timings;

    let ast_lib = time(print_times, "parse", || parse::parse_modules(db));

    if db.build_options().should_emit(EmitOption::Ast) {
        ast_lib.pretty_print().expect("ast printing to work");
    }

    bail_on_errors!(db);

    let mut hir = time(print_times, "ast -> hir", || hir::lower(db, ast_lib));

    time(print_times, "resolve", || passes::resolve(db, &mut hir));
    bail_on_errors!(db);

    time(print_times, "typeck", || passes::typeck(db, &mut hir));
    bail_on_errors!(db);

    if db.build_options().should_emit(EmitOption::Hir) {
        hir.pretty_print(db).expect("hir printing to work");
    }

    time(print_times, "find main", || passes::find_main(db));
    bail_on_errors!(db);

    let mir = time(print_times, "hir -> mir", || mir::lower(db, &hir));
    bail_on_errors!(db);

    if db.build_options().should_emit(EmitOption::Mir) {
        println!("\nMIR:\n");
        mir.pretty_print(db);
        println!();
    }

    llvm::codegen(db, &mir);
}
