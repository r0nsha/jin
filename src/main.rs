#![warn(
    clippy::correctness,
    clippy::suspicious,
    clippy::style,
    clippy::complexity,
    clippy::perf,
    clippy::pedantic,
    clippy::unwrap_used
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

use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, Subcommand};

use crate::{
    common::target::TargetPlatform,
    db::{BuildOptions, Database, EmitOption},
};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[allow(clippy::struct_excessive_bools)]
struct Cli {
    #[command(subcommand)]
    cmd: Commands,

    #[arg(global = true, long, default_value_t = false)]
    timings: bool,

    #[arg(value_enum, global = true, long)]
    emit: Vec<EmitOption>,
}

#[derive(Subcommand)]
enum Commands {
    Build { file: PathBuf },
}

macro_rules! expect {
    ($db: expr) => {
        if $db.diagnostics.any() {
            $db.print_diagnostics();
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
        TargetPlatform::current().expect("Current platform is not supported"),
    );

    match cli.cmd {
        Commands::Build { file } => {
            let mut db = Database::new(build_options, &file)?;
            build(&mut db);
            Ok(())
        }
    }
}

fn build(db: &mut Database) {
    db.timings.start("parse");
    let ast_lib = parse::parse_modules(db);

    if db.build_options().should_emit(EmitOption::Ast) {
        ast_lib.pretty_print().expect("ast printing to work");
    }
    expect!(db);

    db.timings.start("ast -> hir");
    let mut hir = hir::lower(db, ast_lib);

    db.timings.start("resolve");
    passes::resolve(db, &mut hir);
    expect!(db);

    db.timings.start("typeck");
    passes::typeck(db, &mut hir);
    db.timings.stop();
    expect!(db);

    if db.build_options().should_emit(EmitOption::Hir) {
        hir.pretty_print(db).expect("hir printing to work");
    }

    db.timings.start("find main");
    passes::find_main(db);
    expect!(db);

    db.timings.start("hir -> mir");
    let mir = mir::lower(db, &hir);
    db.timings.stop();
    expect!(db);

    if db.build_options().should_emit(EmitOption::Mir) {
        println!("\nMIR:\n");
        mir.pretty_print(db);
        println!();
    }

    llvm::codegen(db, &mir);

    db.timings.print();
}
