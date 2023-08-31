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
    clippy::cast_possible_truncation
)]

mod ast;
mod common;
mod db;
mod diagnostics;
mod llvm;
mod mir;
mod parse;
mod passes;
mod span;
mod tast;
mod ty;

use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, Subcommand};

use crate::{
    common::target::TargetPlatform,
    db::{
        build_options::{BuildOptions, EmitOption},
        Db,
    },
    ty::tcx::TyCtxt,
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
    db.timings.start("parse");
    let mut ast = parse::parse_modules(db);

    if db.build_options().should_emit(EmitOption::Ast) {
        ast.pretty_print().expect("ast printing to work");
    }
    expect!(db);

    let tcx = TyCtxt::new();

    db.timings.start("resolve");
    passes::resolve(db, &tcx, &mut ast);
    expect!(db);

    db.timings.start("ast -> typed ast");
    let mut tast = tast::lower(db, &tcx, ast);

    db.timings.start("typeck");
    if let Err(diag) = passes::typeck(db, &tcx, &mut tast) {
        db.diagnostics.add(diag);
    }
    db.timings.stop();
    expect!(db);

    if db.build_options().should_emit(EmitOption::TypedAst) {
        tast.pretty_print(db).expect("typed-ast printing to work");
    }

    // TODO: uncomment
    // db.timings.start("check entry");
    // if let Err(diag) = passes::check_entry(db) {
    //     db.diagnostics.add(diag);
    // }
    // db.timings.stop();
    // expect!(db);

    db.timings.start("typed ast -> mir");
    let mir = mir::lower(db, &tast).expect("mir lowering to succeed");
    db.timings.stop();
    expect!(db);

    if db.build_options().should_emit(EmitOption::Mir) {
        println!();
        println!("Mir:");
        println!();
        mir.pretty_print(db);
        println!();
    }

    llvm::codegen(db, &mir);

    db.timings.print();
}
