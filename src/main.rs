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
    db.time.start("parse");
    let mut ast = parse::parse_modules(db);

    if db.build_options().should_emit(EmitOption::Ast) {
        ast.pretty_print().expect("ast printing to work");
    }
    expect!(db);

    db.time.start("resolve");
    passes::resolve(db, &mut ast);
    expect!(db);

    db.time.start("ast -> hir");
    let mut hir = hir::lower(db, ast);

    db.time.start("typeck");
    if let Err(diag) = passes::typeck(db, &mut hir) {
        db.diagnostics.add(diag);
    }
    db.time.stop();
    expect!(db);

    if db.build_options().should_emit(EmitOption::Hir) {
        hir.pretty_print(db).expect("hir printing to work");
    }

    db.time.start("check entry");
    if let Err(diag) = passes::check_entry(db, &hir) {
        db.diagnostics.add(diag);
    }
    db.time.stop();
    expect!(db);

    db.time.start("monomorphize");
    let mono_items = passes::monomorphize(db, &hir);
    db.time.stop();

    db.time.start("hir -> mir");
    let mir = mir::lower(db, &hir, mono_items).expect("mir lowering to succeed");
    db.time.stop();
    expect!(db);

    if db.build_options().should_emit(EmitOption::Mir) {
        println!();
        println!("Mir:");
        println!();
        mir.pretty_print(db);
        println!();
    }

    llvm::codegen(db, &mir);

    db.time.print();
}
