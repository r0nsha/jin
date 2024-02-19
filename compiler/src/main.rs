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
mod util;
mod word;

use std::fs;

use anyhow::anyhow;
use camino::{Utf8Path, Utf8PathBuf};
use clap::{Parser, Subcommand};

use crate::{
    db::{
        build_options::{BuildOptions, EmitOption},
        Db,
    },
    mir::Mir,
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
    Check { file: Utf8PathBuf },
}

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;

    if let Err(err) = run_cli() {
        eprintln!("Error: {err}");
    }

    Ok(())
}

fn run_cli() -> anyhow::Result<()> {
    let cli = Cli::parse();

    let target_platform =
        TargetPlatform::current().map_err(|os| anyhow!("{os} is not supported"))?;
    let build_options = BuildOptions::new(cli.timings, cli.emit, cli.out_dir, target_platform);
    let mut db = Db::new(build_options);

    match cli.cmd {
        Commands::Build { file } => build(&mut db, &file)?,
        Commands::Check { file } => check(&mut db, &file)?,
    }

    db.print_timings();
    db.print_diagnostics();

    Ok(())
}

fn build(db: &mut Db, root_file: &Utf8Path) -> anyhow::Result<()> {
    if let Some(mir) = build_to_mir(db, root_file)? {
        cgen::codegen(db, &mir);
    }

    Ok(())
}

fn check(db: &mut Db, root_file: &Utf8Path) -> anyhow::Result<()> {
    build_to_mir(db, root_file).map(|_| ())
}

fn build_to_mir(db: &mut Db, root_file: &Utf8Path) -> anyhow::Result<Option<Mir>> {
    // File -> Ast
    let ast = db.time("Parse", |db| parse::parse(db, root_file))?;
    fs::create_dir_all(db.output_dir())?;
    db.emit_file(EmitOption::Ast, |_, file| ast.pretty_print(file))?;

    if db.diagnostics.any_errors() {
        return Ok(None);
    }

    // Ast -> Hir
    let hir = db.time("Type checking", |db| typeck::typeck(db, ast));
    db.emit_file(EmitOption::Hir, |db, file| hir.pretty_print(db, file))?;
    if db.diagnostics.any_errors() {
        return Ok(None);
    }

    let mut mir = db.time("Hir -> Mir", |db| mir::lower(db, &hir));
    if db.diagnostics.any_errors() {
        return Ok(Some(mir));
    }

    db.time("Mir Monomorphization", |db| mir::monomorphize(db, &mut mir));
    db.emit_file(EmitOption::Mir, |db, file| mir.pretty_print(db, file))?;

    Ok(Some(mir))
}

impl TryFrom<Cli> for BuildOptions {
    type Error = anyhow::Error;

    fn try_from(cli: Cli) -> Result<Self, Self::Error> {
        let tp = TargetPlatform::current().map_err(|os| anyhow!("{os} is not supported"))?;
        Ok(BuildOptions::new(cli.timings, cli.emit, cli.out_dir, tp))
    }
}
