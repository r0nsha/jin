use std::{fs, process::Command};

use anyhow::anyhow;
use camino::{Utf8Path, Utf8PathBuf};
use clap::{Parser, Subcommand, ValueEnum};
use compiler_core::{
    db::{
        build_options::{BuildOptions, EmitOption},
        Db,
    },
    target::TargetPlatform,
};
use compiler_mir::Mir;
use execute::Execute;

fn main() -> anyhow::Result<()> {
    color_eyre::install().map_err(|r| anyhow!("{r}"))?;

    let cli = Cli::parse();

    let target_platform =
        TargetPlatform::current().map_err(|os| anyhow!("{os} is not supported"))?;
    let build_options = BuildOptions::new(
        cli.timings,
        cli.emit.into_iter().map(Into::into).collect(),
        cli.output_dir,
        target_platform,
    );

    let mut db = Db::new(build_options);

    match cli.cmd {
        Commands::Build { file } => {
            build(&mut db, &file)?;
        }
        Commands::Run { file } => {
            if let Some(exe) = build(&mut db, &file)? {
                let _ = Command::new(exe).execute_output();
            }
        }
        Commands::Check { file } => {
            check(&mut db, &file)?;
        }
    }

    db.print_timings();
    db.print_diagnostics();

    Ok(())
}

fn build(db: &mut Db, root_file: &Utf8Path) -> anyhow::Result<Option<Utf8PathBuf>> {
    if let Some(mut mir) = build_to_mir(db, root_file)? {
        db.time("Mir Monomorphization", |db| compiler_mir::monomorphize(db, &mut mir));
        db.emit_file(EmitOption::Mir, |db, file| mir.pretty_print(db, file))?;

        Ok(Some(compiler_backend_c::codegen(db, &mir)))
    } else {
        Ok(None)
    }
}

fn check(db: &mut Db, root_file: &Utf8Path) -> anyhow::Result<()> {
    if let Some(mir) = build_to_mir(db, root_file)? {
        db.emit_file(EmitOption::Mir, |db, file| mir.pretty_print(db, file))?;
    }

    Ok(())
}

fn build_to_mir(db: &mut Db, root_file: &Utf8Path) -> anyhow::Result<Option<Mir>> {
    let ast = db.time("Parse", |db| compiler_parse::parse(db, root_file))?;
    fs::create_dir_all(db.output_dir())?;
    db.emit_file(EmitOption::Ast, |db, file| ast.pretty_print(db, file))?;

    if db.diagnostics.any_errors() {
        return Ok(None);
    }

    let hir = db.time("Type checking", |db| compiler_typeck::typeck(db, ast));
    db.emit_file(EmitOption::Hir, |db, file| hir.pretty_print(db, file))?;
    if db.diagnostics.any_errors() {
        return Ok(None);
    }

    let mir = db.time("Hir -> Mir", |db| compiler_mir::lower(db, &hir));
    Ok(if db.diagnostics.any_errors() { None } else { Some(mir) })
}

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[allow(clippy::struct_excessive_bools)]
struct Cli {
    #[command(subcommand)]
    cmd: Commands,

    #[arg(global = true, long, default_value_t = false)]
    timings: bool,

    #[arg(global = true, long, value_enum)]
    emit: Vec<Emit>,

    #[arg(global = true, long, short)]
    output_dir: Option<String>,
}

#[derive(Subcommand)]
enum Commands {
    Build { file: Utf8PathBuf },
    Run { file: Utf8PathBuf },
    Check { file: Utf8PathBuf },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum Emit {
    Ast,
    Hir,
    Mir,
    C,
}

impl From<Emit> for EmitOption {
    fn from(value: Emit) -> Self {
        match value {
            Emit::Ast => EmitOption::Ast,
            Emit::Hir => EmitOption::Hir,
            Emit::Mir => EmitOption::Mir,
            Emit::C => EmitOption::C,
        }
    }
}
