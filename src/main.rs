#![warn(clippy::pedantic, clippy::perf, clippy::nursery)]
#![allow(
    clippy::struct_excessive_bools,
    clippy::similar_names,
    clippy::module_name_repetitions,
    clippy::too_many_lines
)]

mod ast;
mod codegen;
mod common;
mod db;
mod diagnostics;
mod hir;
mod mir;
mod parse;
mod passes;
mod span;
mod ty;

use std::{
    fs::{self, File},
    path::{Path, PathBuf},
    process::Command,
};

use clap::{Parser, Subcommand};

use common::time::time;
use db::{BuildOptions, Database};
use mir::Mir;

macro_rules! bail_on_errors {
    ($db: expr) => {
        if $db.diagnostics.any() {
            return;
        }
    };
}

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    cmd: Commands,

    #[arg(long, default_value_t = true)]
    print_times: bool,

    #[arg(global = true, long, default_value_t = false)]
    print_ast: bool,

    #[arg(global = true, long, default_value_t = false)]
    print_hir: bool,

    #[arg(global = true, long, default_value_t = false)]
    print_mir: bool,
}

#[derive(Subcommand)]
enum Commands {
    Build { file: PathBuf },
    Run { file: PathBuf },
}

fn main() {
    color_eyre::install().unwrap();

    let cli = Cli::parse();

    let build_options = BuildOptions {
        print_times: cli.print_times,
        print_ast: cli.print_ast,
        print_hir: cli.print_hir,
        print_mir: cli.print_mir,
    };

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
    let print_times = db.build_options().print_times;

    let ast_lib = time(print_times, "parse", || parse::parse_modules(db));

    if db.build_options().print_ast {
        ast_lib.pretty_print().expect("ast printing to work");
    }

    bail_on_errors!(db);

    let mut hir = time(print_times, "ast -> hir", || hir::lower(db, ast_lib));

    time(print_times, "resolve", || passes::resolve(db, &mut hir));
    bail_on_errors!(db);

    time(print_times, "infer", || passes::infer(db, &mut hir));
    bail_on_errors!(db);

    if db.build_options().print_hir {
        hir.pretty_print(db).expect("hir printing to work");
    }

    time(print_times, "find main", || passes::find_main(db));
    bail_on_errors!(db);

    let mir = time(print_times, "hir -> mir", || mir::lower(db, &hir));
    bail_on_errors!(db);

    if db.build_options().print_mir {
        println!("\nMIR:\n");
        mir.pretty_print(db);
        println!();
    }

    codegen(db, &mir);
}

fn codegen(db: &mut Database, mir: &Mir) {
    let print_times = db.build_options().print_times;

    let out_dir = Path::new("out");
    let main_module_name = db.main_module().unwrap().name.name();
    let out_file_name = out_dir.join(main_module_name.as_str());
    let out_c_file = out_file_name.with_extension("c");

    fs::create_dir_all(out_dir).unwrap();
    let mut c_file = File::create(&out_c_file).unwrap();

    time(print_times, "codegen", || codegen::codegen(db, mir, &mut c_file));

    time(print_times, "clang", || {
        Command::new("clang")
            .args([
                "-Wno-unused-command-line-argument",
                out_c_file.to_string_lossy().as_ref(),
                "-o",
                out_file_name.to_string_lossy().as_ref(),
                "-x",
                "c",
                "-std=c99",
            ])
            .spawn()
            .unwrap()
    });
}
