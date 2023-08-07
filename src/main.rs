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

use db::{BuildOptions, Database};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    cmd: Commands,

    #[arg(long, default_value_t = true)]
    print_times: bool,

    #[arg(long, default_value_t = false)]
    print_ast: bool,

    #[arg(long, default_value_t = false)]
    print_hir: bool,
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
    };

    match cli.cmd {
        Commands::Build { file } => build(build_options, file),
        Commands::Run { file } => {
            todo!();

            // if let Some(output_file) = build(build_options, file) {
            // let _ = Command::new(output_file).spawn();
            // }
        }
    }
}

fn build(build_options: BuildOptions, file: PathBuf) {
    let mut db = Database::new(build_options, file).unwrap();
    build_inner(&mut db);
}

fn build_inner(db: &mut Database) {
    let print_times = db.build_options().print_times;

    let ast = time! { print_times, "ast generation", parse::parse_modules(db) };

    if db.build_options().print_ast {
        println!("Ast:");
        for module in &ast {
            module.pretty_print().unwrap();
        }
    }

    bail_if_failed!(db);

    let mut hir = time! { print_times, "ast -> hir", hir::lower(db, ast) };

    time! { print_times, "resolve", passes::resolve(db, &mut hir) };
    bail_if_failed!(db);

    time! { print_times, "infer", passes::infer(db, &mut hir) };
    bail_if_failed!(db);

    if db.build_options().print_hir {
        println!("\nHir:\n");
        for module in &hir {
            module.pretty_print(db);
        }
        println!();
    }

    time! { print_times, "find main", passes::find_main(db) };
    bail_if_failed!(db);

    // let mut hir_modules = time! { print_times, "hir -> mir", mir::lower(db, hir_modules) };
    // bail_if_failed!(db);

    codegen(db, hir);
}

fn codegen(db: &mut Database, hir_modules: Vec<hir::Module>) {
    let print_times = db.build_options().print_times;

    let out_dir = Path::new("out");
    let main_module_name = db.main_module().unwrap().name.name();
    let out_file_name = out_dir.join(main_module_name.as_str());
    let out_c_file = out_file_name.with_extension("c");

    fs::create_dir_all(out_dir).unwrap();
    let mut c_file = File::create(&out_c_file).unwrap();

    time! { print_times, "codegen", codegen::codegen(&db, &hir_modules, &mut c_file) };

    time! { print_times, "clang",
        Command::new("clang")
            .args([out_c_file.to_string_lossy().as_ref(), "-o", out_file_name.to_string_lossy().as_ref(), "-x", "c", "-std=c99"])
            .spawn()
            .unwrap()
    };
}

macro_rules! bail_if_failed {
    ($db: expr) => {
        if $db.diagnostics.any() {
            $db.diagnostics.print(&$db.sources).unwrap();
            return;
        }
    };
}

use bail_if_failed;
