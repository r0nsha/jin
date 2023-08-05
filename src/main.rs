mod codegen;
mod common;
mod db;
mod diagnostics;
mod hir;
mod parse;
mod passes;
mod span;
mod ty;

use std::path::PathBuf;

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

    let ast_modules = time! { print_times, "ast generation", parse::parse_modules(db) };

    if db.build_options().print_ast {
        println!("Ast:");
        for module in &ast_modules {
            module.pretty_print().unwrap();
        }
    }

    bail_if_failed!(db);

    let mut hir_modules = time! { print_times, "ast -> hir", hir::lower(db, ast_modules) };

    time! { print_times, "resolve", passes::resolve(db, &mut hir_modules) };
    bail_if_failed!(db);

    time! { print_times, "infer", passes::infer(db, &mut hir_modules) };
    bail_if_failed!(db);

    if db.build_options().print_hir {
        println!("\nHir:\n");
        for module in &hir_modules {
            module.pretty_print(db);
        }
        println!();
    }

    time! { print_times, "find main", passes::find_main(db) };
    bail_if_failed!(db);

    time! { print_times, "codegen", codegen::codegen(&hir_modules) };

    // // TODO: don't create this out dir
    // // TODO: handle error (ICE)
    // fs::create_dir_all("out").unwrap();
    //
    // // TODO: rename file
    // // TODO: handle error (ICE)
    // fs::write("out/main.c", &code).unwrap();
    //
    // // TODO: rename input
    // // TODO: rename output
    // // TODO: handle error (ICE)
    // time! { print_times, "clang",
    //     Command::new("clang")
    //         .args(["out/main.c", "-o", "out/main", "-x", "c", "-std=c99"])
    //         .spawn()
    //         .unwrap()
    // };
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
