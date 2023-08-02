mod ast;
mod check;
mod codegen;
mod common;
mod db;
mod diagnostics;
mod hir;
mod parse_modules;
mod parser;
mod scopes;
mod span;
mod tokenize;
mod ty;
mod util;

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use db::{BuildOptions, Database};
use parse_modules::parse_modules;

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

    let modules = time! { print_times, "ast generation", parse_modules(db) };

    if db.diagnostics.any() {
        db.diagnostics.print(&db.sources).unwrap();
        return;
    }

    if db.build_options().print_ast {
        println!("Ast:");
        for module in &modules {
            module.pretty_print().unwrap();
        }
    }

    // let hir_cache = time! { print_times, "check", check(state, modules)? };
    //
    // if state.build_options().print_hir {
    //     println!();
    //     hir_cache.pretty_print().unwrap();
    //     println!();
    // }
    //
    // let code = time! { print_times, "codegen", codegen(typed_module) };
    //
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
