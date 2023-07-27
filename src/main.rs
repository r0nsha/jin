mod ast;
mod check;
mod codegen;
mod hir;
mod parser;
mod scopes;
mod span;
mod state;
mod tokenize;
mod ty;
mod util;

use std::{fs, path::PathBuf, process::Command};

use clap::{Parser, Subcommand};
use state::BuildOptions;

use crate::{check::check, state::State};

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
}

pub type CompilerResult<T, E = miette::Report> = miette::Result<T, E>;

fn main() -> CompilerResult<()> {
    miette::set_panic_hook();

    let cli = Cli::parse();

    let build_options = BuildOptions {
        print_times: cli.print_times,
        print_ast: cli.print_ast,
        print_hir: cli.print_hir,
    };

    match cli.cmd {
        Commands::Build { file } => build(build_options, file),
    }
}

fn build(build_options: BuildOptions, file: PathBuf) -> CompilerResult<()> {
    let print_times = build_options.print_times;

    let state = State::new(build_options, file).unwrap();

    let modules = time! { print_times, "ast generation", ast::gen(&state)? };

    if state.build_options().print_ast {
        for module in &modules {
            println!("Ast:");
            module.pretty_print().unwrap();
            println!();
        }
    }

    let hir = time! { print_times, "check", check(&state, modules)? };

    // TODO: print Hir
    // if state.build_options().print_hir {
    //     println!("Hir:");
    //     hir.pretty_print().unwrap();
    //     println!();
    // }

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

    Ok(())
}
