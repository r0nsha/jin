mod ast;
mod codegen;
mod name_resolution;
mod parser;
mod span;
mod state;
mod tokenize;
mod ty;
mod typecheck;
mod util;

use std::{fs, path::PathBuf, process::Command};

use clap::{Parser, Subcommand};
use state::CompilerOptions;

use crate::{codegen::codegen, state::State, typecheck::typecheck};

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
    print_typed_ast: bool,
}

#[derive(Subcommand)]
enum Commands {
    Build { file: PathBuf },
}

pub type CompilerResult<T, E = miette::Report> = miette::Result<T, E>;

fn main() -> CompilerResult<()> {
    miette::set_panic_hook();

    let cli = Cli::parse();

    let mut state = State::new(CompilerOptions {
        print_times: cli.print_times,
        print_ast: cli.print_ast,
        print_typed_ast: cli.print_typed_ast,
    });

    match cli.cmd {
        Commands::Build { file } => build(&mut state, file),
    }
}

fn build(state: &mut State, file: PathBuf) -> CompilerResult<()> {
    // TODO: handle error
    let source_id = state.source_cache.add_file(file).unwrap();
    let source = state.source_cache.get(source_id).unwrap();

    let print_times = state.options().print_times;

    let tokens = time! { print_times, "tokenize", tokenize::tokenize(&state, source)? };
    let module = time! { print_times, "parser", parser::parse(&state, tokens)? };

    if state.options().print_ast {
        println!("Ast:");
        module.pretty_print().unwrap();
        println!();
    }

    let resolved_module =
        time! { print_times, "name resolution", name_resolution::resolve(&state, module)? };

    let typed_module = time! { print_times, "typecheck", typecheck(&state, resolved_module)? };

    if state.options().print_typed_ast {
        println!("Typed Ast:");
        typed_module.pretty_print().unwrap();
        println!();
    }

    let code = time! { print_times, "codegen", codegen(typed_module) };

    // TODO: don't create this out dir
    // TODO: handle error (ICE)
    fs::create_dir_all("out").unwrap();

    // TODO: rename file
    // TODO: handle error (ICE)
    fs::write("out/main.c", &code).unwrap();

    // TODO: rename input
    // TODO: rename output
    // TODO: handle error (ICE)
    time! { print_times, "clang",
        Command::new("clang")
            .args(["out/main.c", "-o", "out/main", "-x", "c", "-std=c99"])
            .spawn()
            .unwrap()
    };

    Ok(())
}
