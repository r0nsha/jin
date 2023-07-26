mod ast;
mod codegen;
mod diagnostics;
mod lexer;
mod parser;
mod span;
mod state;
mod ty;
mod typecheck;
mod util;

use std::{fs, os::unix::process::CommandExt, path::PathBuf, process::Command};

use clap::{Parser, Subcommand};
use diagnostics::CompilerReport;
use state::CompilerOptions;

use crate::{codegen::codegen, state::State, typecheck::typecheck};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    cmd: Commands,

    #[arg(short, long, default_value_t = true)]
    time: bool,
}

#[derive(Subcommand)]
enum Commands {
    Build { file: PathBuf },
}

pub type CompilerResult<T> = std::result::Result<T, CompilerReport>;

fn main() -> color_eyre::eyre::Result<()> {
    color_eyre::install()?;

    let cli = Cli::parse();

    let mut state = State::new(CompilerOptions { time: cli.time });

    let result = match cli.cmd {
        Commands::Build { file } => build(&mut state, file),
    };

    if let Err(report) = result {
        report
            .eprint(&state.source_cache)
            .expect("diagnostics to work");
    }

    Ok(())
}

fn build(state: &mut State, file: PathBuf) -> CompilerResult<()> {
    // TODO: handle error
    let source_key = state.source_cache.add_file(file).unwrap();
    let source = state.source_cache.get(source_key).unwrap();

    let tokens = time! { state.options.time, "lexer", lexer::tokenize(source)? };
    let module = time! { state.options.time, "parser", parser::parse(tokens)? };

    let typed_module = time! { state.options.time, "typecheck", typecheck(module)? };

    println!("Typed Ast:");
    typed_module.pretty_print().unwrap();
    println!();

    let code = time! { state.options.time, "codegen", codegen(typed_module) };

    println!("Code:");
    println!("{code}");
    println!();

    // TODO: don't create this out dir
    // TODO: handle error (ICE)
    fs::create_dir_all("out").unwrap();

    // TODO: rename file
    // TODO: handle error (ICE)
    fs::write("out/main.c", &code).unwrap();

    // TODO: rename input
    // TODO: rename output
    // TODO: handle error (ICE)
    time! { state.options.time, "clang",
        Command::new("clang")
            .args(["out/main.c", "-o", "out/main", "-x", "c", "-std=c99"])
            .exec()
    };

    Ok(())
}
