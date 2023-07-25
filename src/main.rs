mod ast;
mod codegen;
mod diagnostics;
mod lexer;
mod parser;
mod span;
mod state;
mod ty;
mod typecheck;

use std::{fs, os::unix::process::CommandExt, path::PathBuf, process::Command};

use clap::{Parser, Subcommand};
use diagnostics::CompilerReport;

use crate::{codegen::codegen, state::State, typecheck::typecheck};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    cmd: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Build { file: PathBuf },
}

pub type CompilerResult<T> = std::result::Result<T, CompilerReport>;

fn main() -> color_eyre::eyre::Result<()> {
    color_eyre::install()?;

    let cli = Cli::parse();

    let mut state = State::new();

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

// fn handle_command(state: &State, mut cmd: impl FnMut() -> Result<()>) {
//     if let Err(report) = cmd() {
//         report.print(&state.source_cache);
//     }
// }

fn build(state: &mut State, file: PathBuf) -> CompilerResult<()> {
    // TODO: handle error
    let source_key = state.source_cache.add_file(file).unwrap();
    let source = state.source_cache.get(source_key).unwrap();

    let tokens = lexer::tokenize(source);
    let module = parser::parse(tokens)?;

    // TODO: handle error
    let typed_module = typecheck(module).unwrap();

    println!("Typed Ast:");
    typed_module.pretty_print().unwrap();
    println!();

    let code = codegen(typed_module);

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
    Command::new("clang")
        .args(["out/main.c", "-o", "out/main", "-x", "c", "-std=c99"])
        .exec();

    Ok(())
}
