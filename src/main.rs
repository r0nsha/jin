mod ast;
mod codegen;
mod lexer;
mod parser;
mod span;
mod state;
mod ty;
mod typecheck;

use std::{fs, os::unix::process::CommandExt, path::PathBuf, process::Command};

use clap::{Parser, Subcommand};
use miette::Result;

use crate::{codegen::codegen, state::State, typecheck::typecheck};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    cmd: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Run { file: PathBuf },
}

fn main() -> Result<()> {
    miette::set_panic_hook();

    let cli = Cli::parse();

    match cli.cmd {
        Commands::Run { file } => {
            let mut state = State::new();

            // TODO: handle error
            let file_name = file.to_str().unwrap().to_string();
            // TODO: handle error
            let file_source = fs::read_to_string(file).unwrap();

            let source = state.source_map.add_source(file_name, file_source);

            let tokens = lexer::tokenize(source.as_ref());
            let module = parser::parse(source.clone(), tokens)?;

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
        }
    }

    Ok(())
}
