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

use crate::{ast::Ast, codegen::codegen, span::Span, state::State, typecheck::typecheck};

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

fn main() {
    let cli = Cli::parse();

    match cli.cmd {
        Commands::Run { file } => {
            let mut state = State::new();

            // TODO: handle error
            let file_name = file.to_str().unwrap().to_string();
            let file_source = fs::read_to_string(file).unwrap();

            let source = state.source_map.add_source(file_name, file_source);

            let tokens = lexer::tokenize(source.as_ref());
            let ast = parser::parse(source.as_ref(), tokens);

            let (typed_module, _type_schema) = typecheck(module).unwrap();

            typed_module.pretty_print().unwrap();

            let code = codegen(&typed_module);

            println!("\n{code}");

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
}
