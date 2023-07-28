mod ast;
mod check;
mod codegen;
mod diagnostics;
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
use codespan_reporting::term::{
    self,
    termcolor::{ColorChoice, StandardStream},
};
use diagnostics::Diagnostic;
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
    Run { file: PathBuf },
}

pub type CompilerResult<T> = Result<T, Diagnostic>;

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

            if let Some(output_file) = build(build_options, file) {
                // let _ = Command::new(output_file).spawn();
            }
        }
    }
}

fn build(build_options: BuildOptions, file: PathBuf) {
    let mut state = State::new(build_options, file).unwrap();

    match build_inner(&mut state, build_options, file) {
        Ok(output_file) => {
            // let _ = Command::new(output_file).spawn();
        }
        Err(diagnostic) => emit_diagnostics(&state, diagnostic).unwrap(),
    }
}

fn build_inner(
    state: &mut State,
    build_options: BuildOptions,
    file: PathBuf,
) -> CompilerResult<()> {
    let print_times = build_options.print_times;

    let modules = time! { print_times, "ast generation", ast::gen(&state)? };

    if state.build_options().print_ast {
        for module in &modules {
            println!();
            println!("Ast:");
            module.pretty_print().unwrap();
            println!();
        }
    }

    let hir_cache = time! { print_times, "check", check(&state, modules)? };

    if state.build_options().print_hir {
        println!();
        hir_cache.pretty_print().unwrap();
        println!();
    }

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

fn emit_diagnostics(
    state: &State,
    diagnostic: Diagnostic,
) -> Result<(), codespan_reporting::files::Error> {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    term::emit(
        &mut writer.lock(),
        &config,
        &state.source_cache,
        &diagnostic.into(),
    )
}
