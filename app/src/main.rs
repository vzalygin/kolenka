use std::{
    error::Error,
    fs,
    path::{Path, PathBuf},
    process::ExitCode,
};

use clap::Parser;
use colored::Colorize;
use lib::{Context, build_hir, generate_bytecode, parse_source};

use crate::cli::{Cli, Mode};
use crate::merge::merge_with_std;

mod cli;
mod merge;
mod std_wasm;

fn main() -> ExitCode {
    let cli = Cli::parse();

    match run(cli) {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("{} {}", "error".red(), err);
            ExitCode::FAILURE
        }
    }
}

fn run(cli: Cli) -> Result<(), Box<dyn Error>> {
    let source = fs::read_to_string(&cli.input_file)?;
    let output_file = cli
        .output
        .clone()
        .unwrap_or_else(|| default_output_file(&cli.input_file));
    let mode = cli.mode();
    let quiet = cli.quiet;
    let levels = cli.log_levels();

    let mut parser_stdout = std::io::stdout();
    let mut typing_stdout = std::io::stdout();
    let mut hir_stdout = std::io::stdout();
    let mut generator_stdout = std::io::stdout();
    let mut parser_context = Context::new(&mut parser_stdout, levels.parser);
    let mut typing_context = Context::new(&mut typing_stdout, levels.analyzer);
    let mut hir_context = Context::new(&mut hir_stdout, levels.analyzer);
    let mut generator_context = Context::new(&mut generator_stdout, levels.codegen);

    let ast = parse_source(&source, &mut parser_context)?;
    let program = build_hir(&ast, &mut typing_context, &mut hir_context)?;

    if mode == Mode::Compile {
        let bytecode = generate_bytecode(&program, &mut generator_context);
        let bytecode = merge_with_std(&bytecode)?;
        fs::write(output_file, bytecode)?;
    }

    Ok(())
}

fn default_output_file(input_file: &Path) -> PathBuf {
    input_file.with_extension("wasm")
}
