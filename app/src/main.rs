use colored::Colorize;
use std::io::{self, Write};

use lib::{CompilerError, Context, LogLevel, infer_ast, parse_source};

fn main() {
    loop {
        print!("{} ", "ready :>".cyan());
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("read line error");

        match compile(input) {
            Ok(_) => println!("{}", "ok".green()),
            Err(e) => println!("{} {}", "error".red(), e),
        }
    }
}

fn compile(source: String) -> Result<(), CompilerError> {
    let mut parser_context = Context::new(std::io::stdout(), LogLevel::Debug);
    let mut typing_context = Context::new(std::io::stdout(), LogLevel::Debug);

    let ast = parse_source(&source, &mut parser_context)?;

    let prog_type = infer_ast(&ast, &mut typing_context)?;

    Ok(())
}
