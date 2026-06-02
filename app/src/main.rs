use colored::Colorize;
use std::{
    env, fs::File, io::{self, Write}
};

use lib::{CompilerError, Context, LogLevel, build_hir, generate_bytecode, parse_source};

fn main() {
    // wasm::module();
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 {
        compile_void(args[1].clone());
        return;
    }

    loop {
        print!("{} ", "ready :>".cyan());
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("read line error");

        compile_void(input);
    }
}

fn compile_void(source: String) {
    match compile(source) {
        Ok(_) => println!("{}", "ok".green()),
        Err(e) => println!("{} {}", "error".red(), e),
    }
}

fn compile(source: String) -> Result<(), CompilerError> {
    let mut parser_stdout = std::io::stdout();
    let mut typing_stdout = std::io::stdout();
    let mut hir_stdout = std::io::stdout();
    let mut generator_stdout = std::io::stdout();
    let mut parser_context = Context::new(&mut parser_stdout, LogLevel::Debug);
    let mut typing_context = Context::new(&mut typing_stdout, LogLevel::Debug);
    let mut hir_context = Context::new(&mut hir_stdout, LogLevel::Debug);
    let mut generator_context = Context::new(&mut generator_stdout, LogLevel::Debug);

    let ast = parse_source(&source, &mut parser_context)?;
    let program = build_hir(&ast, &mut typing_context, &mut hir_context)?;
    let bytecode = generate_bytecode(&program, &mut generator_context);

    let mut file = File::create("test.wasm").unwrap();
    file.write_all(&bytecode).unwrap();


    Ok(())
}
