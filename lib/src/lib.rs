mod codegen;
mod context;
mod error;
mod hir;
mod id;
mod parser;
mod typing;

pub use crate::{
    context::{Context, LogLevel},
    error::CompilerError,
    hir::generate_hir,
    id::ProgramId,
    parser::{Ast, parse_source},
    typing::Type,
};

static MAIN_FN_NAME: &str = "$main";

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
