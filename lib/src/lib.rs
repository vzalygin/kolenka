mod codegen;
mod context;
mod error;
mod hir;
mod parser;
mod typing;
mod id;

pub use crate::{
    context::{Context, LogLevel},
    error::CompilerError,
    hir::generate_hir,
    parser::{Ast, parse_source},
    typing::Type,
    id::ProgramId
};

static MAIN_FN_NAME: &'static str = "$main";

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
