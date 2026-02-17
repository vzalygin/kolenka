mod context;
mod error;
mod parser;
mod typing;

pub use crate::{
    context::{Context, LogLevel},
    error::CompilerError,
    parser::{Ast, parse_source},
    typing::{Type, infer_ast},
};

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
