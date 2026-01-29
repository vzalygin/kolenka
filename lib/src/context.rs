use std::io;

use crate::error::CompilerError;

pub struct Context {
    output: dyn io::Write,
}

impl Context {
    pub fn emit_err(&mut self, err: CompilerError) {
        writeln!(self.output, "ERR {}", err);
    }

    pub fn emit_info(&mut self, msg: impl Into<String>) {
        writeln!(self.output, "INFO {}", msg.into());
    }

    pub fn emit_debug(&mut self, msg: impl Into<String>) {
        writeln!(self.output, "DEBUG {}", msg.into());
    }
}
