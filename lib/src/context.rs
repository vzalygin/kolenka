use std::io;

use crate::error::CompilerError;

pub struct Context {
    output: Box<dyn io::Write>,
}

impl Context {
    pub fn new(output: Box<dyn io::Write>) -> Context {
        Context { output }
    }

    pub(crate) fn emit_err(&mut self, err: CompilerError) {
        let _ = writeln!(self.output, "ERR {}", err);
    }

    pub(crate) fn emit_info(&mut self, msg: impl Into<String>) {
        let _ = writeln!(self.output, "INFO {}", msg.into());
    }

    pub(crate) fn emit_debug(&mut self, msg: impl Into<String>) {
        let _ = writeln!(self.output, "DEBUG {}", msg.into());
    }
}
