use colored::Colorize;
use std::io;

use crate::error::CompilerError;

const LOG_INDENT: &str = "\t";

#[derive(Clone, Copy, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum LogLevel {
    Debug,
    Warn,
    Error,
    Never,
}

pub struct Context<'w> {
    writer: &'w mut dyn io::Write,
    log_level: LogLevel,
    indent: String,
}

impl<'w> Context<'w> {
    pub fn new<W: io::Write + 'static>(writer: &'w mut W, log_level: LogLevel) -> Context<'w> {
        Context {
            writer: writer,
            log_level,
            indent: "".to_string(),
        }
    }

    pub(crate) fn step(&mut self) -> Context<'_> {
        Context {
            writer: self.writer,
            log_level: self.log_level,
            indent: self.indent.clone() + LOG_INDENT,
        }
    }

    pub(crate) fn emit_err(&mut self, err: &CompilerError) {
        if self.log_level <= LogLevel::Error {
            writeln!(self.writer, "{}{} {}", "error".red(), self.indent, err)
                .expect("cannot emit err log");
        }
    }

    pub(crate) fn emit_warn(&mut self, msg: impl Into<String>) {
        if self.log_level <= LogLevel::Warn {
            writeln!(
                self.writer,
                "{}{} {}",
                "warn ".yellow(),
                self.indent,
                msg.into()
            )
            .expect("cannot emit warn log");
        }
    }

    pub(crate) fn emit_debug(&mut self, msg: impl Into<String>) {
        if self.log_level <= LogLevel::Debug {
            writeln!(
                self.writer,
                "{}{} {}",
                "debug".truecolor(0, 10, 10),
                self.indent,
                msg.into()
            )
            .expect("cannot emit debug log");
        }
    }
}
