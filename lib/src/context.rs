use colored::Colorize;
use std::io;

use crate::error::CompilerError;

const LOG_SHIFT: &str = "\t";

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum LogLevel {
    Debug,
    Warn,
    Error,
    Never,
}

pub struct Context<'w> {
    writer: &'w mut dyn io::Write,
    log_level: LogLevel,
    shift: String,
}

impl<'w> Context<'w> {
    pub fn new<W: io::Write + 'static>(writer: &'w mut W, log_level: LogLevel) -> Context<'w> {
        Context {
            writer: writer,
            log_level,
            shift: "".to_string(),
        }
    }

    pub(crate) fn step(&mut self) -> Context<'_> {
        Context {
            writer: self.writer,
            log_level: self.log_level.clone(),
            shift: self.shift.clone() + LOG_SHIFT,
        }
    }

    pub(crate) fn emit_err(&mut self, err: &CompilerError) {
        if self.log_level <= LogLevel::Error {
            writeln!(self.writer, "{}{} {}", "error".red(), self.shift, err).expect("Cannot emit err log");
        }
    }

    pub(crate) fn emit_warn(&mut self, msg: impl Into<String>) {
        if self.log_level <= LogLevel::Warn {
            writeln!(self.writer, "{}{} {}", "warn ".yellow(), self.shift, msg.into())
                .expect("cannot emit warn log");
        }
    }

    pub(crate) fn emit_debug(&mut self, msg: impl Into<String>) {
        if self.log_level <= LogLevel::Debug {
            writeln!(
                self.writer,
                "{}{} {}",
                "debug".truecolor(0, 10, 10),
                self.shift,
                msg.into()
            )
            .expect("cannot emit debug log");
        }
    }
}
