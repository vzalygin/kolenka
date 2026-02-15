use std::io;
use colored::{Colorize, ColoredString};

use crate::error::CompilerError;

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum LogLevel {
    Debug,
    Warn,
    Error,
    Never,
}

pub trait ContextWriter: io::Write {}
impl ContextWriter for io::Stdout {}

pub struct Context {
    write: Box<dyn ContextWriter>,
    log_level: LogLevel,
}

impl Context {
    pub fn new<W: ContextWriter + 'static>(write: W, log_level: LogLevel) -> Context {
        Context {
            write: Box::new(write),
            log_level: log_level,
        }
    }

    // pub(crate) fn deeper(&self) -> Context {
    //     Context {
    //         write: self.write.clone_box(),
    //         log_level: self.log_level.clone(),
    //         tab: self.tab.clone() + "\t",
    //     }
    // }

    pub(crate) fn emit_err(&mut self, err: &CompilerError) {
        if self.log_level <= LogLevel::Error {
            writeln!(self.write, "{} {}", "error".red(), err).expect("Cannot emit err log");
        }
    }

    pub(crate) fn emit_warn(&mut self, msg: impl Into<String>) {
        if self.log_level <= LogLevel::Warn {
            writeln!(self.write, "{} {}", "warn ".yellow(), msg.into()).expect("cannot emit warn log");
        }
    }

    pub(crate) fn emit_debug(&mut self, msg: impl Into<String>) {
        if self.log_level <= LogLevel::Debug {
            writeln!(self.write, "{} {}", "debug".truecolor(0, 10, 10), msg.into()).expect("cannot emit debug log");
        }
    }
}
