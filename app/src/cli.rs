use std::path::PathBuf;

use clap::{ArgGroup, Parser, ValueEnum};
use lib::LogLevel;

#[derive(Debug, Parser)]
#[command(name = "kolenka")]
#[command(version, about = "Compile Cat source files to WebAssembly modules")]
#[command(group(
    ArgGroup::new("mode")
        .args(["verificate", "compile"])
        .multiple(false)
))]
pub struct Cli {
    #[arg(short = 'f', long = "verificate", help = "Check the program without generating WebAssembly")]
    pub verificate: bool,

    #[arg(short = 'c', long = "compile", help = "Compile the program to WebAssembly")]
    pub compile: bool,

    #[arg(short = 'q', long = "quiet", help = "Disable diagnostic output")]
    pub quiet: bool,

    #[arg(short = 'o', long = "output", value_name = "FILE", help = "Write WebAssembly bytecode to FILE")]
    pub output: Option<PathBuf>,

    #[arg(long = "log", value_name = "LEVEL", value_enum, help = "Set the common diagnostic level")]
    pub log: Option<CliLogLevel>,

    #[arg(long = "log-parser", value_name = "LEVEL", value_enum, help = "Set parser diagnostic level")]
    pub log_parser: Option<CliLogLevel>,

    #[arg(long = "log-analyzer", value_name = "LEVEL", value_enum, help = "Set analyzer diagnostic level")]
    pub log_analyzer: Option<CliLogLevel>,

    #[arg(long = "log-codegen", value_name = "LEVEL", value_enum, help = "Set code generator diagnostic level")]
    pub log_codegen: Option<CliLogLevel>,

    #[arg(value_name = "INPUT_FILE")]
    pub input_file: PathBuf,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, ValueEnum)]
pub enum CliLogLevel {
    Debug,
    Warn,
    Error,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Mode {
    Verificate,
    Compile,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LogLevels {
    pub parser: LogLevel,
    pub analyzer: LogLevel,
    pub codegen: LogLevel,
}

impl Cli {
    pub fn mode(&self) -> Mode {
        if self.verificate {
            Mode::Verificate
        } else {
            Mode::Compile
        }
    }

    pub fn log_levels(&self) -> LogLevels {
        if self.quiet {
            return LogLevels {
                parser: LogLevel::Never,
                analyzer: LogLevel::Never,
                codegen: LogLevel::Never,
            };
        }

        let common = self.log.unwrap_or(CliLogLevel::Warn);

        LogLevels {
            parser: self.log_parser.unwrap_or(common).into(),
            analyzer: self.log_analyzer.unwrap_or(common).into(),
            codegen: self.log_codegen.unwrap_or(common).into(),
        }
    }
}

impl From<CliLogLevel> for LogLevel {
    fn from(value: CliLogLevel) -> Self {
        match value {
            CliLogLevel::Debug => LogLevel::Debug,
            CliLogLevel::Warn => LogLevel::Warn,
            CliLogLevel::Error => LogLevel::Error,
        }
    }
}

#[cfg(test)]
mod tests {
    use clap::{CommandFactory, Parser};

    use super::*;

    fn parse(args: &[&str]) -> Cli {
        Cli::try_parse_from(args).unwrap()
    }

    #[test]
    fn compiles_by_default_with_input_file_only() {
        let cli = parse(&["kolenka", "program.cat"]);

        assert_eq!(cli.mode(), Mode::Compile);
        assert_eq!(cli.input_file, PathBuf::from("program.cat"));
    }

    #[test]
    fn parses_verificate_mode() {
        let cli = parse(&["kolenka", "--verificate", "program.cat"]);

        assert_eq!(cli.mode(), Mode::Verificate);
    }

    #[test]
    fn parses_compile_mode() {
        let cli = parse(&["kolenka", "--compile", "program.cat"]);

        assert_eq!(cli.mode(), Mode::Compile);
    }

    #[test]
    fn rejects_conflicting_modes() {
        let result = Cli::try_parse_from(["kolenka", "--verificate", "--compile", "program.cat"]);

        assert!(result.is_err());
    }

    #[test]
    fn parses_output_file() {
        let cli = parse(&["kolenka", "--compile", "program.cat", "--output", "result.wasm"]);

        assert_eq!(cli.output, Some(PathBuf::from("result.wasm")));
    }

    #[test]
    fn quiet_disables_all_logs() {
        let cli = parse(&["kolenka", "--quiet", "--log", "debug", "program.cat"]);

        assert_eq!(
            cli.log_levels(),
            LogLevels {
                parser: LogLevel::Never,
                analyzer: LogLevel::Never,
                codegen: LogLevel::Never,
            }
        );
    }

    #[test]
    fn common_log_level_is_used_for_all_components() {
        let cli = parse(&["kolenka", "--log", "debug", "program.cat"]);

        assert_eq!(
            cli.log_levels(),
            LogLevels {
                parser: LogLevel::Debug,
                analyzer: LogLevel::Debug,
                codegen: LogLevel::Debug,
            }
        );
    }

    #[test]
    fn component_log_levels_override_common_level() {
        let cli = parse(&[
            "kolenka",
            "--log",
            "error",
            "--log-parser",
            "debug",
            "--log-analyzer",
            "warn",
            "--log-codegen",
            "error",
            "program.cat",
        ]);

        assert_eq!(
            cli.log_levels(),
            LogLevels {
                parser: LogLevel::Debug,
                analyzer: LogLevel::Warn,
                codegen: LogLevel::Error,
            }
        );
    }

    #[test]
    fn rejects_unknown_log_level() {
        let result = Cli::try_parse_from(["kolenka", "--log", "trace", "program.cat"]);

        assert!(result.is_err());
    }

    #[test]
    fn rejects_documented_quite_typo() {
        let result = Cli::try_parse_from(["kolenka", "--quite", "program.cat"]);

        assert!(result.is_err());
    }

    #[test]
    fn cli_definition_is_valid() {
        Cli::command().debug_assert();
    }
}
