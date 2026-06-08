use std::{
    error::Error,
    fmt,
    fs,
    path::{Path, PathBuf},
    sync::{Mutex, OnceLock},
};

use tempfile::{TempDir, TempPath};
use wasm_merge_sys::run_wasm_merge;

use crate::std_wasm::KOLENKA_STD_WASM;

const STD_MODULE_NAME: &str = "kolenka_std";
const USER_MODULE_NAME: &str = "main";
const WASM_FEATURE_FLAGS: &[&str] = &[
    "--enable-bulk-memory",
    "--enable-reference-types",
    "--enable-multivalue",
    "--enable-mutable-globals",
    "--enable-sign-ext",
];

#[derive(Debug)]
pub enum MergeError {
    Io {
        path: PathBuf,
        source: std::io::Error,
    },
    Failed {
        exit_code: i32,
    },
}

impl fmt::Display for MergeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MergeError::Io { path, source } => {
                write!(f, "cannot access {}: {}", path.display(), source)
            }
            MergeError::Failed { exit_code } => {
                write!(f, "wasm-merge failed with exit code {}", exit_code)
            }
        }
    }
}

impl Error for MergeError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            MergeError::Io { source, .. } => Some(source),
            MergeError::Failed { .. } => None,
        }
    }
}

pub fn merge_with_std(user_wasm: &[u8]) -> Result<Vec<u8>, MergeError> {
    merge_modules(user_wasm, KOLENKA_STD_WASM)
}

fn merge_modules(user_wasm: &[u8], std_wasm: &[u8]) -> Result<Vec<u8>, MergeError> {
    let temp_dir = TempDir::new().map_err(|source| MergeError::Io {
        path: std::env::temp_dir(),
        source,
    })?;
    let debug = merge_debug_enabled();

    let std_path = temp_dir.path().join("kolenka_std.wasm");
    let user_path = temp_dir.path().join("main.wasm");
    let output_path = temp_dir.path().join("out.wasm");

    write_file(&std_path, std_wasm)?;
    write_file(&user_path, user_wasm)?;

    let mut args = vec![
        "wasm-merge".to_string(),
        "-o".to_string(),
        output_path.to_string_lossy().into_owned(),
    ];
    if debug {
        args.push("--debug".to_string());
    }
    args.extend(WASM_FEATURE_FLAGS.iter().map(|flag| flag.to_string()));
    args.extend([
        user_path.to_string_lossy().into_owned(),
        USER_MODULE_NAME.to_string(),
        std_path.to_string_lossy().into_owned(),
        STD_MODULE_NAME.to_string(),
    ]);

    if debug {
        println!("wasm-merge debug dir: {}", temp_dir.path().display());
        println!("wasm-merge args: {}", args.join(" "));
    }

    let exit_code = run_locked_wasm_merge(&args);
    let result = read_file(&output_path).map_err(|err| match err {
        MergeError::Io { .. } => MergeError::Failed { exit_code },
        err => err,
    });

    if debug {
        keep_temp_dir(temp_dir);
    }

    result
}

fn run_locked_wasm_merge(args: &[String]) -> i32 {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();

    let _guard = LOCK.get_or_init(|| Mutex::new(())).lock().unwrap();
    run_wasm_merge(args)
}

fn write_file(path: &Path, bytes: &[u8]) -> Result<(), MergeError> {
    fs::write(path, bytes).map_err(|source| MergeError::Io {
        path: path.to_path_buf(),
        source,
    })
}

fn read_file(path: &Path) -> Result<Vec<u8>, MergeError> {
    fs::read(path).map_err(|source| MergeError::Io {
        path: path.to_path_buf(),
        source,
    })
}

fn merge_debug_enabled() -> bool {
    matches!(
        std::env::var("KOLENKA_WASM_MERGE_DEBUG").as_deref(),
        Ok("1" | "true" | "yes" | "on")
    )
}

fn keep_temp_dir(temp_dir: TempDir) {
    let path = TempPath::from_path(temp_dir.into_path());
    path.keep()
        .expect("cannot keep wasm-merge debug directory");
}

#[cfg(test)]
mod tests {
    use std::sync::OnceLock;

    use lib::{build_hir, generate_bytecode, parse_source, Context, LogLevel};
    use wasmparser::{ExternalKind, Imports, Parser, Payload, Validator};

    use super::*;

    fn generated_program_wasm() -> Vec<u8> {
        let mut parser_output = Vec::new();
        let mut typing_output = Vec::new();
        let mut hir_output = Vec::new();
        let mut codegen_output = Vec::new();
        let mut parser_context = Context::new(&mut parser_output, LogLevel::Never);
        let mut typing_context = Context::new(&mut typing_output, LogLevel::Never);
        let mut hir_context = Context::new(&mut hir_output, LogLevel::Never);
        let mut codegen_context = Context::new(&mut codegen_output, LogLevel::Never);

        let ast = parse_source("1 print", &mut parser_context).unwrap();
        let hir = build_hir(&ast, &mut typing_context, &mut hir_context).unwrap();

        generate_bytecode(&hir, &mut codegen_context)
    }

    fn merged_program_wasm() -> Vec<u8> {
        static MERGED: OnceLock<Vec<u8>> = OnceLock::new();

        MERGED
            .get_or_init(|| merge_with_std(&generated_program_wasm()).unwrap())
            .clone()
    }

    #[test]
    fn merged_module_is_valid_wasm() {
        let wasm = merged_program_wasm();

        Validator::new().validate_all(&wasm).unwrap();
    }

    #[test]
    fn merged_module_does_not_import_kolenka_std() {
        let wasm = merged_program_wasm();

        for payload in Parser::new(0).parse_all(&wasm) {
            if let Payload::ImportSection(imports) = payload.unwrap() {
                for module in import_modules(imports) {
                    assert_ne!(module, STD_MODULE_NAME);
                }
            }
        }
    }

    #[test]
    fn merged_module_keeps_wasi_imports() {
        let wasm = merged_program_wasm();
        let mut has_wasi_import = false;

        for payload in Parser::new(0).parse_all(&wasm) {
            if let Payload::ImportSection(imports) = payload.unwrap() {
                for module in import_modules(imports) {
                    if module == "wasi_snapshot_preview1" {
                        has_wasi_import = true;
                    }
                }
            }
        }

        assert!(has_wasi_import);
    }

    #[test]
    fn merged_module_exports_start() {
        let wasm = merged_program_wasm();
        let mut has_start = false;

        for payload in Parser::new(0).parse_all(&wasm) {
            if let Payload::ExportSection(exports) = payload.unwrap() {
                for export in exports {
                    let export = export.unwrap();
                    if export.name == "_start" && export.kind == ExternalKind::Func {
                        has_start = true;
                    }
                }
            }
        }

        assert!(has_start);
    }

    fn import_modules(imports: wasmparser::ImportSectionReader<'_>) -> Vec<&str> {
        imports
            .into_iter()
            .flat_map(|imports| match imports.unwrap() {
                Imports::Single(_, import) => vec![import.module],
                Imports::Compact1 { module, .. } | Imports::Compact2 { module, .. } => {
                    vec![module]
                }
            })
            .collect()
    }
}
