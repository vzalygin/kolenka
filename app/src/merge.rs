use std::{
    error::Error, fmt, fs, path::{Path, PathBuf}, process::Command, sync::{Mutex, OnceLock}
};

use lib::Context;
use tempfile::TempDir;

use crate::std_wasm::{KOLENKA_STD_WASM, WASM_MERGE_EXECUTABLE};

const STD_MODULE_NAME: &str = "kolenka_std";
const USER_MODULE_NAME: &str = "main";

const WASM_MERGE_EXECUTABLE_NAME: &str = "wasm-merge";
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

pub fn merge_with_std(user_wasm: &[u8], ctx: &mut Context) -> Result<Vec<u8>, MergeError> {
    merge_modules(user_wasm, KOLENKA_STD_WASM, ctx)
}

fn merge_modules(user_wasm: &[u8], std_wasm: &[u8], ctx: &mut Context) -> Result<Vec<u8>, MergeError> {
    
    ctx.emit_debug("---\nwasm-merge call");

    let temp_dir = TempDir::new().map_err(|source| MergeError::Io {
        path: std::env::temp_dir(),
        source,
    })?;

    let std_path = temp_dir.path().join("kolenka_std.wasm");
    let user_path = temp_dir.path().join("main.wasm");
    let output_path = temp_dir.path().join("out.wasm");

    let wasm_exe_path = temp_dir.path().join(WASM_MERGE_EXECUTABLE_NAME);

    fs::write(&wasm_exe_path, WASM_MERGE_EXECUTABLE).map_err(|source| MergeError::Io {
        path: std::env::temp_dir(),
        source,
    })?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;

        let mut perms = fs::metadata(&wasm_exe_path).map_err(|source| MergeError::Io {
            path: std::env::temp_dir(),
            source,
        })?.permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&wasm_exe_path, perms).map_err(|source| MergeError::Io {
            path: std::env::temp_dir(),
            source,
        })?;
    }

    write_file(&std_path, std_wasm)?;
    write_file(&user_path, user_wasm)?;

    let mut args = vec![
        "-o".to_string(),
        output_path.to_string_lossy().into_owned(),
    ];
    args.extend(WASM_FEATURE_FLAGS.iter().map(|flag| flag.to_string()));
    args.extend([
        user_path.to_string_lossy().into_owned(),
        USER_MODULE_NAME.to_string(),
        std_path.to_string_lossy().into_owned(),
        STD_MODULE_NAME.to_string(),
    ]);

    ctx.emit_debug(format!("wasm-merge dir: {}", temp_dir.path().display()));
    ctx.emit_debug(format!("wasm-merge args: {}", args.join(" ")));

    let exit_status = Command::new(wasm_exe_path)
        .args(args)
        .output()
        .map_err(|source| MergeError::Io { path: std::env::temp_dir(), source })?
        .status;

    ctx.emit_debug(format!("wasm-merge exit-status: {:?}", exit_status));

    let result = read_file(&output_path).map_err(|err| match err {
        MergeError::Io { .. } => MergeError::Failed { exit_code: exit_status.code().unwrap_or(0) },
        err => err,
    });

    result
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
