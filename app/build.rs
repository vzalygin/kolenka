use std::{env, path::PathBuf};

fn main() {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let std_wasm = manifest_dir
        .join("..")
        .join("target")
        .join("wasm32-wasip1")
        .join("release")
        .join("kolenka_std.wasm");

    println!("cargo:rerun-if-changed={}", std_wasm.display());

    if !std_wasm.exists() {
        panic!(
            "missing {}\nrun: cargo build -p kolenka_std --target wasm32-wasip1 --release",
            std_wasm.display()
        );
    }
}
