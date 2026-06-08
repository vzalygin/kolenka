pub static KOLENKA_STD_WASM: &[u8] = include_bytes!(
    "../../target/wasm32-wasip1/release/kolenka_std.wasm"
);

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
pub static WASM_MERGE_EXECUTABLE: &[u8] =
    include_bytes!("../../assets/wasm-merge-linux-x86_64");

#[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
compile_error!("only linux x86_64 supported");
