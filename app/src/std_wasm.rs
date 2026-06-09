pub static KOLENKA_STD_WASM_BYTES: &[u8] = include_bytes!(
    "../../target/wasm32-wasip1/release/kolenka_std.wasm"
);

pub static WASM_MERGE_EXECUTABLE_BYTES: &[u8] =
    include_bytes!("../../assets/wasm-merge-linux-x86_64");
