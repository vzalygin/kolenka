//! Модуль контекста исполнения кода

pub(crate) static MAIN_FN_NAME: &str = "&_start";
pub(crate) static WASM_MAIN_FN_NAME: &str = "_start";

// std
pub(crate) static STD_READ_FN_NAME: &str = "read";
pub(crate) static STD_PRINT_FN_NAME: &str = "print";
pub(crate) static WASM_STD_MODULE_NAME: &str = "kolenka_std";
pub(crate) static WASM_STD_READ_FN_NAME: &str = "read_i32";
pub(crate) static WASM_STD_PRINT_FN_NAME: &str = "print_i32";

pub(crate) static STD_FNS: [&str; 2] = [STD_READ_FN_NAME, STD_PRINT_FN_NAME];
