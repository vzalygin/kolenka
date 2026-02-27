//! Набор функций стандартной библиотеки языка

use std::io::Read;

#[unsafe(export_name = "read_i32")]
pub extern "C" fn read_i32() -> i32 {
    let mut input = [0u8; 256];
    loop {
        let nread = std::io::stdin().read(&mut input).unwrap();
        let input = std::str::from_utf8(&input[..nread]).unwrap();
        let mut iter = input.split_whitespace();
        for num in iter {
            if let Ok(num) = num.parse::<i32>() {
                return num;
            }
        }
    }
}

#[unsafe(export_name = "print_i32")]
pub extern "C" fn print_i32(num: i32) {
    println!("{}", num);
}
