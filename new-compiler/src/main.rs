#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub parser);

mod ast;

fn main() {
    println!("Hello, world!");
}
