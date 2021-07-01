extern crate clap;
extern crate env_logger;
extern crate lexpr;
extern crate log;

use std::fs::{File, OpenOptions};
use std::io::{Read, Write};

use log::debug;

mod ast;
mod ident;
//mod ir;
//mod scope;
mod util;

fn main() {
    use ast::{Node, Visitor};
    use clap::Arg;

    env_logger::init();

    let clap = clap::App::new("myapp")
        .version("0.1").author("Naomi Nitel <naominitel@gmail.com>")
        .about("compiler for unnamed_lang")
        .arg(Arg::from_usage("<FILE>"));

    let opts = clap.get_matches();
    let input = opts.value_of("FILE").unwrap();

    let mut parser = match File::open(input) {
        Ok(file) => lexpr::Parser::from_reader(file),
        Err(err) => {
            println!("Input `{}`: I/O Error {}", input, err);
            return;
        }
    };

    let mut int = ident::Interner::new();
    let ast = parser.expect_datum();

    match ast {
        Ok(ast) => {
            println!("parsed ast: {}", ast.value());
            // convert to our ast
            let ast = ast::from(&mut int, ast.as_ref());
            debug!("parsed ast: {}", ast);
        }
        Err(err) => println!("Input `{}`: parse error {:?}", input, err),
    }
}
