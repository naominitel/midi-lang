#[macro_use]
extern crate lalrpop_util;
lalrpop_mod!(pub parser);
extern crate clap;

use std::fs::{File, OpenOptions};
use std::io::{Read, Write};

mod ast;
mod ident;
mod ir;
mod scope;
mod util;

fn main() {
    use ast::{Args, ExprNode, Visitor};
    use clap::Arg;

    let clap = clap::App::new("myapp")
        .version("0.1").author("Naomi Nitel <naominitel@gmail.com>")
        .about("compiler for unnamed_lang")
        .arg(Arg::from_usage("<FILE>"));

    let opts = clap.get_matches();
    let input = opts.value_of("FILE").unwrap();

    let mut s = String::new();
    if let Err(err) = File::open(input).and_then(|mut f| f.read_to_string(&mut s)) {
        println!("Input `{}`: I/O Error {}", input, err);
        return;
    }

    let mut int = ident::Interner::new();
    let ast = parser::DefParser::new().parse(&mut int, &s);

    match ast {
        Ok(mut def) => {
            println!("Parsed: `{}`", def);

            let mut scope = scope::Scope::new(&mut int);
            scope.visit_def(&mut def);

            println!("Scoped: `{}`", def);

            let (args, ret, body) = match *def.value.node {
                ExprNode::Lambda(args, ret, body) => (args, ret, body),
                _ => panic!("toplevel def should be a function")
            };
            let fndef = ast::FnDef {
                name: def.name,
                body: body,
                inputs: Args::Named(args),
                outputs: Args::Unnamed(vec![ret]),
                locals: vec![],
                attrs: vec![]
            };
            let ir = ir::update(fndef);
            let mut out = OpenOptions::new()
                .write(true)
                .truncate(true)
                .create(true)
                .open("a.out").unwrap();
            out.write(&ir).unwrap();
        }
        Err(err) => println!("Input `{}`: parse error {:?}", input, err),
    }
}
