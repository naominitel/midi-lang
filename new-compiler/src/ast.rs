use crate::ident::{Ident, Interner};
use lexpr::{datum, Number};
use lexpr::parse::Span;
use lexpr::value::Value;

// our own sexpr type which embeds the info we want

pub struct Datum {
    node: Box<Node>,
    span: Span,
}

pub enum Node {
    Null,
    Num(Number),
    Symbol(Ident),
    Cons(Datum, Datum)
}

impl Datum {
    fn iter(&self) -> DatIter<'_> {
        DatIter { dat: self }
    }
}

pub struct DatIter<'a> {
    dat: &'a Datum
}

impl<'a> Iterator for DatIter<'a> {
    type Item = &'a Datum;

    fn next(&mut self) -> Option<&'a Datum> {
        match &*self.dat.node {
            Node::Null => None,
            Node::Cons(ref car, ref cdr) => {
                self.dat = cdr;
                Some(car)
            }
            _ => panic!("non-cons item in cdr pos")
        }
    }
}

pub fn from(int: &mut Interner, dat: lexpr::datum::Ref) -> Datum {
    Datum {
        span: dat.span(),
        node: Box::new(match dat.as_pair() {
            Some((car, cdr)) =>
                Node::Cons(from(int, car), from(int, cdr)),
            None => match dat.value() {
                Value::Null => Node::Null,
                // FIXME: clone?
                Value::Number(n) => Node::Num(n.clone()),
                Value::Symbol(s) => Node::Symbol(int.get(&s)),
                v => panic!("unsupported value {}", v)
            }
        }),
    }
}

#[allow(path_statements)]
pub trait Visitor {
    fn visit_cons(&mut self, car: &mut Datum, cdr: &mut Datum) {
        car.visit(self);
        cdr.visit(self);
    }

    fn visit_symbol(&mut self, var: &mut Ident) { var; }
    fn visit_num(&mut self, num: &mut Number) { num; }
    fn visit_null(&mut self) { () }

    // special forms
    fn visit_lambda(&mut self, args: &mut Vec<Ident>, expr: &mut Datum) {
        expr.visit(self);
    }
}

impl Datum {
    pub fn visit<V: Visitor + ?Sized>(&mut self, v: &mut V) {
        match &mut *self.node {
            Node::Cons(ref mut car, ref mut cdr) => v.visit_cons(car, cdr),
            Node::Symbol(ref mut s) => v.visit_symbol(s),
            Node::Num(n) => v.visit_num(n),
            Node::Null => v.visit_null(),
        }
    }
}

// display implementations (PP)

use std::fmt;

fn fmt_list(car: &Datum, cdr: &Datum, fmt: &mut fmt::Formatter)
    -> Result<(), fmt::Error> {
    fmt.write_fmt(format_args!("{}", car))?;
    match &*cdr.node {
        Node::Null => Ok(()),
        Node::Cons(car, cdr) => {
            fmt.write_fmt(format_args!(" "))?;
            fmt_list(car, cdr, fmt)
        }
        non_cons => {
            fmt.write_fmt(format_args!(" . "))?;
            fmt.write_fmt(format_args!("{}", cdr))
        }
    }
}

impl fmt::Display for Datum {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match &*self.node {
            Node::Null => fmt.write_fmt(format_args!("()")),
            Node::Num(n) => fmt.write_fmt(format_args!("{}", n)),
            Node::Symbol(s) => fmt.write_fmt(format_args!("{:?}", s)),
            Node::Cons(car, cdr) => {
                fmt.write_fmt(format_args!("("))?;
                fmt_list(car, cdr, fmt)?;
                fmt.write_fmt(format_args!(")"))
            }
        }
    }
}
