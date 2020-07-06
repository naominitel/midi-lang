use std::borrow::Borrow;
use std::sync::Arc;
use bytecode::{self, Value};
use primitives;

#[derive(Clone)]
pub struct Function {
    code: Arc<[Node]>,
    entry: usize,
    nargs: usize,
    env: Arc<EvalCtx>,
}

impl ::std::fmt::Debug for Function {
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "<closure>")
    }
}

impl PartialEq for Function {
    fn eq(&self, rhs: &Function) -> bool {
        self == rhs
    }
}

impl Eq for Function {
}

enum Node {
    Nop(usize),
    Glob(Arc<String>),
    Const(Value),
    Var(usize),
    // nargs, op
    Closure(usize, usize),

    If(usize, usize, usize),
    BinOp(bytecode::BinOp, usize, usize),
    UnOp(bytecode::UnOp, usize),

    Call(usize, Vec<usize>),
    PrimCall(primitives::Primitive, Vec<usize>),

    Index(usize, usize),
    Field(usize, Arc<String>),

    Poly(Vec<(usize, usize, usize)>),
    Mono(usize, usize, usize)
}

#[derive(Debug)]
struct EvalCtx {
    env: Vec<Value>,
    next: Option<Arc<EvalCtx>>
}

impl EvalCtx {
    fn env(&self, var: usize) -> Value {
        if var < self.env.len() {
            return self.env[var].clone();
        }
        if let Some(next) = &self.next {
            return next.env(var - self.env.len());
        }
        panic!("bad env index");
    }
}

macro_rules! typed_binop (
    ( $ty1:tt $v1:tt ,
      $ty2:tt $v2:tt ,
      $rop:tt , $rty:tt ) => {{
        use ::bytecode::Value::*;
        match ($v1, $v2) {
            ( $ty1 ( i1 ) , $ty2 ( i2 )) => $rty ( i1 $rop i2 ),
            (Error, _) | (_, Error) => Error,
            (_, _) => {
                // TODO: catch errors
                eprintln!("warning: invalid types for operator {}",
                          stringify!($rop));
                Error
            }
        }
    }}
);

macro_rules! poly_binop (
    ( $v1:tt , $v2:tt , $rop:tt ) => {{
        use ::bytecode::Value::*;
        match ($v1, $v2) {
            (Int ( i1 ) , Int ( i2 )) => Bool ( i1 $rop i2 ),
            (Str ( s1 ) , Str ( s2 )) => Bool ( s1 $rop s2 ),
            (Bool ( b1 ) , Bool ( b2 )) => Bool ( b1 $rop b2 ),
            (Error, _) | (_, Error) => Error,
            (_, _) => {
                // TODO: catch errors
                eprintln!("warning: invalid types for operator {}",
                          stringify!($rop));
                Error
            }
        }
    }}
);

macro_rules! typed_unop (
    ( $ty:tt $v:tt , $rop:tt , $rty:tt ) => {{
        use ::bytecode::Value::*;
        match $v {
            $ty ( i ) => $rty ( $rop i ),
            Error => Error,
            _ => {
                // TODO: catch errors
                eprintln!("warning: invalid type for operator {}",
                          stringify!($rop));
                Error
            }
        }
    }}
);

impl Function {
    fn eval(&self, op: usize, ctx: &Arc<EvalCtx>) -> Value {
        use self::Node::*;
        match self.code[op] {
            Nop(op) => self.eval(op, ctx),
            Glob(ref glob) => panic!("unimplemented: globals"),
            Const(ref v) => v.clone(),
            Var(v) => ctx.env(v),
            Closure(nargs, op) => {
                Value::Closure(Function {
                    code: self.code.clone(),
                    entry: op,
                    nargs: nargs,
                    env: ctx.clone()
                })
            }
            If(c, t, f) => {
                match self.eval(c, ctx) {
                    Value::Bool(true) => self.eval(t, ctx),
                    Value::Bool(false) => self.eval(f, ctx),
                    _ => panic!("if: expected bool"),
                }
            }
            BinOp(op, v1, v2) => {
                let v1 = self.eval(v1, ctx);
                let v2 = self.eval(v2, ctx);
                use bytecode::BinOp::*;
                match op {
                    Fby => unreachable!(""),
                    Add => typed_binop!(Int v1, Int v2, +, Int),
                    Sub => typed_binop!(Int v1, Int v2, -, Int),
                    Mul => typed_binop!(Int v1, Int v2, *, Int),
                    Div => typed_binop!(Int v1, Int v2, /, Int),
                    Mod => typed_binop!(Int v1, Int v2, %, Int),
                    And => typed_binop!(Bool v1, Bool v2, &&, Bool),
                    Or  => typed_binop!(Bool v1, Bool v2, ||, Bool),
                    // polymorphic compareason operators
                    Eq  => poly_binop!(v1, v2, ==),
                    Neq => poly_binop!(v1, v2, !=),
                    Le  => poly_binop!(v1, v2, <=),
                    Ge  => poly_binop!(v1, v2, >=),
                    Lt  => poly_binop!(v1, v2, <),
                    Gt  => poly_binop!(v1, v2, >),
                }
            }

            UnOp(op, v) => {
                let v = self.eval(v, ctx);
                match op {
                    bytecode::UnOp::Minus => typed_unop!(Int v, -, Int),
                    bytecode::UnOp::Not   => typed_unop!(Bool v, !, Bool),
                }
            }

            Call(op, ref args) => match self.eval(op, ctx) {
                Value::Closure(f) => {
                    let args = args.iter().map(|op| self.eval(*op, ctx)).collect::<Vec<_>>();
                    f.invoke(args)
                }
                _ => panic!("invoke: expected a function")
            },

            PrimCall(prim, ref args) => {
                let args = args.iter().map(|op| self.eval(*op, ctx)).collect::<Vec<_>>();
                unimplemented!("primitives");
            }

            Index(obj, index) => match self.eval(obj, ctx) {
                Value::Poly(notes) => match self.eval(index, ctx) {
                    Value::Int(i) => Value::Mono(notes[i as usize]),
                    _ => panic!("index: expected int index")
                }
                _ => panic!("index: expected Poly object")
            },

            Field(obj, ref field) => match self.eval(obj, ctx) {
                Value::Mono((pitch, gate, vel)) => match field.as_str() {
                    "pitch" => Value::Int(pitch as i64),
                    "gate" => Value::Gate(gate),
                    "vel" => Value::Int(vel as i64),
                    _ => panic!("field: invalid field {}", field)
                }
                _ => panic!("field: expected Mono object")
            },

            Poly(ref notes) => {
                let notes = notes.iter().map(|&(p, g, v)| {
                    let pitch = self.eval(p, ctx);
                    let gate = self.eval(g, ctx);
                    let vel = self.eval(v, ctx);
                    match (pitch, gate, vel) {
                        (Value::Int (p), Value::Gate(g), Value::Int(v)) =>
                            (p as u32, g, v as u32),
                        _ => panic!("poly: expected [int, gate, int]")
                    }
                }).collect::<Arc<[_]>>();
                Value::Poly(notes)
            }
             
            Mono(p, g, v) => {
                let pitch = self.eval(p, ctx);
                let gate = self.eval(g, ctx);
                let vel = self.eval(v, ctx);
                match (pitch, gate, vel) {
                    (Value::Int (p), Value::Gate(g), Value::Int(v)) =>
                        Value::Mono((p as u32, g, v as u32)),
                    _ => panic!("mono: expected [int, gate, int]")
                }
            }
        }
    }

    fn invoke(&self, args: Vec<Value>) -> Value {
        let ctx = Arc::new(EvalCtx {
            env: args, next: Some(self.env.clone())
        });

        self.eval(self.entry, &ctx)
    }
}
