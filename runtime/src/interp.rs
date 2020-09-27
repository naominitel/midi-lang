use std::sync::{Arc, Mutex};
use bytecode::{self, Value};
use primitives;

#[derive(Clone)]
pub struct Function {
    code: Vec<bytecode::Op>,
    entry: u16,
    nargs: u8,
    nlocals: u16,
    env: EvalCtx,
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

#[derive(Debug)]
struct CtxFrame {
    env: Vec<Value>,
}

#[derive(Clone, Debug)]
pub struct EvalCtx {
    ctx: Vec<Arc<Mutex<CtxFrame>>>,
}

impl EvalCtx {
    pub fn new() -> EvalCtx {
        EvalCtx { ctx: vec![Arc::new(Mutex::new(CtxFrame { env: vec![] }))] }
    }

    fn get(&self, var: usize, depth: u16) -> Value {
        self.ctx[depth as usize].lock().unwrap().env[var].clone()
    }

    fn set(&mut self, var: usize, depth: u16, val: Value) {
        self.ctx[depth as usize].lock().unwrap().env[var] = val;
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
    fn eval(&self, op: u16, ctx: &mut EvalCtx) -> Value {
        use bytecode::Op::*;
        match self.code[op as usize] {
            Nop(ref ops) => {
                let mut ret = Value::Undef;
                for &op in ops {
                    ret = self.eval(op, ctx);
                }
                ret
            }
            Glob(ref glob) => panic!("unimplemented: globals"),
            Const(ref v) => v.clone(),
            Get(v, c) => ctx.get(v as usize, c),
            Set(v, c, e) => {
                let val = self.eval(e, ctx);
                ctx.set(v as usize, c, val);
                Value::Undef
            }
            Lambda(nargs, nlocals, op) => {
                Value::Closure(Function {
                    code: self.code.clone(),
                    entry: op,
                    nargs: nargs,
                    nlocals: nlocals,
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

            Pre(..) => panic!("pre in functional mode"),

            Call(..) => panic!("unimplemented: global function"),

            FunCall(op, ref args) => match self.eval(op, ctx) {
                Value::Closure(f) => {
                    let args = args.iter().map(|op| self.eval(*op, ctx)).collect::<Vec<_>>();
                    f.invoke(args)
                }
                _ => panic!("invoke: expected a function")
            },

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

    pub fn invoke(&self, args: Vec<Value>) -> Value {
        if self.nargs as usize != args.len() {
            panic!("bad arity: expected {} but found {}", self.nargs, args.len());
        }
        let mut env = args;
        for i in 0 .. self.nlocals {
            env.push(Value::Undef);
        }
        let mut ctx = self.env.clone();
        ctx.ctx.push(Arc::new(Mutex::new(CtxFrame { env })));

        self.eval(self.entry, &mut ctx)
    }

    pub fn new(toplevel_env: EvalCtx, def: bytecode::NodeDef)
           -> Function {
        if def.outputs.len() != 1 {
            panic!("function nodes should have exactly 1 output")
        }
        Function {
            env: toplevel_env, nargs: def.n_inputs, nlocals: def.locals.len() as u16,
            code: def.equs, entry: def.outputs[0]
        }
    }
}
