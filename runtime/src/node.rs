use std::sync::Arc;
use std::borrow::Borrow;
use log::debug;

use bytecode::{self, NodeDef, Value, Gate};
use engine;
use primitives;

// Just to avoid casts everywhere
// will always be an u16 in practice
type Var = usize;

#[derive(Clone)]
pub struct NodeInstance {
    equ: Vec<Op>,
    pub locals: Vec<Var>,
    outputs: Vec<Var>,
    // the current memory state, the
    // one which is being computed
    // None means a value is not yet
    // computed
    cur: Vec<Option<Value>>,
    // memory, the last computed value
    // of each variable
    mem: Vec<Value>,
    name: String,

    sub_nodes: Vec<NodeInstance>,
    prim_inst: Vec<primitives::Instance>
}

// Basically bytecode::Op but whre node calls
// are instanciated
// TODO: while we are here we could make some
// other optims here like inline globs and use
// a custom value type different from the
// bytecode repr.
// This type should be cheaply clone-able,
// hence the Arcs (Instead of RCs becaure the
// nodes can be concurrently accessed by the
// main thread)
#[derive(Clone)] // TODO: remove Strings
enum Op {
    Nop(Var),
    Glob(Arc<String>),
    Const(Value),
    Pre(Var),

    If(Var, Var, Var),
    BinOp(bytecode::BinOp, Var, Var),
    UnOp(bytecode::UnOp, Var),

    Call(usize, Arc<[Var]>),
    PrimCall(primitives::Primitive, usize, Arc<[Var]>),

    Index(Var, Var),
    Field(Var, Arc<String>),

    Poly(Arc<[(Var, Var, Var)]>),
    Mono(Var, Var, Var),
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

impl NodeInstance {
    fn var(&mut self, var: Var) -> Value {
        match self.cur[var].clone() {
            Some(value) => value,
            None => {
                let val = self.eval(var);
                debug!("{}: eval var {} -> {:?}", self.name, var, val);
                self.cur[var] = Some(val.clone());
                val
            }
        }
    }

    fn eval(&mut self, var: Var) -> Value {
        use self::Op::*;
        match self.equ[var].clone() {
            Glob(glob) => {
                if *glob == "undef" { Value::Undef }
                else if *glob == "MONO_OFF" { Value::Mono((0, Gate::Off, 0)) }
                else { panic!("unknown const") }
            }
            Const(val) => val.clone(),
            Nop(var) => self.var(var),
            Pre(vvar) => self.mem[vvar].clone(),

            If(vcond, vt, vf) => {
                match self.var(vcond) {
                    Value::Bool(b) => {
                        if b { self.var(vt) }
                        else { self.var(vf) }
                    }
                    Value::Error => Value::Error,
                    _ => {
                        eprintln!("warning: invalid type for if");
                        bytecode::Value::Error
                    }
                }
            }

            BinOp(bytecode::BinOp::Fby, v0, vn) => {
                // FIXME: not necessarily the best
                // way to check for initialization
                match self.mem[var] {
                    Value::Error => self.var(v0),
                    _ => self.var(vn)
                }
            }
            BinOp(op, v1, v2) => {
                let v1 = self.var(v1);
                let v2 = self.var(v2);
                use bytecode::BinOp::*;
                match op {
                    Fby => unreachable!(),
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
                let v = self.var(v);
                match op {
                    bytecode::UnOp::Minus => typed_unop!(Int v, -, Int),
                    bytecode::UnOp::Not   => typed_unop!(Bool v, !, Bool),
                }
            }

            Index(var, index) => {
                match (self.var(var), self.var(index)) {
                    (Value::Poly(notes), Value::Int(i)) =>
                        Value::Mono(notes[i as usize]),
                    (_, _) => {
                        eprintln!("warning: invalid type for index");
                        Value::Error
                    }
                }
            },

            Field(var, index) => {
                match self.var(var) {
                    Value::Mono((pitch, gate, vel)) => {
                        let idx: &String = index.borrow();
                        if idx == "pitch" {
                            Value::Int(pitch as i64)
                        } else if idx == "gate" {
                            Value::Gate(gate)
                        } else if idx == "vel" {
                            Value::Int(vel as i64)
                        } else {
                            eprintln!("warning: invalid field {}", index);
                            Value::Error
                        }
                    }

                    _ => {
                        eprintln!("warning: invalid type for field access");
                        Value::Error
                    }
                }
            },

            Poly(notes) => {
                let mut n = Vec::with_capacity(notes.len());
                for &(v1, v2, v3) in notes.iter() {
                    match (self.var(v1), self.var(v2), self.var(v3)) {
                        (Value::Int(i1),
                         Value::Gate(i2),
                         Value::Int(i3)) => {
                            n.push((i1 as u32, i2, i3 as u32));
                        }
                        (v1, v2, v3) => {
                            eprintln!("poly types: {:?} {:?} {:?}", v1, v2, v3);
                            eprintln!("warning: invalid type for poly");
                            return Value::Error;
                        }
                    }
                }
                Value::Poly(n.into())
            },

            Mono(v1, v2, v3) => {
                match (self.var(v1), self.var(v2), self.var(v3)) {
                    (Value::Int(i1),
                     Value::Gate(i2),
                     Value::Int(i3)) => Value::Mono((i1 as u32, i2, i3 as u32)),
                    _ => {
                        eprintln!("warning: invalid type for mono");
                        Value::Error
                    }
                }
            }

            Call(inst, args) => {
                let mut arg_values = Vec::with_capacity(args.len());
                for i in 0 .. args.len() {
                    arg_values.push(self.var(args[i]));
                }
                let ref mut inst = self.sub_nodes[inst];
                debug!("call: {:?}({:?})", self.name, arg_values);
                debug!("call: pre mem: {:?}", self.mem);
                inst.update(arg_values);
                let output = inst.get_outputs();
                // FIXME: multi output
                output[0].clone()
            }

            PrimCall(prim, inst, ref mut args) => {
                let mut arg_values = Vec::with_capacity(args.len());
                for i in 0 .. args.len() {
                    arg_values.push(self.var(args[i]));
                }
                primitives::update(prim, &mut self.prim_inst[inst], arg_values)
            }
        }
    }

    pub fn update(&mut self, inputs: Vec<Value>) {
        for i in 0 .. self.equ.len() {
            self.cur[i] = None;
        }
        for (i, var) in inputs.into_iter().enumerate() {
            self.cur[i + self.equ.len()] = Some(var);
        }
        for i in 0 .. self.locals.len() {
            let _ = self.var(self.locals[i]);
        }
        // copy new state to memory
        // (only updated variables)
        for (i, value) in self.cur.iter().enumerate() {
            if let Some(val) = value {
                if *val != Value::Undef {
                    self.mem[i] = val.clone();
                }
            }
        }
        debug!("{}: finished, mem state: {:?}", self.name, self.mem)
    }

    pub fn get_outputs(&self) -> Vec<Value> {
        let mut ret = Vec::with_capacity(self.outputs.len());
        for &var in self.outputs.iter() {
            ret.push(self.mem[var].clone());
        }
        ret
    }

    fn instanciate(def: &NodeDef, engine: &engine::Engine) -> NodeInstance {
        use self::Op::*;
        let mut equ = Vec::with_capacity(def.equs.len());
        let mut sub_nodes = Vec::new();
        let mut prim_inst = Vec::new();
        for eq in def.equs.iter() {
            // bytecode::Op to -> self::Op
            equ.push(match eq {
                bytecode::Op::Nop(var) => {
                    assert!(var.len() == 1);
                    Nop(var[0] as usize)
                }
                bytecode::Op::Glob(glob) => Glob(glob.clone()),
                bytecode::Op::Const(val) => Const(val.clone()),
                bytecode::Op::Get(..) => panic!("get: not supported in synchrone node"),
                bytecode::Op::Set(..) => panic!("set: not supported in synchrone node"),
                bytecode::Op::Pre(var) => Pre(*var as usize),
                bytecode::Op::If(c, t, f) =>
                    If(*c as usize, *t as usize, *f as usize),
                bytecode::Op::BinOp(op, v1, v2) =>
                    BinOp(*op, *v1 as usize, *v2 as usize),
                bytecode::Op::UnOp(op, v) => UnOp(*op, *v as usize),
                bytecode::Op::Lambda(..) => panic!("lambda: not supported in synchrone node"),
                bytecode::Op::FunCall(..) => panic!("funcall: not supported in synchrone node"),
                bytecode::Op::Call(fun, args) => {
                    let k: &String = &fun.borrow();
                    if let Some(def) = engine.node_defs.get(k) {
                        // recursive instanciation
                        let node = NodeInstance::instanciate(def, engine);
                        sub_nodes.push(node);
                        Call(sub_nodes.len() - 1,
                             args.iter().map(|v| *v as usize).collect::<Vec<_>>().into())
                    } else if let Some(&prim) = engine.primitives.get(k.as_str()) {
                        let inst = primitives::instance(prim);
                        prim_inst.push(inst);
                    PrimCall(prim, prim_inst.len() - 1,
                             args.iter().map(|v| *v as usize).collect::<Vec<_>>().into())
                    } else {
                        panic!("cannot instanciate call to \
                                undefined node {}", fun);
                    }
                }
                bytecode::Op::Index(v1, v2) =>
                    Index(*v1 as usize, *v2 as usize),
                bytecode::Op::Field(v, label) =>
                    Field(*v as usize, label.clone()),
                bytecode::Op::Poly(notes) =>
                    Poly(notes.iter().map(
                        |(pitch, gate, vel)|
                        (*pitch as usize, *gate as usize, *vel as usize)
                    ).collect::<Vec<_>>().into()),
                bytecode::Op::Mono(pitch, gate, vel) =>
                    Mono(*pitch as usize, *gate as usize, *vel as usize)
            });
        }
        let nvars = def.equs.len() + def.n_inputs as usize;
        let mut mem = Vec::with_capacity(nvars);
        for _ in 0 .. nvars {
            mem.push(Value::Error)
        }
        let mut cur = Vec::with_capacity(nvars);
        for _ in 0 .. nvars {
            cur.push(None);
        }
        let outputs = def.outputs.iter().map(|v| *v as usize).collect();
        let locals = def.locals.iter().map(|v| *v as usize).collect();
        let name = def.name.to_string();
        NodeInstance { mem, equ, cur, outputs, locals,
                       name, sub_nodes, prim_inst }
    }

    // TODO: arg will probably need to be changed to String
    // FIXME: what's the use of this function??
    pub fn new(def: &NodeDef, engine: &engine::Engine) -> NodeInstance {
        NodeInstance::instanciate(def, engine)
    }
}
