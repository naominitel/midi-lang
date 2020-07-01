use std::io::{self, Read};
use std::sync::Arc;
use log::debug;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use message;

type Var = u16;
type GlobRef = u32;
type ConstRef = u32;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Gate {
    On, Off, Tie
}

// For now values are simple and clonable
// (Strings are RC-ed)
// We might do something more intelligent later
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Value {
    Int(i64),
    Str(Arc<String>),
    Bool(bool),
    Gate(Gate),
    Error,
    // FIXME: ?
    Undef,
    Mono(MonoNote),
    Poly(Arc<[MonoNote]>),
    Primitive(usize),
}

pub type MonoNote = (u32, Gate, u32);

#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Fby,
    Add, Sub, Mul, Div, Mod,
    Eq, Neq, Lt, Gt, Le, Ge,
    And, Or
}

#[derive(Clone, Copy, Debug)]
pub enum UnOp {
    Minus, Not
}

#[derive(Clone)]
pub enum Op {
    Glob(Arc<String>),
    Const(Value),
    Pre(Var),

    Nop(Var),

    If(Var, Var, Var),

    BinOp(BinOp, Var, Var),
    UnOp(UnOp, Var),

    Call(Arc<String>, Vec<Var>),

    Index(Var, Var),
    Field(Var, Arc<String>),

    Poly(Vec<(Var, Var, Var)>),
    Mono(Var, Var, Var),
}

#[derive(Clone)]
pub struct NodeDef {
    pub name: Arc<String>,
    pub n_inputs: u8,
    pub outputs: Vec<Var>,
    pub locals: Vec<Var>,
    pub equs: Vec<Op>,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
#[derive(FromPrimitive)]
enum Opcode {
    Nop = 0x00,
    Cons = 0x01,
    Glob = 0x04,
    Pre = 0x08,
    Add = 0x10,
    Sub = 0x11,
    Mul = 0x12,
    Div = 0x13,
    Mod = 0x14,
    Min = 0x17,
    Eq = 0x18,
    Neq = 0x19,
    Ge = 0x1A,
    Le = 0x1B,
    Gt = 0x1C,
    Lt = 0x1D,
    And = 0x20,
    Or = 0x21,
    Not = 0x27,
    Fby = 0x28,
    If = 0x40,
    Call = 0x50,
    Indx = 0x54,
    Fld = 0x58,
    Poly = 0x60,
    Mono = 0x68
}

// constant value types
const T_INT:  u8 = 0x01;
const T_BOOL: u8 = 0x02;
const T_GATE: u8 = 0x08;
const T_STR:  u8 = 0x10;

struct NodeDefParser<'a> {
    equs: &'a [u8],
    cst: &'a [u8],
    cst_offset: usize
}

impl<'a> NodeDefParser<'a> {
    pub fn new(equs: &message::UpdateMessage) -> NodeDefParser {
        let locals =
            if equs.header.n_locals == 0 { 0 }
            else { (((((equs.header.n_locals - 1) << 1) >> 4) + 1) << 4) };
        let equ_size =
            if equs.header.equ_sec_size % 16 != 0 {
                equs.header.equ_sec_size + 8
            } else { equs.header.equ_sec_size };
        let cst_offset =
            ::std::mem::size_of::<message::UpdateHeader>()
            + equ_size as usize
            + locals as usize;
        NodeDefParser {
            equs: &equs.equ_section,
            cst: &equs.cst_section,
            cst_offset: cst_offset
        }
    }

    fn read_u8(&mut self) -> io::Result<u8> {
        let mut next = 0;
        self.equs.read_exact(::std::slice::from_mut(&mut next))?;
        Ok(next)
    }

    fn read_u16(&mut self) -> io::Result<u16> {
        let mut buf = [0 ; ::std::mem::size_of::<u16>()];
        self.equs.read_exact(&mut buf)?;
        Ok(u16::from_le_bytes(buf))
    }

    fn read_u32(&mut self) -> io::Result<u32> {
        let mut buf = [0 ; ::std::mem::size_of::<u32>()];
        self.equs.read_exact(&mut buf)?;
        Ok(u32::from_le_bytes(buf))
    }

    fn read_i64(&mut self) -> io::Result<i64> {
        let mut buf = [0 ; ::std::mem::size_of::<i64>()];
        self.equs.read_exact(&mut buf)?;
        Ok(i64::from_le_bytes(buf))
    }

    fn read_pad(&mut self, count: usize) -> io::Result<()> {
        for _ in 0 .. count {
            let _ = self.read_u8()?;
        }
        Ok(())
    }

    fn read_binop(&mut self, op: BinOp) -> io::Result<Op> {
        let _ = self.read_u8()?;
        let arg1 = self.read_u16()?;
        let arg2 = self.read_u16()?;
        self.read_pad(2)?;
        Ok(Op::BinOp(op, arg1, arg2))
    }

    fn read_unop(&mut self, op: UnOp) -> io::Result<Op> {
        let _ = self.read_u8()?;
        let arg = self.read_u16()?;
        self.read_pad(4)?;
        Ok(Op::UnOp(op, arg))
    }

    fn at_end(&mut self) -> bool {
        self.equs.len() == 0
    }

    fn get_const(&mut self, ptr: u32) -> io::Result<Arc<String>> {
        let seek = ptr as usize - self.cst_offset;
        let mut s = String::from("");
        for &ch in self.cst[seek ..].iter() {
            if ch == 0 {
                return Ok(Arc::new(s));
            }

            s.push(ch as char);
        }
        panic!("premature end of cst section")
    }

    fn read_equs(&mut self) -> io::Result<Vec<Op>> {
        let mut equs = Vec::with_capacity(self.equs.len() << 3);

        while !self.at_end() {
            use self::Opcode::*;
            let opc = self.read_u8()?;
            let op = match Opcode::from_u8(opc) {
                Some(Fby) => self.read_binop(BinOp::Fby)?,
                Some(Add) => self.read_binop(BinOp::Add)?,
                Some(Sub) => self.read_binop(BinOp::Sub)?,
                Some(Mul) => self.read_binop(BinOp::Mul)?,
                Some(Div) => self.read_binop(BinOp::Div)?,
                Some(Mod) => self.read_binop(BinOp::Mod)?,
                Some(Eq)  => self.read_binop(BinOp::Eq)?,
                Some(Neq) => self.read_binop(BinOp::Neq)?,
                Some(Ge)  => self.read_binop(BinOp::Ge)?,
                Some(Le)  => self.read_binop(BinOp::Le)?,
                Some(Gt)  => self.read_binop(BinOp::Gt)?,
                Some(Lt)  => self.read_binop(BinOp::Lt)?,
                Some(And) => self.read_binop(BinOp::And)?,
                Some(Or)  => self.read_binop(BinOp::Or)?,
                Some(Min) => self.read_unop(UnOp::Minus)?,
                Some(Not) => self.read_unop(UnOp::Not)?,
                Some(If)  => {
                    let _ = self.read_u8()?;
                    let cond = self.read_u16()?;
                    let vart = self.read_u16()?;
                    let varf = self.read_u16()?;
                    Op::If(cond, vart, varf)
                }
                Some(Pre) => {
                    let _ = self.read_u8()?;
                    let var = self.read_u16()?;
                    self.read_pad(4)?;
                    Op::Pre(var)
                }
                Some(Call) => {
                    let _ = self.read_u8()?;
                    let nargs = self.read_u16()?;
                    let f = self.read_u32()?;
                    let mut args = Vec::with_capacity(16);
                    let mut nbytes = 0;
                    for _ in 0 .. nargs {
                        let arg = self.read_u16()?;
                        nbytes += 2;
                        args.push(arg);
                    }
                    while nbytes % 8 != 0 {
                        let _ = self.read_u8()?;
                        nbytes += 1;
                    }
                    Op::Call(self.get_const(f)?, args)
                }
                Some(Nop) => {
                    let _ = self.read_u8()?;
                    let var = self.read_u16()?;
                    self.read_pad(4)?;
                    Op::Nop(var)
                }
                Some(Glob) => {
                    self.read_pad(3)?;
                    let ptr = self.read_u32()?;
                    Op::Glob(self.get_const(ptr)?)
                }
                Some(Cons) => {
                    match self.read_u8()? {
                        T_INT => {
                            self.read_pad(6)?;
                            Op::Const(Value::Int(self.read_i64()?))
                        }

                        T_BOOL => {
                            let b = match self.read_u8()? {
                                0x00 => false,
                                0x01 => true,
                                _ => panic!("invalid bool value")
                            };
                            self.read_pad(5)?;
                            Op::Const(Value::Bool(b))
                        }

                        T_STR => {
                            self.read_pad(2)?;
                            let ptr = self.read_u32()?;
                            Op::Const(Value::Str(self.get_const(ptr)?))
                        }

                        T_GATE => {
                            let gate = match self.read_u8()? {
                                0x00 => self::Gate::Off,
                                0x01 => self::Gate::On,
                                0xFF => self::Gate::Tie,
                                _ => panic!("invalid gate value")
                            };
                            self.read_pad(5)?;
                            Op::Const(Value::Gate(gate))
                        }

                        _ => panic!("unknown value type")
                    }
                }
                Some(Indx) => {
                    let _ = self.read_u8()?;
                    let e1 = self.read_u16()?;
                    let e2 = self.read_u16()?;
                    self.read_pad(2)?;
                    Op::Index(e1, e2)
                }
                Some(Fld) => {
                    let _ = self.read_u8()?;
                    let e = self.read_u16()?;
                    let f = self.read_u32()?;
                    Op::Field(e, self.get_const(f)?)
                }
                Some(Poly) => {
                    let size = self.read_u8()?;
                    let mut notes = Vec::with_capacity(size as usize);
                    self.read_pad(6)?;
                    for _ in 0 .. size {
                        let pitch = self.read_u16()?;
                        let gate = self.read_u16()?;
                        let velo = self.read_u16()?;
                        let _ = self.read_u16();
                        notes.push((pitch, gate, velo))
                    }
                    Op::Poly(notes)
                }
                Some(Mono) => {
                    let _ = self.read_u8()?;
                    let pitch = self.read_u16()?;
                    let gate = self.read_u16()?;
                    let velo = self.read_u16()?;
                    Op::Mono(pitch, gate, velo)
                }
                None => panic!("invalid opcode {:?}", opc)
            };

            equs.push(op);
        }


        Ok(equs)
    }

    pub fn parse(msg: message::UpdateMessage) -> io::Result<NodeDef> {
        let mut outputs = Vec::with_capacity(16);
        let mut locals = Vec::with_capacity(msg.header.n_locals as usize);
        let inputs = msg.header.n_inputs;

        for i in 0 .. msg.header.n_outputs {
            let base = (i as usize) << 1;
            let u0 = msg.header.output_vars[base] as u16;
            let u1 = msg.header.output_vars[base + 1] as u16;
            outputs.push((u1 << 8) | u0);
        }

        for i in 0 .. msg.header.n_locals {
            let base = (i as usize) << 1;
            let u0 = msg.locals[base] as u16;
            let u1 = msg.locals[base + 1] as u16;
            locals.push((u1 << 8) | u0);
        }

        let name_ptr = msg.header.name_ptr;
        let mut parser = NodeDefParser::new(&msg);
        let equs = parser.read_equs()?;
        let node_name = parser.get_const(name_ptr)?;

        Ok(NodeDef {
            name: node_name,
            n_inputs: inputs,
            outputs: outputs,
            equs: equs,
            locals: locals
        })
    }
}

impl NodeDef {
    pub fn default() -> NodeDef {
        NodeDef {
            name: Arc::new(String::from("__default__")),
            n_inputs: 0,
            outputs: vec![],
            equs: vec![],
            locals: vec![]
        }
    }

    pub fn parse(msg: message::UpdateMessage) -> io::Result<NodeDef> {
        NodeDefParser::parse(msg)
    }

    pub fn dump(&self) {
        debug!("=== node def {}, {} inputs ===", self.name, self.n_inputs);
        debug!("outputs: ");
        for i in self.outputs.iter() {
            debug!("{} ", i);
        }
        debug!("");
        debug!("locals: ");
        for i in self.locals.iter() {
            debug!("{} ", i);
        }
        debug!("");
        debug!("equs:");
        for (i, eq) in self.equs.iter().enumerate() {
            use self::Op::*;
            use self::Value::*;
            use self::Gate::*;
            use self::BinOp::*;
            use self::UnOp::*;
            debug!("  {} = ", i);
            match eq {
                Glob(glob) => debug!("global({})", glob),
                Const(Int(i)) => debug!("const(int({}))", i),
                Const(Str(s)) => debug!("const(str({}))", s),
                Const(Gate(On)) => debug!("const(ON)"),
                Const(Gate(Off)) => debug!("const(OFF)"),
                Const(Gate(Tie)) => debug!("const(TIE)"),
                Const(_) => debug!("??"),
                BinOp(Fby, v1, v2) => debug!("{} -> {}", v1, v2),
                BinOp(Add, v1, v2) => debug!("{} + {}", v1, v2),
                BinOp(Sub, v1, v2) => debug!("{} - {}", v1, v2),
                BinOp(Mul, v1, v2) => debug!("{} * {}", v1, v2),
                BinOp(Div, v1, v2) => debug!("{} / {}", v1, v2),
                BinOp(Mod, v1, v2) => debug!("{} % {}", v1, v2),
                BinOp(Eq, v1, v2) => debug!("{} == {}", v1, v2),
                BinOp(Neq, v1, v2) => debug!("{} != {}", v1, v2),
                BinOp(Ge, v1, v2) => debug!("{} >= {}", v1, v2),
                BinOp(Le, v1, v2) => debug!("{} <= {}", v1, v2),
                BinOp(Gt, v1, v2) => debug!("{} > {}", v1, v2),
                BinOp(Lt, v1, v2) => debug!("{} < {}", v1, v2),
                BinOp(And, v1, v2) => debug!("{} && {}", v1, v2),
                UnOp(Minus, v) => debug!("- {}", v),
                UnOp(Not, v) => debug!("! {}", v),
                Nop(v) => debug!("{}", v),
                If(c, t, f) => debug!("if {} then {} else {}", c, t, f),
                Pre(v) => debug!("pre {}", v),
                Call(f, args) => debug!("{}({:?})", f, args),
                Index(v1, v2) => debug!("{}[{}]", v1, v2),
                Field(v, field) => debug!("{}.{}", v, field),
                Op::Poly(_) => debug!("poly"),
                _ => debug!("mono"),
            }
        }
    }
}
