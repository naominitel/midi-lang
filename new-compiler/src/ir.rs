use crate::ast;
use std::collections::HashMap;
use crate::util::BinWrite;

const UPDATE: u32 = 0x10;

#[repr(u8)]
enum OpCode {
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

#[repr(u8)]
enum TypeCode {
    Int = 0x01,
    Bool = 0x02,
    Gate = 0x08,
    Str = 0x10,
}

struct TransCtx<'a> {
    buf: Vec<u8>,
    relocs: Vec<(usize, &'a str)>,
    next_node: u16,
    env: HashMap<String, u16>
}

use std::io::Write;

impl<'a> TransCtx<'a> {

    fn reloc(&mut self, data: &'a str) {
        let seek = self.buf.len();
        self.buf.write_le::<u32>(0);
        self.relocs.push((seek, data))
    }

    fn write_cst(&mut self, cst: &'a ast::Const) {
        self.buf.write_le::<u8>(OpCode::Cons as u8);
        match cst {
            ast::Const::Num(i) => {
                self.buf.write_le::<u8>(TypeCode::Int as u8);
                self.buf.pad(6);
                self.buf.write_le::<i64>(*i);
            }
            ast::Const::Bool(b) => {
                self.buf.write_le::<u8>(TypeCode::Bool as u8);
                self.buf.write_le::<u8>(if *b { 0x01 } else { 0x00 });
                // FIXME: could be pad 5?
                self.buf.align(8);
            }
            ast::Const::Str(ref s) => {
                self.buf.write_le::<u8>(TypeCode::Str as u8);
                self.buf.pad(2);
                self.reloc(&s);
            }
            ast::Const::Gate(g) => {
                self.buf.write_le::<u8>(TypeCode::Gate as u8);
                self.buf.write_le::<u8>(match g {
                    Off => 0x00,
                    On  => 0x01,
                    Tie => 0x02,
                });
                // FIXME: could be pad 5?
                self.buf.align(8);
            }
        }
    }

    fn next(&mut self) -> u16 {
        let next = self.next_node;
        self.next_node += 1;
        next
    }

    fn simple_op(&mut self, op: OpCode, args: &[&'a ast::Expr]) -> u16 {
        let args = args.iter().map(|e| self.expr(e)).collect::<Vec<_>>();
        let this = self.next();
        self.buf.write_le::<u8>(op as u8);
        self.buf.write_le::<u8>(0);
        for a in args {
            self.buf.write_le::<u16>(a);
        }
        self.buf.align(8);
        this
    }

    fn expr(&mut self, e: &'a ast::Expr) -> u16 {
        use ast::ExprNode::*;
        use ast::BinOp::*;
        match &*e.node {
            // simple operations (AST basic nodes)
            // binops
            BinOp(x, Add, y) => self.simple_op(OpCode::Add, &[x, y]),
            BinOp(x, Sub, y) => self.simple_op(OpCode::Sub, &[x, y]),
            BinOp(x, Mul, y) => self.simple_op(OpCode::Mul, &[x, y]),
            BinOp(x, Div, y) => self.simple_op(OpCode::Div, &[x, y]),
            BinOp(x, Mod, y) => self.simple_op(OpCode::Mod, &[x, y]),
            BinOp(x, Eq,  y) => self.simple_op(OpCode::Eq,  &[x, y]),
            BinOp(x, Neq, y) => self.simple_op(OpCode::Neq, &[x, y]),
            BinOp(x, Ge,  y) => self.simple_op(OpCode::Ge,  &[x, y]),
            BinOp(x, Le,  y) => self.simple_op(OpCode::Le,  &[x, y]),
            BinOp(x, Gt,  y) => self.simple_op(OpCode::Gt,  &[x, y]),
            BinOp(x, Lt,  y) => self.simple_op(OpCode::Lt,  &[x, y]),
            BinOp(x, And, y) => self.simple_op(OpCode::And, &[x, y]),
            BinOp(x, Or,  y) => self.simple_op(OpCode::Or,  &[x, y]),

            // unary ops
            UnOp(Plus, x)    => self.simple_op(OpCode::Nop, &[x]),
            UnOp(Minus, x)   => self.simple_op(OpCode::Min, &[x]),
            UnOp(Not, x)     => self.simple_op(OpCode::Not, &[x]),

            // basic constructs
            If(c, t, f)      => self.simple_op(OpCode::If,  &[c, t, f]),
            Index(e, i)      => self.simple_op(OpCode::Indx, &[e, i]),

            // special constructs (field access, poly and mono literals)

            Field(e, f) => {
                let e = self.expr(e);
                let this = self.next();
                self.buf.write_le::<u8>(OpCode::Fld as u8);
                self.buf.write_le::<u8>(0);
                self.buf.write_le::<u16>(e);
                // TODO: fields should be
                // compiled to int indices
                self.reloc(&f);
                this
            }

            Poly(s) => {
                let vars = s.iter().map(|(e1, e2, e3)| {
                    let e1 = self.expr(e1);
                    let e2 = self.expr(e2);
                    let e3 = self.expr(e3);
                    (e1, e2, e3)
                }).collect::<Vec<_>>();
                let this = self.next();
                self.buf.write_le::<u8>(OpCode::Poly as u8);
                self.buf.write_le::<u8>(vars.len() as u8);
                self.buf.pad(6);
                for (e1, e2, e3) in vars {
                    self.buf.write_le::<u16>(e1);
                    self.buf.write_le::<u16>(e2);
                    self.buf.write_le::<u16>(e3);
                    self.buf.write_le::<u16>(0);
                }
                this
            }

            Mono(e1, e2, e3) => {
                let e1 = self.expr(e1);
                let e2 = self.expr(e2);
                let e3 = self.expr(e3);
                let this = self.next();
                self.buf.write_le::<u8>(OpCode::Mono as u8);
                self.buf.write_le::<u8>(0);
                self.buf.write_le::<u16>(e1);
                self.buf.write_le::<u16>(e2);
                self.buf.write_le::<u16>(e3);
                this
            }
            // vars, consts (AST leafs)
            Var(v) => {
                let this = self.next();
                let v = *self.env.get(&*v).unwrap();
                self.buf.write_le(OpCode::Nop);
                self.buf.write_le::<u8>(0);
                self.buf.write_le::<u16>(v);
                self.buf.align(8);
                this
            }
            Cst(cst) => {
                let this = self.next();
                self.write_cst(cst);
                this
            }
            // AST nodes affetcting env: let
            // closures unsupported for now bc of the runtime
            Let(v, e1, e2) => {
                let e1 = self.expr(e1);
                let old = self.env.insert(v.clone(), e1);
                // TODO: ok for now because expressions don't have
                // side-effects but should force evaluation of e1 first
                let this = self.next();
                let e2 = self.expr(e2);
                match old {
                    Some(e) => self.env.insert(v.clone(), e),
                    None => self.env.remove(&*v),
                };
                e2
            }
            _ => unimplemented!()
        }
    }

    fn new(buf: Vec<u8>) -> TransCtx<'a> {
        TransCtx { buf: buf, relocs: Vec::new(),
                   env: HashMap::new(), next_node: 0 }
    }
}

fn update(def: ast::FnDef) -> Vec<u8> {
    let buf = Vec::with_capacity(1024);
    let mut ctx = TransCtx::new(buf);

    // reserve space for msg size
    ctx.buf.write_le::<u32>(0);
    ctx.buf.write_le::<u32>(UPDATE);
    ctx.reloc(&def.name);

    ctx.buf.write_le::<u8>(def.inputs.len() as u8);
    ctx.buf.write_le::<u8>(def.outputs.len() as u8);
    ctx.buf.write_le::<u16>(def.locals.len() as u16);

    // var number of outputs
    ctx.buf.pad(0x30);

    // attributes
    for ast::Attribute { name, args } in def.attrs.iter() {
        ctx.buf.write(name.as_bytes());
        ctx.buf.write_le(0u8);
        ctx.buf.write_le(args.len() as u8);
        ctx.buf.align(8);
        for arg in args {
            match arg {
                // FIXME
                // ast::PropertyArg::Id(id) => msg.write_cst(&ast::Const::Str(id)),
                ast::PropertyArg::Cst(cst) => {
                    ctx.write_cst(&cst);
                    // XXX: why align here and not above?
                    ctx.buf.align(8);
                }
                _ => ()
            }
        }
        ctx.buf.align(16);
    }

    let instr_begin = ctx.buf.len() as u32;
    let e = ctx.expr(&def.body);
    let instr_size = ctx.buf.len() as u32 - instr_begin;
    ctx.buf.align(16);

    // locals var (none for now)
    for _ in def.locals.iter() {
        buf.write_le::<u16>(0);
    }

    ctx.buf.align(16);
    let cst_begin = ctx.buf.len() as u32;

    let TransCtx { buf, relocs, .. } = ctx;

    // constant data
    let relocs = relocs.into_iter().map(|(seek, data)| {
        let new_seek = buf.len();
        buf.write(data.as_bytes());
        buf.write_le(0u8);
        buf.align(16);
        (seek, new_seek)
    }).collect::<Vec<_>>();

    let cst_size = buf.len() as u32 - cst_begin;
    let size = buf.len() as u32;

    // relocs
    // message size
    unsafe {
        use std::mem::transmute;
        *(transmute::<_, *mut u32>(buf.as_ptr())) = size;
        // outputs: we only have one and it is the function expr
        *(transmute::<_, *mut u16>(buf[0x10..].as_ptr())) = e;
        *(transmute::<_, *mut u32>(buf[0x20..].as_ptr())) = instr_size;
        *(transmute::<_, *mut u32>(buf[0x40..].as_ptr())) = cst_size;

        for (seek, new_seek) in relocs {
            *(transmute::<_, *mut u32>(buf[seek as usize..].as_ptr())) =
                new_seek as u32;
        }
    }

    buf
}
