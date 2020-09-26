use crate::ident::Ident;

#[derive(Debug)]
pub enum BinOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Neq, Lt, Gt, Le, Ge,
    And, Or
}

#[derive(Debug)]
pub enum UnOp {
    Minus, Plus
}

#[derive(Debug)]
pub enum Gate {
    On, Off, Tie
}

#[derive(Debug)]
pub enum Const {
    Num(i64),
    Bool(bool),
    Str(String),
    Gate(Gate)
}

#[derive(Debug)]
pub enum PropertyArg {
    Id(Ident),
    Cst(Const)
}

#[derive(Debug)]
pub struct Attribute {
    pub name: Ident,
    pub args: Vec<PropertyArg>
}

#[derive(Debug)]
pub struct Node<T> {
    pub node: Box<T>,
}

pub type Expr = Node<ExprNode>;
#[derive(Debug)]
pub enum ExprNode {
    BinOp(Expr, BinOp, Expr),
    UnOp(UnOp, Expr),
    If(Expr, Expr, Expr),
    Call(Expr, Vec<Expr>),
    Var(Ident),
    Cst(Const),
    Index(Expr, Expr),
    Field(Expr, Ident),
    Poly(Vec<(Expr, Expr, Expr)>),
    Mono(Expr, Expr, Expr),
    Lambda(Vec<(Ident, Type)>, Type, Expr),
    Let(Ident, Expr, Expr),
}

#[derive(Debug)]
pub enum Type {
    Int, Str,
    Poly, Mono,
    Gate,
}

#[derive(Debug)]
pub struct Def {
    pub name: Ident,
    pub value: Expr,
}

#[derive(Debug)]
pub enum Args {
    Named(Vec<(Ident, Type)>),
    Unnamed(Vec<Type>)
}

impl Args {
    pub fn len(&self) -> usize {
        match self { Args::Named(v) => v.len(),
                     Args::Unnamed(v) => v.len() }
    }
}

// XXX: should not necessary once we change IR
#[derive(Debug)]
pub struct FnDef {
    pub name: Ident,
    pub body: Expr,
    pub inputs: Args,
    pub outputs: Args,
    pub locals: Vec<(Ident, Type)>,
    pub attrs: Vec<Attribute>,
}

pub trait Visitor {
    fn visit_def(&mut self, def: &mut Def) {
        def.value.visit(self);
    }

    fn visit_binop(&mut self, lhs: &mut Expr, op: &mut BinOp, rhs: &mut Expr) {
        lhs.visit(self);
        rhs.visit(self);
    }

    fn visit_op(&mut self, op: &mut UnOp, expr: &mut Expr) {
        expr.visit(self);
    }

    fn visit_if(&mut self, cond: &mut Expr, btrue: &mut Expr, bfalse: &mut Expr) {
        cond.visit(self);
        btrue.visit(self);
        bfalse.visit(self);
    }

    fn visit_call(&mut self, func: &mut Expr, args: &mut Vec<Expr>) {
        func.visit(self);
        args.iter_mut().map(|e| e.visit(self));
    }

    fn visit_var(&mut self, var: &mut Ident) {}
    fn visit_const(&mut self, cst: &mut Const) {}

    fn visit_index(&mut self, expr: &mut Expr, index: &mut Expr) {
        expr.visit(self);
        index.visit(self);
    }

    fn visit_field(&mut self, expr: &mut Expr, field: &mut Ident) {
        expr.visit(self);
    }

    fn visit_poly(&mut self, notes: &mut Vec<(Expr, Expr, Expr)>) {}
    fn visit_mono(&mut self, pitch: &mut Expr, gate: &mut Expr, vel: &mut Expr) {}

    fn visit_lambda(&mut self, args: &mut Vec<(Ident, Type)>, ret_ty: &mut Type, expr: &mut Expr) {
        expr.visit(self);
    }

    fn visit_let(&mut self, var: &mut Ident, val: &mut Expr, expr: &mut Expr) {
        val.visit(self);
        expr.visit(self);
    }
}

impl Expr {
    pub fn visit<V: Visitor + ?Sized>(&mut self, v: &mut V) {
        use ExprNode::*;
        match &mut *self.node {
            BinOp(lhs, op, rhs) => v.visit_binop(lhs, op, rhs),
            UnOp(op, expr) => v.visit_op(op, expr),
            If(cond, bt, bf) => v.visit_if(cond, bt, bf),
            Call(f, args) => v.visit_call(f, args),
            Var(var) => v.visit_var(var),
            Cst(cst) => v.visit_const(cst),
            Index(expr, idx) => v.visit_index(expr, idx),
            Field(expr, fld) => v.visit_field(expr, fld),
            Poly(notes) => v.visit_poly(notes),
            Mono(pitch, gate, vel) => v.visit_mono(pitch, gate, vel),
            Lambda(args, ret_ty, expr) => v.visit_lambda(args, ret_ty, expr),
            Let(var, val, expr) => v.visit_let(var, val, expr),
        }
    }
}
