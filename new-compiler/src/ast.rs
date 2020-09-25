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
