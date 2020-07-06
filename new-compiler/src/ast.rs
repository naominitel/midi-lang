pub type Ident = String;

pub enum BinOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Neq, Lt, Gt, Le, Ge,
    And, Or
}

pub enum UnOp {
    Minus, Plus
}

pub enum Gate {
    On, Off, Tie
}

pub enum Const {
    Num(i64),
    Bool(bool),
    Str(String),
    Gate(Gate)
}

pub enum PropertyArg {
    Id(Ident),
    Cst(Const)
}

pub struct Attribute {
    pub name: Ident,
    pub args: Vec<PropertyArg>
}

pub struct Node<T> {
    pub node: Box<T>,
}

pub type Expr = Node<ExprNode>;
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
    Lambda(Vec<(Ident, Type)>, Expr),
    Let(Ident, Expr, Expr),
}

pub enum Type {
    Int, Str,
    Poly, Mono,
    Gate,
}

pub struct Def {
    pub name: Ident,
    pub value: Expr,
}

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

pub struct FnDef {
    pub name: Ident,
    pub body: Expr,
    pub inputs: Args,
    pub outputs: Args,
    pub locals: Vec<(Ident, Type)>,
    pub attrs: Vec<Attribute>,
}
