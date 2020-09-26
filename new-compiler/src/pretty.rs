

struct Pretty<'a> {
    formatter: &'a mut fmt::Formatter,
    ident_level: usize
}

impl<'a> Pretty<'a> {
    fn expr(&mut self, expr: &Expr) {
        match expr.node {
            BinOp
        }
    }
}
