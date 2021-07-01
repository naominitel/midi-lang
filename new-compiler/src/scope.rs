// scope analysis
// replace source identifiers by unique ones which refer to lexical scopes

use crate::ast::{Datum, Visitor};
use crate::ident::{Ident, Interner};

pub struct Scope<'a> {
    interner: &'a mut Interner,
}

// an introduced variable
// contains all the info to call outro to restore things when leaving scope
type Handle<'b> = (&'b mut Ident, Ident, Option<Ident>);

impl<'a> Scope<'a> {
    pub fn new(interner: &mut Interner) -> Scope {
        Scope { interner }
    }

    // introduces a new variable (when encountering a lexical binding)
    // will update data in original identifier `var` to point to a new unique
    // var for this scope. each subsequent use of the original `var` in subtree
    // will be replaced by a use of the new unique var
    fn intro_var<'b>(&mut self, var: &'b mut Ident) -> Handle<'b> {
        let this = self.interner.create(var.name());
        let prev = var.data().scope.replace(Some(this.clone()));
        (var, this, prev)
    }

    // outro a variable (when leaving lexical scope of its definition)
    // pointer in original identifier is restored so that it is unbound again
    // (or bound back to a higher definition)
    // variable at bind-site is replaced by the new identifier
    fn outro_var<'b>(&mut self, (var, this, prev): Handle<'b>) {
        var.data().scope.set(prev);
        *var = this;
    }
}

impl<'a> Visitor for Scope<'a> {
    // updates a use site of a variable
    // if pointer inside original id points to an id, it means this var is bound
    // in this scope, we replace this use site with the new pointed id
    // otherwise it means the variable is unbound here
    fn visit_var(&mut self, var: &mut Ident) {
        let id = {
            let this = var.data().clone();
            let scope = this.scope.take();

            let id = match scope {
                Some(ref id) => id.clone(),
                None => panic!("undefined variable: {}", var)
            };

            this.scope.replace(scope);
            id
        };

        *var = id;
    }

    fn visit_cons(&mut self, car: &mut Datum, cdr: &mut Datum) {
        match car.node {
            Node::Symbol(s) if s == "lambda" => {
                let iter = cdr.iter();
                let args = match iter.next() {
                    Some(args) => args,
                    None => panic!("ill-formed lambda")
                }
            }
        }
    }

    fn visit_lambda(&mut self, args: &mut Vec<(Ident, Type)>,
                    _: &mut Type, expr: &mut Expr) {
        let vars = args.iter_mut().map(|(arg, _)| {
            self.intro_var(arg)
        }).collect::<Vec<_>>();

        expr.visit(self);

        for var in vars.into_iter() {
            self.outro_var(var);
        }
    }

    fn visit_let(&mut self, var: &mut Ident, val: &mut Expr, expr: &mut Expr) {
        val.visit(self);
        let var = self.intro_var(var);
        expr.visit(self);
        self.outro_var(var);
    }
}
