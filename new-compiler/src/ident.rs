use std::cell::Cell;
use std::collections::HashMap;
use std::fmt;
use std::hash;
use std::rc::Rc;

#[derive(Clone)]
pub struct Ident {
    name: Rc<str>,
    id: usize,

    data: Rc<IdentData>
}

// data for analysis passes
// TODO: make generic somehow
// (some kinda compile-time KV store with traits)
pub struct IdentData {
    pub scope: Cell<Option<Ident>>,
}

impl PartialEq for Ident {
    fn eq(&self, rhs: &Ident) -> bool {
        self.id == rhs.id
    }
}

impl Eq for Ident {}

impl hash::Hash for Ident {
    fn hash<H: hash::Hasher>(&self, h: &mut H) {
        self.id.hash(h)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_str(&self.name)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_fmt(format_args!("{}__{}", self.name, self.id))
    }
}

impl Ident {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn data(&self) -> &IdentData {
        &self.data
    }
}

pub struct Interner {
    table: HashMap<Rc<str>, Ident>,
    next_id: usize
}

impl Interner {
    pub fn new() -> Interner {
        Interner {
            table: HashMap::new(),
            next_id: 0
        } 
    }

    pub fn get(&mut self, id: &str) -> Ident {
        match self.table.get(id) {
            Some(u) => u.clone(),
            None => {
                let u = self.create(id);
                self.table.insert(u.name.clone(), u.clone());
                u
            }
        }
    }

    pub fn create(&mut self, name: &str) -> Ident {
        let u = Ident {
            name: name.to_string().into(),
            id: self.next_id,

            data: Rc::new(IdentData{
                scope: Cell::new(None)
            })
        };
        self.next_id += 1; u
    }
}
