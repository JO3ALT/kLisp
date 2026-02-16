pub type Id = usize;

#[derive(Clone, Debug)]
pub enum Node {
    Nil,
    Bool(bool),
    Int(i64),
    Str(String),
    Sym(String),
    List(Vec<Id>),
    Func(FuncObj),
}

#[derive(Clone, Debug)]
pub struct FuncObj {
    pub params: Vec<String>,
    pub body: Vec<Id>,
    pub env: crate::lisp::env::EnvPtr,
}

#[derive(Default, Debug)]
pub struct Arena {
    nodes: Vec<Node>,
}

impl Arena {
    pub fn new() -> Self { Self { nodes: vec![] } }

    pub fn alloc(&mut self, n: Node) -> Id {
        let id = self.nodes.len();
        self.nodes.push(n);
        id
    }

    pub fn nil(&mut self) -> Id { self.alloc(Node::Nil) }
    pub fn t(&mut self) -> Id { self.alloc(Node::Bool(true)) }
    pub fn int(&mut self, n: i64) -> Id { self.alloc(Node::Int(n)) }
    pub fn sym(&mut self, s: &str) -> Id { self.alloc(Node::Sym(s.to_string())) }
    pub fn list(&mut self, xs: Vec<Id>) -> Id { self.alloc(Node::List(xs)) }

    pub fn get(&self, id: Id) -> &Node { &self.nodes[id] }
    pub fn node_count(&self) -> usize { self.nodes.len() }

    pub fn is_nil(&self, id: Id) -> bool { matches!(self.get(id), Node::Nil) }
    pub fn is_truthy(&self, id: Id) -> bool { !self.is_nil(id) }

    pub fn sym_name(&self, id: Id) -> Option<&str> {
        match self.get(id) {
            Node::Sym(s) => Some(s.as_str()),
            _ => None,
        }
    }

    pub fn fmt(&self, id: Id) -> String {
        #[derive(Clone, Copy)]
        enum Action {
            Emit(Id),
            OpenList,
            CloseList,
            Space,
        }
        let mut out = String::new();
        let mut stack = vec![Action::Emit(id)];
        while let Some(action) = stack.pop() {
            match action {
                Action::Emit(cur) => match self.get(cur) {
                    Node::Nil => out.push_str("nil"),
                    Node::Bool(true) => out.push_str("t"),
                    Node::Bool(false) => out.push_str("nil"),
                    Node::Int(n) => out.push_str(&n.to_string()),
                    Node::Str(s) => {
                        out.push('"');
                        out.push_str(&s.replace('"', "\\\""));
                        out.push('"');
                    }
                    Node::Sym(s) => out.push_str(s),
                    Node::List(xs) => {
                        stack.push(Action::CloseList);
                        for i in (0..xs.len()).rev() {
                            stack.push(Action::Emit(xs[i]));
                            if i > 0 {
                                stack.push(Action::Space);
                            }
                        }
                        stack.push(Action::OpenList);
                    }
                    Node::Func(_) => out.push_str("#<function>"),
                },
                Action::OpenList => out.push('('),
                Action::CloseList => out.push(')'),
                Action::Space => out.push(' '),
            }
        }
        out
    }
}
