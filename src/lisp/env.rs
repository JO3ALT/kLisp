use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::lisp::arena::Id;

pub type EnvPtr = Rc<RefCell<EnvFrame>>;

#[derive(Debug, Default)]
pub struct EnvFrame {
    pub map: HashMap<String, Id>,
    pub parent: Option<EnvPtr>,
}

pub fn new_env(parent: Option<EnvPtr>) -> EnvPtr {
    Rc::new(RefCell::new(EnvFrame { map: HashMap::new(), parent }))
}

pub fn env_get(env: &EnvPtr, k: &str) -> Option<Id> {
    let mut cur = Some(env.clone());
    while let Some(e) = cur {
        let frame = e.borrow();
        if let Some(v) = frame.map.get(k).copied() {
            return Some(v);
        }
        cur = frame.parent.clone();
    }
    None
}

pub fn env_define(env: &EnvPtr, k: String, v: Id) {
    env.borrow_mut().map.insert(k, v);
}

pub fn env_set_existing(env: &EnvPtr, k: &str, v: Id) -> Option<()> {
    let mut cur = Some(env.clone());
    while let Some(e) = cur {
        {
            let mut frame = e.borrow_mut();
            if frame.map.contains_key(k) {
                frame.map.insert(k.to_string(), v);
                return Some(());
            }
            cur = frame.parent.clone();
        }
    }
    None
}
