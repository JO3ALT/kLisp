pub mod arena;
pub mod env;
pub mod eval;

use self::arena::Arena;

pub struct Ctx {
    pub arena: Arena,
    pub env: env::EnvPtr,
}

impl Ctx {
    pub fn new() -> Self {
        let mut ctx = Self {
            arena: Arena::new(),
            env: env::new_env(None),
        };
        // Bind builtin names to self-evaluating symbols so evaluator can dispatch them.
        for name in [
            "cons", "car", "cdr", "list", "null?", "atom?", "pair?", "eq?", "assoc", "append",
            "map", "mapcar", "+", "-", "print", "funcall", "null", "atom", "consp", "not",
            "equal", "eq", "reverse", "nreverse", "format", "cadr", "caddr", "cddr", "cdddr",
        ] {
            let sym = ctx.arena.sym(name);
            env::env_define(&ctx.env, name.to_string(), sym);
        }
        ctx
    }
}
