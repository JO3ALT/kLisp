use crate::lisp::arena::{Arena, FuncObj, Id, Node};
use crate::lisp::{env, Ctx};
use std::time::Instant;

#[derive(Clone, Debug, Default)]
pub struct EvalLimits {
    pub timeout_ms: Option<u64>,
    pub max_steps: Option<u64>,
    pub max_nodes: Option<usize>,
    pub max_solutions: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct EvalState {
    pub started_at: Instant,
    pub steps: u64,
}

impl EvalState {
    pub fn new() -> Self {
        Self {
            started_at: Instant::now(),
            steps: 0,
        }
    }
}

impl Default for EvalState {
    fn default() -> Self {
        Self::new()
    }
}

enum EvalOutcome {
    Value(Id),
    TailCall { env: env::EnvPtr, expr: Id },
}

#[allow(dead_code)]
pub fn eval(ctx: &mut Ctx, envp: &env::EnvPtr, expr: Id) -> Result<Id, String> {
    let mut state = EvalState::new();
    let limits = EvalLimits::default();
    eval_with_limits(ctx, envp, expr, &limits, &mut state)
}

pub fn eval_with_limits(
    ctx: &mut Ctx,
    envp: &env::EnvPtr,
    expr: Id,
    limits: &EvalLimits,
    state: &mut EvalState,
) -> Result<Id, String> {
    let mut cur_env = envp.clone();
    let mut cur_expr = expr;
    loop {
        match eval_expr(ctx, &cur_env, cur_expr, true, limits, state)? {
            EvalOutcome::Value(v) => return Ok(v),
            EvalOutcome::TailCall { env, expr } => {
                cur_env = env;
                cur_expr = expr;
            }
        }
    }
}

fn check_limits(ctx: &Ctx, limits: &EvalLimits, state: &mut EvalState) -> Result<(), String> {
    state.steps = state.steps.saturating_add(1);

    if let Some(max_steps) = limits.max_steps {
        if state.steps > max_steps {
            return Err(format!("step limit exceeded: {}", max_steps));
        }
    }
    if let Some(timeout_ms) = limits.timeout_ms {
        if state.started_at.elapsed().as_millis() > u128::from(timeout_ms) {
            return Err(format!("timeout exceeded: {} ms", timeout_ms));
        }
    }
    if let Some(max_nodes) = limits.max_nodes {
        if ctx.arena.node_count() > max_nodes {
            return Err(format!("node limit exceeded: {}", max_nodes));
        }
    }
    Ok(())
}

fn eval_to_value(
    ctx: &mut Ctx,
    envp: &env::EnvPtr,
    expr: Id,
    limits: &EvalLimits,
    state: &mut EvalState,
) -> Result<Id, String> {
    let mut cur_env = envp.clone();
    let mut cur_expr = expr;
    loop {
        match eval_expr(ctx, &cur_env, cur_expr, false, limits, state)? {
            EvalOutcome::Value(v) => return Ok(v),
            EvalOutcome::TailCall { env, expr } => {
                cur_env = env;
                cur_expr = expr;
            }
        }
    }
}

fn eval_expr(
    ctx: &mut Ctx,
    envp: &env::EnvPtr,
    expr: Id,
    tail: bool,
    limits: &EvalLimits,
    state: &mut EvalState,
) -> Result<EvalOutcome, String> {
    check_limits(ctx, limits, state)?;
    match ctx.arena.get(expr).clone() {
        Node::Nil | Node::Bool(_) | Node::Int(_) | Node::Str(_) | Node::Func(_) => {
            Ok(EvalOutcome::Value(expr))
        }
        Node::Sym(s) => {
            if s.starts_with(':') {
                Ok(EvalOutcome::Value(expr))
            } else {
                let v = env::env_get(envp, &s).ok_or_else(|| format!("unbound symbol: {s}"))?;
                Ok(EvalOutcome::Value(v))
            }
        }
        Node::List(xs) => eval_list(ctx, envp, xs, tail, limits, state),
    }
}

fn tail_or_eval(
    ctx: &mut Ctx,
    envp: &env::EnvPtr,
    expr: Id,
    tail: bool,
    limits: &EvalLimits,
    state: &mut EvalState,
) -> Result<EvalOutcome, String> {
    if tail {
        Ok(EvalOutcome::TailCall {
            env: envp.clone(),
            expr,
        })
    } else {
        Ok(EvalOutcome::Value(eval_to_value(
            ctx, envp, expr, limits, state,
        )?))
    }
}

fn eval_list(
    ctx: &mut Ctx,
    envp: &env::EnvPtr,
    xs: Vec<Id>,
    tail: bool,
    limits: &EvalLimits,
    state: &mut EvalState,
) -> Result<EvalOutcome, String> {
    if xs.is_empty() {
        return Ok(EvalOutcome::Value(ctx.arena.nil()));
    }

    let head = xs[0];
    let head_sym = ctx.arena.sym_name(head).map(|s| s.to_string());

    if let Some(h) = head_sym.as_deref() {
        match h {
            "quote" => {
                if xs.len() != 2 {
                    return Err("quote: arity".into());
                }
                return Ok(EvalOutcome::Value(xs[1]));
            }
            "begin" | "progn" => {
                for &e in xs.iter().skip(1).take(xs.len().saturating_sub(2)) {
                    let _ = eval_to_value(ctx, envp, e, limits, state)?;
                }
                if let Some(&e_last) = xs.last() {
                    if xs.len() == 1 {
                        return Ok(EvalOutcome::Value(ctx.arena.nil()));
                    }
                    return tail_or_eval(ctx, envp, e_last, tail, limits, state);
                }
                return Ok(EvalOutcome::Value(ctx.arena.nil()));
            }
            "if" => {
                if xs.len() != 4 {
                    return Err("if: (if cond then else)".into());
                }
                let c = eval_to_value(ctx, envp, xs[1], limits, state)?;
                let b = if ctx.arena.is_truthy(c) { xs[2] } else { xs[3] };
                return tail_or_eval(ctx, envp, b, tail, limits, state);
            }
            "define" | "defparameter" => {
                if xs.len() != 3 {
                    return Err(format!("{}: ({} sym expr)", h, h));
                }
                let name = ctx
                    .arena
                    .sym_name(xs[1])
                    .ok_or_else(|| format!("{}: need symbol", h))?
                    .to_string();
                let v = eval_to_value(ctx, envp, xs[2], limits, state)?;
                env::env_define(envp, name, v);
                return Ok(EvalOutcome::Value(v));
            }
            "defun" => {
                if xs.len() < 4 {
                    return Err("defun: (defun name (params...) body...)".into());
                }
                let name = ctx
                    .arena
                    .sym_name(xs[1])
                    .ok_or("defun: need symbol")?
                    .to_string();
                let params = parse_params(ctx, xs[2])?;
                let body: Vec<Id> = xs.iter().skip(3).copied().collect();
                let f = FuncObj {
                    params,
                    body,
                    env: envp.clone(),
                };
                let fid = ctx.arena.alloc(Node::Func(f));
                env::env_define(envp, name, fid);
                return Ok(EvalOutcome::Value(fid));
            }
            "set!" => {
                if xs.len() != 3 {
                    return Err("set!: (set! sym expr)".into());
                }
                let name = ctx
                    .arena
                    .sym_name(xs[1])
                    .ok_or("set!: need symbol")?
                    .to_string();
                let v = eval_to_value(ctx, envp, xs[2], limits, state)?;
                env::env_set_existing(envp, &name, v)
                    .ok_or_else(|| format!("set!: unbound symbol: {name}"))?;
                return Ok(EvalOutcome::Value(v));
            }
            "setf" => {
                if xs.len() < 3 || xs.len() % 2 == 0 {
                    return Err("setf: (setf sym expr [sym expr]...)".into());
                }
                let mut last = ctx.arena.nil();
                for pair in xs[1..].chunks(2) {
                    let name = ctx
                        .arena
                        .sym_name(pair[0])
                        .ok_or("setf: only symbol place is supported")?
                        .to_string();
                    let v = eval_to_value(ctx, envp, pair[1], limits, state)?;
                    env::env_set_existing(envp, &name, v)
                        .ok_or_else(|| format!("setf: unbound symbol: {name}"))?;
                    last = v;
                }
                return Ok(EvalOutcome::Value(last));
            }
            "incf" => {
                if xs.len() != 2 && xs.len() != 3 {
                    return Err("incf: (incf sym [delta])".into());
                }
                let name = ctx
                    .arena
                    .sym_name(xs[1])
                    .ok_or("incf: need symbol")?
                    .to_string();
                let cur = env::env_get(envp, &name)
                    .ok_or_else(|| format!("incf: unbound symbol: {name}"))?;
                let delta = if xs.len() == 3 {
                    eval_to_value(ctx, envp, xs[2], limits, state)?
                } else {
                    ctx.arena.int(1)
                };
                let next = match (ctx.arena.get(cur), ctx.arena.get(delta)) {
                    (Node::Int(a), Node::Int(b)) => ctx.arena.int(*a + *b),
                    _ => return Err("incf: expects integer variable and integer delta".into()),
                };
                env::env_set_existing(envp, &name, next)
                    .ok_or_else(|| format!("incf: unbound symbol: {name}"))?;
                return Ok(EvalOutcome::Value(next));
            }
            "push" => {
                if xs.len() != 3 {
                    return Err("push: (push item sym)".into());
                }
                let item = eval_to_value(ctx, envp, xs[1], limits, state)?;
                let name = ctx
                    .arena
                    .sym_name(xs[2])
                    .ok_or("push: only symbol place is supported")?
                    .to_string();
                let cur = env::env_get(envp, &name)
                    .ok_or_else(|| format!("push: unbound symbol: {name}"))?;
                let next = match ctx.arena.get(cur) {
                    Node::Nil => ctx.arena.list(vec![item]),
                    Node::List(xs0) => {
                        let mut out = Vec::with_capacity(xs0.len() + 1);
                        out.push(item);
                        out.extend_from_slice(xs0);
                        ctx.arena.list(out)
                    }
                    _ => return Err("push: target must be nil or list".into()),
                };
                env::env_set_existing(envp, &name, next)
                    .ok_or_else(|| format!("push: unbound symbol: {name}"))?;
                return Ok(EvalOutcome::Value(next));
            }
            "lambda" => {
                if xs.len() < 3 {
                    return Err("lambda: (lambda (params...) body...)".into());
                }
                let params = parse_params(ctx, xs[1])?;
                let body: Vec<Id> = xs.into_iter().skip(2).collect();
                let f = FuncObj {
                    params,
                    body,
                    env: envp.clone(),
                };
                return Ok(EvalOutcome::Value(ctx.arena.alloc(Node::Func(f))));
            }
            "cond" => {
                for &clause in xs.iter().skip(1) {
                    let items = match ctx.arena.get(clause) {
                        Node::List(items) if !items.is_empty() => items.clone(),
                        _ => return Err("cond: each clause must be a non-empty list".into()),
                    };
                    let test = items[0];
                    let body = items[1..].to_vec();
                    let test_true = if matches!(ctx.arena.sym_name(test), Some("t")) {
                        true
                    } else {
                        let v = eval_to_value(ctx, envp, test, limits, state)?;
                        ctx.arena.is_truthy(v)
                    };
                    if test_true {
                        if body.is_empty() {
                            return Ok(EvalOutcome::Value(ctx.arena.t()));
                        }
                        for &e in body.iter().take(body.len().saturating_sub(1)) {
                            let _ = eval_to_value(ctx, envp, e, limits, state)?;
                        }
                        let e_last = *body.last().unwrap_or(&ctx.arena.nil());
                        return tail_or_eval(ctx, envp, e_last, tail, limits, state);
                    }
                }
                return Ok(EvalOutcome::Value(ctx.arena.nil()));
            }
            "and" => {
                if xs.len() == 1 {
                    return Ok(EvalOutcome::Value(ctx.arena.t()));
                }
                for &e in xs.iter().skip(1).take(xs.len().saturating_sub(2)) {
                    let v = eval_to_value(ctx, envp, e, limits, state)?;
                    if !ctx.arena.is_truthy(v) {
                        return Ok(EvalOutcome::Value(v));
                    }
                }
                let e_last = *xs.last().unwrap_or(&ctx.arena.nil());
                return tail_or_eval(ctx, envp, e_last, tail, limits, state);
            }
            "or" => {
                if xs.len() == 1 {
                    return Ok(EvalOutcome::Value(ctx.arena.nil()));
                }
                for &e in xs.iter().skip(1).take(xs.len().saturating_sub(2)) {
                    let v = eval_to_value(ctx, envp, e, limits, state)?;
                    if ctx.arena.is_truthy(v) {
                        return Ok(EvalOutcome::Value(v));
                    }
                }
                let e_last = *xs.last().unwrap_or(&ctx.arena.nil());
                return tail_or_eval(ctx, envp, e_last, tail, limits, state);
            }
            "when" => {
                if xs.len() < 2 {
                    return Err("when: (when cond body...)".into());
                }
                let c = eval_to_value(ctx, envp, xs[1], limits, state)?;
                if !ctx.arena.is_truthy(c) {
                    return Ok(EvalOutcome::Value(ctx.arena.nil()));
                }
                if xs.len() == 2 {
                    return Ok(EvalOutcome::Value(ctx.arena.nil()));
                }
                for &e in xs.iter().skip(2).take(xs.len().saturating_sub(3)) {
                    let _ = eval_to_value(ctx, envp, e, limits, state)?;
                }
                let e_last = *xs.last().unwrap_or(&ctx.arena.nil());
                return tail_or_eval(ctx, envp, e_last, tail, limits, state);
            }
            "unless" => {
                if xs.len() < 2 {
                    return Err("unless: (unless cond body...)".into());
                }
                let c = eval_to_value(ctx, envp, xs[1], limits, state)?;
                if ctx.arena.is_truthy(c) {
                    return Ok(EvalOutcome::Value(ctx.arena.nil()));
                }
                if xs.len() == 2 {
                    return Ok(EvalOutcome::Value(ctx.arena.nil()));
                }
                for &e in xs.iter().skip(2).take(xs.len().saturating_sub(3)) {
                    let _ = eval_to_value(ctx, envp, e, limits, state)?;
                }
                let e_last = *xs.last().unwrap_or(&ctx.arena.nil());
                return tail_or_eval(ctx, envp, e_last, tail, limits, state);
            }
            "let" => {
                if xs.len() < 3 {
                    return Err("let: (let ((x e) ...) body...)".into());
                }
                let binds = xs[1];
                let body: Vec<Id> = xs.iter().skip(2).copied().collect();
                let bind_pairs = parse_let_bindings(ctx, binds)?;
                let mut evaluated: Vec<(String, Id)> = Vec::with_capacity(bind_pairs.len());
                for (name, expr_id) in bind_pairs {
                    let v = eval_to_value(ctx, envp, expr_id, limits, state)?;
                    evaluated.push((name, v));
                }
                let let_env = env::new_env(Some(envp.clone()));
                for (name, v) in evaluated {
                    env::env_define(&let_env, name, v);
                }
                return eval_body(ctx, &let_env, &body, tail, limits, state);
            }
            "let*" => {
                if xs.len() < 3 {
                    return Err("let*: (let* ((x e) ...) body...)".into());
                }
                let binds = xs[1];
                let body: Vec<Id> = xs.iter().skip(2).copied().collect();
                let bind_pairs = parse_let_bindings(ctx, binds)?;
                let let_env = env::new_env(Some(envp.clone()));
                for (name, expr_id) in bind_pairs {
                    let v = eval_to_value(ctx, &let_env, expr_id, limits, state)?;
                    env::env_define(&let_env, name, v);
                }
                return eval_body(ctx, &let_env, &body, tail, limits, state);
            }
            "labels" => {
                if xs.len() < 3 {
                    return Err("labels: (labels ((fn (args) body...) ...) body...)".into());
                }
                let defs_id = xs[1];
                let labels_env = env::new_env(Some(envp.clone()));
                let defs = match ctx.arena.get(defs_id) {
                    Node::List(items) => items.clone(),
                    _ => return Err("labels: definitions must be list".into()),
                };
                for &d in &defs {
                    let name = match ctx.arena.get(d) {
                        Node::List(items) if !items.is_empty() => ctx
                            .arena
                            .sym_name(items[0])
                            .ok_or("labels: fn name must be symbol")?
                            .to_string(),
                        _ => return Err("labels: bad function definition".into()),
                    };
                    env::env_define(&labels_env, name, ctx.arena.nil());
                }
                for &d in &defs {
                    let (name, params_id, body_ids) = match ctx.arena.get(d) {
                        Node::List(items) if items.len() >= 3 => {
                            let n = ctx
                                .arena
                                .sym_name(items[0])
                                .ok_or("labels: fn name must be symbol")?
                                .to_string();
                            (n, items[1], items[2..].to_vec())
                        }
                        _ => {
                            return Err(
                                "labels: each definition must be (name (params...) body...)".into(),
                            )
                        }
                    };
                    let params = parse_params(ctx, params_id)?;
                    let fid = ctx.arena.alloc(Node::Func(FuncObj {
                        params,
                        body: body_ids,
                        env: labels_env.clone(),
                    }));
                    env::env_set_existing(&labels_env, &name, fid)
                        .ok_or("labels: internal binding error")?;
                }
                let body: Vec<Id> = xs.iter().skip(2).copied().collect();
                return eval_body(ctx, &labels_env, &body, tail, limits, state);
            }
            "dolist" => {
                if xs.len() < 3 {
                    return Err("dolist: (dolist (var list) body...)".into());
                }
                let spec = xs[1];
                let (var_name, list_expr) = match ctx.arena.get(spec) {
                    Node::List(items) if items.len() >= 2 => {
                        let n = ctx
                            .arena
                            .sym_name(items[0])
                            .ok_or("dolist: var must be symbol")?
                            .to_string();
                        (n, items[1])
                    }
                    _ => return Err("dolist: iterator spec must be (var list-expr)".into()),
                };
                let lst = eval_to_value(ctx, envp, list_expr, limits, state)?;
                let xs0 = match ctx.arena.get(lst) {
                    Node::Nil => vec![],
                    Node::List(items) => items.clone(),
                    _ => return Err("dolist: list-expr must evaluate to list or nil".into()),
                };
                for item in xs0 {
                    let it_env = env::new_env(Some(envp.clone()));
                    env::env_define(&it_env, var_name.clone(), item);
                    for &e in xs.iter().skip(2) {
                        let _ = eval_to_value(ctx, &it_env, e, limits, state)?;
                    }
                }
                return Ok(EvalOutcome::Value(ctx.arena.nil()));
            }
            "funcall" => {
                if xs.len() < 2 {
                    return Err("funcall: need function".into());
                }
                let f = eval_to_value(ctx, envp, xs[1], limits, state)?;
                let mut args = Vec::with_capacity(xs.len().saturating_sub(2));
                for &a in xs.iter().skip(2) {
                    args.push(eval_to_value(ctx, envp, a, limits, state)?);
                }
                return apply_call(ctx, envp, f, &args, tail, limits, state);
            }
            "declare" | "in-package" => return Ok(EvalOutcome::Value(ctx.arena.nil())),
            _ => {}
        }
    }

    let f = eval_to_value(ctx, envp, head, limits, state)?;
    let mut args = Vec::with_capacity(xs.len().saturating_sub(1));
    for &a in xs.iter().skip(1) {
        args.push(eval_to_value(ctx, envp, a, limits, state)?);
    }
    apply_call(ctx, envp, f, &args, tail, limits, state)
}

fn eval_body(
    ctx: &mut Ctx,
    envp: &env::EnvPtr,
    body: &[Id],
    tail: bool,
    limits: &EvalLimits,
    state: &mut EvalState,
) -> Result<EvalOutcome, String> {
    if body.is_empty() {
        return Ok(EvalOutcome::Value(ctx.arena.nil()));
    }
    for &e in body.iter().take(body.len().saturating_sub(1)) {
        let _ = eval_to_value(ctx, envp, e, limits, state)?;
    }
    let last = *body.last().unwrap_or(&ctx.arena.nil());
    tail_or_eval(ctx, envp, last, tail, limits, state)
}

fn parse_params(ctx: &Ctx, params_list: Id) -> Result<Vec<String>, String> {
    match ctx.arena.get(params_list) {
        Node::List(xs) => {
            let mut ps = vec![];
            for &p in xs {
                let name = ctx
                    .arena
                    .sym_name(p)
                    .ok_or("lambda: param must be symbol")?;
                ps.push(name.to_string());
            }
            Ok(ps)
        }
        Node::Nil => Ok(vec![]),
        _ => Err("lambda: params must be list".into()),
    }
}

fn parse_let_bindings(ctx: &Ctx, binds: Id) -> Result<Vec<(String, Id)>, String> {
    match ctx.arena.get(binds) {
        Node::Nil => Ok(vec![]),
        Node::List(pairs) => {
            let mut out = vec![];
            for &p in pairs {
                match ctx.arena.get(p) {
                    Node::List(two) if two.len() == 2 => {
                        let name = ctx
                            .arena
                            .sym_name(two[0])
                            .ok_or("let: binding name must be symbol")?
                            .to_string();
                        out.push((name, two[1]));
                    }
                    _ => return Err("let: each binding must be (name expr)".into()),
                }
            }
            Ok(out)
        }
        _ => Err("let: bindings must be list".into()),
    }
}

fn apply_call(
    ctx: &mut Ctx,
    envp: &env::EnvPtr,
    f: Id,
    args: &[Id],
    tail: bool,
    limits: &EvalLimits,
    state: &mut EvalState,
) -> Result<EvalOutcome, String> {
    check_limits(ctx, limits, state)?;
    if let Some(name) = ctx.arena.sym_name(f).map(|s| s.to_string()) {
        let v = apply_builtin(ctx, envp, &name, args, limits, state)?;
        return Ok(EvalOutcome::Value(v));
    }
    match ctx.arena.get(f).clone() {
        Node::Func(fun) => {
            if fun.params.len() != args.len() {
                return Err(format!(
                    "arity mismatch: expected {}, got {}",
                    fun.params.len(),
                    args.len()
                ));
            }
            let call_env = env::new_env(Some(fun.env.clone()));
            for (p, &v) in fun.params.iter().zip(args.iter()) {
                env::env_define(&call_env, p.clone(), v);
            }
            if fun.body.is_empty() {
                return Ok(EvalOutcome::Value(ctx.arena.nil()));
            }
            if tail {
                let mut begin = Vec::with_capacity(fun.body.len() + 1);
                begin.push(ctx.arena.sym("begin"));
                begin.extend_from_slice(&fun.body);
                let expr = ctx.arena.list(begin);
                Ok(EvalOutcome::TailCall {
                    env: call_env,
                    expr,
                })
            } else {
                let begin_sym = ctx.arena.sym("begin");
                let mut begin = Vec::with_capacity(fun.body.len() + 1);
                begin.push(begin_sym);
                begin.extend_from_slice(&fun.body);
                let body_expr = ctx.arena.list(begin);
                let v = eval_with_limits(
                    ctx,
                    &call_env,
                    body_expr,
                    limits,
                    state,
                )?;
                Ok(EvalOutcome::Value(v))
            }
        }
        _ => Err("attempt to call non-function".into()),
    }
}

fn apply_builtin(
    ctx: &mut Ctx,
    envp: &env::EnvPtr,
    name: &str,
    args: &[Id],
    limits: &EvalLimits,
    state: &mut EvalState,
) -> Result<Id, String> {
    match name {
        "cons" => {
            if args.len() != 2 {
                return Err("cons: arity 2".into());
            }
            let a = args[0];
            let d = args[1];
            match ctx.arena.get(d) {
                Node::Nil => Ok(ctx.arena.list(vec![a])),
                Node::List(xs) => {
                    let mut out = Vec::with_capacity(xs.len() + 1);
                    out.push(a);
                    out.extend_from_slice(xs);
                    Ok(ctx.arena.list(out))
                }
                _ => Err("cons: second arg must be list or nil".into()),
            }
        }
        "car" => {
            if args.len() != 1 {
                return Err("car: arity 1".into());
            }
            match ctx.arena.get(args[0]) {
                Node::Nil => Ok(ctx.arena.nil()),
                Node::List(xs) => Ok(xs.first().copied().unwrap_or_else(|| ctx.arena.nil())),
                _ => Err("car: arg must be list or nil".into()),
            }
        }
        "cdr" => {
            if args.len() != 1 {
                return Err("cdr: arity 1".into());
            }
            match ctx.arena.get(args[0]) {
                Node::Nil => Ok(ctx.arena.nil()),
                Node::List(xs) => {
                    if xs.len() <= 1 {
                        Ok(ctx.arena.nil())
                    } else {
                        Ok(ctx.arena.list(xs[1..].to_vec()))
                    }
                }
                _ => Err("cdr: arg must be list or nil".into()),
            }
        }

        "list" => Ok(ctx.arena.list(args.to_vec())),
        "funcall" => {
            if args.is_empty() {
                return Err("funcall: need function".into());
            }
            let out = apply_call(ctx, envp, args[0], &args[1..], false, limits, state)?;
            match out {
                EvalOutcome::Value(v) => Ok(v),
                EvalOutcome::TailCall { env, expr } => eval_with_limits(ctx, &env, expr, limits, state),
            }
        }
        "null?" | "null" => {
            if args.len() != 1 {
                return Err("null: arity 1".into());
            }
            Ok(if ctx.arena.is_nil(args[0]) {
                ctx.arena.t()
            } else {
                ctx.arena.nil()
            })
        }
        "atom?" | "atom" => {
            if args.len() != 1 {
                return Err("atom: arity 1".into());
            }
            let b = !matches!(ctx.arena.get(args[0]), Node::List(_));
            Ok(if b { ctx.arena.t() } else { ctx.arena.nil() })
        }
        "pair?" | "consp" => {
            if args.len() != 1 {
                return Err("consp: arity 1".into());
            }
            let b = matches!(ctx.arena.get(args[0]), Node::List(xs) if !xs.is_empty());
            Ok(if b { ctx.arena.t() } else { ctx.arena.nil() })
        }
        "not" => {
            if args.len() != 1 {
                return Err("not: arity 1".into());
            }
            Ok(if ctx.arena.is_nil(args[0]) {
                ctx.arena.t()
            } else {
                ctx.arena.nil()
            })
        }
        "eq?" | "equal" | "eq" => {
            if args.len() != 2 {
                return Err("eq/equal: arity 2".into());
            }
            let b = deep_eq(&ctx.arena, args[0], args[1]);
            Ok(if b { ctx.arena.t() } else { ctx.arena.nil() })
        }

        "assoc" => {
            if args.len() < 2 {
                return Err("assoc: (assoc key alist ...)".into());
            }
            let key = args[0];
            let alist = args[1];
            let xs = match ctx.arena.get(alist) {
                Node::Nil => return Ok(ctx.arena.nil()),
                Node::List(xs) => xs.clone(),
                _ => return Err("assoc: alist must be list".into()),
            };
            for pair in xs {
                match ctx.arena.get(pair) {
                    Node::List(p) if p.len() == 2 => {
                        if deep_eq(&ctx.arena, p[0], key) {
                            return Ok(pair);
                        }
                    }
                    _ => return Err("assoc: each element must be (key value)".into()),
                }
            }
            Ok(ctx.arena.nil())
        }

        "append" => {
            if args.len() != 2 {
                return Err("append: arity 2".into());
            }
            let a1 = args[0];
            let a2 = args[1];

            let mut out = vec![];
            match ctx.arena.get(a1) {
                Node::Nil => {}
                Node::List(xs) => out.extend_from_slice(xs),
                _ => return Err("append: first arg must be list".into()),
            }
            match ctx.arena.get(a2) {
                Node::Nil => {}
                Node::List(xs) => out.extend_from_slice(xs),
                _ => return Err("append: second arg must be list".into()),
            }
            Ok(ctx.arena.list(out))
        }

        "map" | "mapcar" => {
            if args.len() != 2 {
                return Err("map/mapcar: arity 2".into());
            }
            let f = args[0];
            let lst = args[1];
            let xs = match ctx.arena.get(lst) {
                Node::Nil => return Ok(ctx.arena.nil()),
                Node::List(xs) => xs.clone(),
                _ => return Err("map/mapcar: second arg must be list".into()),
            };

            let mut ys = Vec::with_capacity(xs.len());
            for x in xs {
                let out = apply_call(ctx, envp, f, &[x], false, limits, state)?;
                let y = match out {
                    EvalOutcome::Value(v) => v,
                    EvalOutcome::TailCall { env, expr } => {
                        eval_with_limits(ctx, &env, expr, limits, state)?
                    }
                };
                ys.push(y);
            }
            Ok(ctx.arena.list(ys))
        }

        "reverse" | "nreverse" => {
            if args.len() != 1 {
                return Err("reverse: arity 1".into());
            }
            match ctx.arena.get(args[0]) {
                Node::Nil => Ok(ctx.arena.nil()),
                Node::List(xs) => {
                    let mut ys = xs.clone();
                    ys.reverse();
                    Ok(ctx.arena.list(ys))
                }
                _ => Err("reverse: arg must be list or nil".into()),
            }
        }
        "cadr" => {
            if args.len() != 1 {
                return Err("cadr: arity 1".into());
            }
            match ctx.arena.get(args[0]) {
                Node::List(xs) if xs.len() >= 2 => Ok(xs[1]),
                _ => Ok(ctx.arena.nil()),
            }
        }
        "caddr" => {
            if args.len() != 1 {
                return Err("caddr: arity 1".into());
            }
            match ctx.arena.get(args[0]) {
                Node::List(xs) if xs.len() >= 3 => Ok(xs[2]),
                _ => Ok(ctx.arena.nil()),
            }
        }
        "cddr" => {
            if args.len() != 1 {
                return Err("cddr: arity 1".into());
            }
            match ctx.arena.get(args[0]) {
                Node::List(xs) if xs.len() > 2 => Ok(ctx.arena.list(xs[2..].to_vec())),
                Node::List(_) | Node::Nil => Ok(ctx.arena.nil()),
                _ => Err("cddr: arg must be list or nil".into()),
            }
        }
        "cdddr" => {
            if args.len() != 1 {
                return Err("cdddr: arity 1".into());
            }
            match ctx.arena.get(args[0]) {
                Node::List(xs) if xs.len() > 3 => Ok(ctx.arena.list(xs[3..].to_vec())),
                Node::List(_) | Node::Nil => Ok(ctx.arena.nil()),
                _ => Err("cdddr: arg must be list or nil".into()),
            }
        }

        "+" => {
            let mut acc: i64 = 0;
            for &a in args {
                match ctx.arena.get(a) {
                    Node::Int(n) => acc += *n,
                    _ => return Err("+ expects int".into()),
                }
            }
            Ok(ctx.arena.int(acc))
        }
        "-" => {
            if args.is_empty() {
                return Err("- expects at least one int".into());
            }
            let mut acc = match ctx.arena.get(args[0]) {
                Node::Int(n) => *n,
                _ => return Err("- expects int".into()),
            };
            if args.len() == 1 {
                return Ok(ctx.arena.int(-acc));
            }
            for &a in args.iter().skip(1) {
                match ctx.arena.get(a) {
                    Node::Int(n) => acc -= *n,
                    _ => return Err("- expects int".into()),
                }
            }
            Ok(ctx.arena.int(acc))
        }

        "print" => {
            if args.len() != 1 {
                return Err("print: arity 1".into());
            }
            println!("{}", ctx.arena.fmt(args[0]));
            Ok(args[0])
        }
        "format" => {
            if args.len() < 2 {
                return Err("format: need destination and format string".into());
            }
            let fmt_s = match ctx.arena.get(args[1]) {
                Node::Str(s) => s.clone(),
                _ => return Err("format: second arg must be string".into()),
            };
            let mut out = String::new();
            let mut ai = 2usize;
            let bytes = fmt_s.as_bytes();
            let mut i = 0usize;
            while i < bytes.len() {
                if bytes[i] == b'~' && i + 1 < bytes.len() {
                    match bytes[i + 1] {
                        b'S' | b's' => {
                            if ai < args.len() {
                                out.push_str(&ctx.arena.fmt(args[ai]));
                                ai += 1;
                            }
                            i += 2;
                            continue;
                        }
                        b'%' => {
                            out.push('\n');
                            i += 2;
                            continue;
                        }
                        _ => {}
                    }
                }
                out.push(bytes[i] as char);
                i += 1;
            }
            print!("{out}");
            Ok(if args.len() > 2 {
                args[2]
            } else {
                ctx.arena.nil()
            })
        }

        _ => Err(format!("unknown builtin: {name}")),
    }
}

fn deep_eq(a: &Arena, x: Id, y: Id) -> bool {
    let mut stack = vec![(x, y)];
    while let Some((lx, ly)) = stack.pop() {
        match (a.get(lx), a.get(ly)) {
            (Node::Nil, Node::Nil) => {}
            (Node::Bool(b1), Node::Bool(b2)) if b1 == b2 => {}
            (Node::Int(n1), Node::Int(n2)) if n1 == n2 => {}
            (Node::Str(s1), Node::Str(s2)) if s1 == s2 => {}
            (Node::Sym(s1), Node::Sym(s2)) if s1 == s2 => {}
            (Node::List(xs), Node::List(ys)) => {
                if xs.len() != ys.len() {
                    return false;
                }
                for (&p, &q) in xs.iter().zip(ys) {
                    stack.push((p, q));
                }
            }
            _ => return false,
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lisp::Ctx;

    #[test]
    fn max_steps_limit_is_enforced() {
        let mut ctx = Ctx::new();
        let plus = ctx.arena.sym("+");
        let one = ctx.arena.int(1);
        let two = ctx.arena.int(2);
        let expr = ctx.arena.list(vec![plus, one, two]);
        let mut state = EvalState::new();
        let limits = EvalLimits {
            max_steps: Some(1),
            ..EvalLimits::default()
        };
        let envp = ctx.env.clone();
        let err = eval_with_limits(&mut ctx, &envp, expr, &limits, &mut state)
            .expect_err("step limit should fail");
        assert!(err.contains("step limit exceeded"));
    }

    #[test]
    fn max_nodes_limit_is_enforced() {
        let mut ctx = Ctx::new();
        let expr = ctx.arena.int(1);
        let mut state = EvalState::new();
        let limits = EvalLimits {
            max_nodes: Some(0),
            ..EvalLimits::default()
        };
        let envp = ctx.env.clone();
        let err = eval_with_limits(&mut ctx, &envp, expr, &limits, &mut state)
            .expect_err("node limit should fail");
        assert!(err.contains("node limit exceeded"));
    }
}
