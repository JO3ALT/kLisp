mod lisp;
mod parse;

use std::env;
use std::io::{self, Read};
use std::thread;

fn env_opt_u64(name: &str) -> Result<Option<u64>, String> {
    match env::var(name) {
        Ok(v) => {
            let n = v
                .trim()
                .parse::<u64>()
                .map_err(|_| format!("invalid {}: {}", name, v))?;
            Ok(Some(n))
        }
        Err(env::VarError::NotPresent) => Ok(None),
        Err(e) => Err(format!("invalid {}: {}", name, e)),
    }
}

fn env_opt_usize(name: &str) -> Result<Option<usize>, String> {
    match env::var(name) {
        Ok(v) => {
            let n = v
                .trim()
                .parse::<usize>()
                .map_err(|_| format!("invalid {}: {}", name, v))?;
            Ok(Some(n))
        }
        Err(env::VarError::NotPresent) => Ok(None),
        Err(e) => Err(format!("invalid {}: {}", name, e)),
    }
}

fn worker_stack_size_bytes() -> Result<usize, String> {
    let mb = env_opt_usize("KLISP_STACK_MB")?.unwrap_or(1024);
    mb.checked_mul(1024 * 1024)
        .ok_or_else(|| format!("invalid KLISP_STACK_MB: {}", mb))
}

fn lisp_limits_from_env() -> Result<lisp::eval::EvalLimits, String> {
    Ok(lisp::eval::EvalLimits {
        timeout_ms: env_opt_u64("KLISP_TIMEOUT_MS")?,
        max_steps: env_opt_u64("KLISP_MAX_STEPS")?,
        max_nodes: env_opt_usize("KLISP_MAX_NODES")?,
        max_solutions: env_opt_usize("KLISP_MAX_SOLUTIONS")?,
    })
}

fn main() {
    let mut src = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut src) {
        eprintln!("read error: {e}");
        std::process::exit(1);
    }

    let src_owned = src.clone();
    let stack_size = match worker_stack_size_bytes() {
        Ok(v) => v,
        Err(e) => {
            eprintln!("error: {e}");
            std::process::exit(1);
        }
    };
    let result = thread::Builder::new()
        .name("klisp-worker".to_string())
        .stack_size(stack_size)
        .spawn(move || run_lisp(&src_owned))
        .map_err(|e| format!("failed to start worker thread: {e}"))
        .and_then(|h| {
            h.join()
                .map_err(|_| "worker thread panicked".to_string())?
        });

    if let Err(e) = result {
        eprintln!("error: {e}");
        std::process::exit(1);
    }
}

fn run_lisp(src: &str) -> Result<(), String> {
    if src.trim().is_empty() {
        return Ok(());
    }

    let mut ctx = lisp::Ctx::new();
    let limits = lisp_limits_from_env()?;
    let mut state = lisp::eval::EvalState::new();
    let echo_results = env::var("KLISP_ECHO_RESULTS").ok().as_deref() == Some("1");
    let exprs = parse::parse_program(&mut ctx.arena, src)?;
    let mut produced = 0usize;
    for expr in exprs {
        if let Some(max_solutions) = limits.max_solutions {
            if produced >= max_solutions {
                return Err(format!("solution limit exceeded: {}", max_solutions));
            }
        }
        let envp = ctx.env.clone();
        let v = lisp::eval::eval_with_limits(&mut ctx, &envp, expr, &limits, &mut state)?;
        if echo_results {
            println!("{}", ctx.arena.fmt(v));
        }
        produced += 1;
    }
    if env::var("KLISP_SHOW_STATS").ok().as_deref() == Some("1") {
        eprintln!(
            "stats: steps={} nodes={} elapsed_ms={}",
            state.steps,
            ctx.arena.node_count(),
            state.started_at.elapsed().as_millis()
        );
    }
    Ok(())
}
