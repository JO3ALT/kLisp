# Runtime Specification

## Overview
`klisp` provides three CLI modes:
- `lisp`: parse and evaluate S-expressions from stdin.
- `prolog-in`: translate Prolog syntax to Lisp S-expressions.
- `prolog-out`: convert Lisp solver output back to Prolog-style lines via `sexp2prolog_stream.awk`.

## Minimal Runtime Guards (Lisp Mode)
`lisp` mode supports optional execution guardrails via environment variables.
If unset, behavior is unchanged (no limits).

- `KLISP_TIMEOUT_MS`: hard timeout in milliseconds.
- `KLISP_MAX_STEPS`: maximum evaluator steps.
- `KLISP_MAX_NODES`: maximum arena node count.
- `KLISP_MAX_SOLUTIONS`: maximum number of top-level results printed.
- `KLISP_SHOW_STATS=1`: print runtime stats to stderr.

### Limit Behavior
- Any exceeded limit terminates evaluation with an error and non-zero exit.
- `KLISP_MAX_SOLUTIONS` is checked before each top-level form output.
- Step, timeout, and node limits are checked during evaluation/apply traversal.

## Observability
When `KLISP_SHOW_STATS=1`, `lisp` mode emits one line to stderr:
- `steps=<n>`: evaluator step count.
- `nodes=<n>`: final arena node count.
- `elapsed_ms=<n>`: elapsed wall-clock time in milliseconds.

## Examples
```bash
# Fail fast after 10000 eval steps
KLISP_MAX_STEPS=10000 cargo run -q -- lisp < program.lisp

# 1-second timeout + node cap + stats
KLISP_TIMEOUT_MS=1000 KLISP_MAX_NODES=200000 KLISP_SHOW_STATS=1 \
  cargo run -q -- lisp < program.lisp
```
