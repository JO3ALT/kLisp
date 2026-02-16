# Repository Guidelines

## Project Structure & Module Organization
`klisp` is a small Rust interpreter crate.

- `src/main.rs`: binary entry point.
- `src/parse.rs`: S-expression parser wiring and AST-node allocation.
- `src/sexpr.pest`: grammar definition consumed by `pest`.
- `src/lisp/arena.rs`: arena-backed runtime node storage.
- `src/lisp/env.rs`: lexical environment frames and symbol lookup/update.
- `src/lisp/eval.rs`: evaluator, special forms, and builtins.
- `src/lisp/mod.rs`: Lisp runtime context and module exports.

Keep parser and grammar changes in sync (`parse.rs` + `sexpr.pest`) in the same PR.

## Build, Test, and Development Commands
- `cargo check`: fast type/borrow-check pass during development.
- `cargo run`: build and run the binary.
- `cargo test`: run unit and integration tests.
- `cargo fmt`: apply standard Rust formatting.
- `cargo clippy -- -D warnings`: lint and fail on warnings.

Run `cargo fmt` and `cargo clippy -- -D warnings` before opening a PR.

## Coding Style & Naming Conventions
Use standard Rust style (rustfmt defaults, 4-space indentation, no tabs).

- Types/traits/enums: `UpperCamelCase` (`FuncObj`, `EnvFrame`)
- Functions/modules/files: `snake_case` (`parse_program`, `env_set_existing`)
- Constants: `UPPER_SNAKE_CASE`

Prefer small, focused functions and explicit error messages (`Result<_, String>`) for evaluator/parser failures.

## Testing Guidelines
There is currently no committed test suite; new features should include tests.

- Unit tests: inline with modules using `#[cfg(test)] mod tests`.
- Integration tests: add under `tests/` for end-to-end interpreter behavior.
- Test names: describe behavior, e.g. `eval_let_binds_in_parallel`.

Minimum expectation for language changes: one success-path test and one error-path test.

## Commit & Pull Request Guidelines
This repository has no commit history yet, so no established convention exists. Start with concise, imperative commits, preferably Conventional Commit style:

- `feat: add lambda parameter arity check`
- `fix: reject malformed let binding list`

PRs should include:
- Short summary of behavior changes
- Linked issue (if applicable)
- Notes on grammar/runtime impact
- Test evidence (`cargo test`, `cargo clippy`)
