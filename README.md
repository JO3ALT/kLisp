# klisp / Lisp-on-Prolog Playground

[日本語版はこちら](README.ja.md)

## Overview
This project is an experiment: building a small Lisp interpreter in Rust, then running Prolog on top of it.  
Motivation: **I wanted to try building a Lisp and running Prolog on it.**

## Highlights
- Lightweight Rust Lisp runtime (arena-based)
- Prolog frontend (`fprolog`) lowering to Lisp S-expressions
- `sexp2prolog_stream.awk` for Prolog-style output formatting
- Cross-check tests against `SBCL / ECL / SWI-Prolog`

## Architecture (ASCII)
```text
Prolog source
   |
   v
fprolog (frontend)
   |  Lisp S-expressions
   v
prolog-cl.lisp (meta-interpreter on Lisp)
   |
   |  S-expression answers
   v
sexp2prolog_stream.awk
   |
   v
Prolog-style output
```

## Usage
### 1. Run as Lisp
```bash
printf "(+ 1 2)\n" | cargo run -q
```

### 2. End-to-end with `fprolog` + `klisp` (recommended for small/medium cases)
```bash
{ cat prolog-cl.lisp; fprolog < examples/prolog_small/01_family.pl; } \
  | klisp \
  | awk -f sexp2prolog_stream.awk
```

### 3. Heavy examples (8-queens / sudoku9)
Heavy examples are documented with the `SBCL` route at this stage.

### 4. End-to-end with `fprolog` + SBCL (heavy-example route)
```bash
{ cat prolog-cl.lisp; fprolog < examples_sudoku9.pl; } \
  | sbcl --script /dev/stdin \
  | awk -f sexp2prolog_stream.awk
```

### 5. Runtime guard examples
```bash
KLISP_MAX_STEPS=100000 cargo run -q < program.lisp
KLISP_TIMEOUT_MS=1000 KLISP_SHOW_STATS=1 cargo run -q < program.lisp
```

### 6. Run focused comparison test only
```bash
cargo test --test fprolog_swipl_compare -- --nocapture
```

## Small Prolog Samples
`examples/prolog_small/` includes lightweight Prolog-like examples:
- `01_family.pl`
  - Basic fact query with one variable (`parent/2`).
  - Run: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/01_family.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `02_grandparent.pl`
  - Rule composition with conjunction (`grandparent/2`).
  - Run: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/02_grandparent.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `03_ancestor_recursive.pl`
  - Recursive rule over parent links (`ancestor/2`).
  - Run: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/03_ancestor_recursive.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `04_colleague.pl`
  - Join-like query with inequality guard.
  - Run: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/04_colleague.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `05_path.pl`
  - Graph reachability with recursion/backtracking.
  - Run: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/05_path.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `06_once.pl`
  - `once/1` behavior (keep first solution only).
  - Run: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/06_once.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `07_cut.pl`
  - `!` (cut) behavior in a simple clause.
  - Run: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/07_cut.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `08_true_fail_or.pl`
  - Control flow using `true/0`, `fail/0`, and `;`.
  - Run: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/08_true_fail_or.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `09_disjunction.pl`
  - Disjunction in rule body (`A ; B`).
  - Run: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/09_disjunction.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `10_alias_unify.pl`
  - Unification-style equality via repeated variable (`eqpair(X,X)`).
  - Run: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/10_alias_unify.pl; } | klisp | awk -f sexp2prolog_stream.awk`

Rule-based translation demo:
- `11_translation_en_ja.pl`
  - Minimal English sentence pattern `svo/3` -> Japanese structured output (`ja/3`).
  - Run: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/11_translation_en_ja.pl; } | klisp | awk -f sexp2prolog_stream.awk`

## Sample I/O
```bash
$ { cat prolog-cl.lisp; fprolog < examples/prolog_small/01_family.pl; } | klisp | awk -f sexp2prolog_stream.awk
X = bob.
X = liz.
```

```bash
$ { cat prolog-cl.lisp; fprolog < examples/prolog_small/11_translation_en_ja.pl; } | klisp | awk -f sexp2prolog_stream.awk
J = ja(watashi,ringo,suki).
```

```bash
$ fprolog < examples/prolog_small/01_family.pl
(progn
  ...
)
```

## Language Spec (Focused)
### Lisp subset
- Data: `nil`, `t`, integers, strings, symbols, lists
- Syntax: quote `'x`, list `(a b c)`, `;` comments
- Special forms: `quote`, `begin`, `if`, `define`, `set!`, `lambda`, `let`
- Builtins: `cons/car/cdr/list/null?/atom?/pair?/eq?/assoc/append/map/+`

### Prolog subset (via `fprolog`)
- Facts/rules/queries: `fact.`, `head :- body.`, `?- goal.`
- Variables: uppercase-initial or `_`
- Control: `,` `;` `!` `once/1` `true/0` `fail/0`
- Lists: `[a,b]`, `[H|T]`

## Runtime Guards (Lisp mode)
- `KLISP_TIMEOUT_MS`
- `KLISP_MAX_STEPS`
- `KLISP_MAX_NODES`
- `KLISP_MAX_SOLUTIONS`
- `KLISP_SHOW_STATS=1`

## SBCL / ECL / SWI-Prolog Comparison
The project includes automated cross-checks using the same Prolog cases (`tests/cases/*.pl`):
- `fprolog -> prolog-cl.lisp -> SBCL -> awk`
- `fprolog -> prolog-cl.lisp -> ECL -> awk`
- Native `SWI-Prolog` execution

Run:
```bash
cargo test --test fprolog_swipl_compare -- --nocapture
```

If all pass, SBCL and ECL pipelines match SWI-Prolog output for the covered subset.
`cut` (`!`) corner semantics may still differ slightly across runtimes; current docs treat that as runtime-specific behavior.

## Test
```bash
cargo test
```

## License
MIT. See [LICENSE](LICENSE).
