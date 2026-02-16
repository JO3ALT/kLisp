# klisp / Lisp-on-Prolog Playground

[English version](README.md)

## 概要
Rustで小さなLispインタプリタを作り、その上でPrologを動かす実験プロジェクトです。  
「**Lispを作ってPrologを動かすのをやってみたかったので試してみました**」という動機で作っています。

## 特徴
- Rust製の軽量Lisp実行系（アリーナ方式）
- Prologフロントエンド（`fprolog`）からLisp S式へ変換
- Lisp出力をProlog風に整形する `sexp2prolog_stream.awk`
- `SBCL / ECL / SWI-Prolog` 比較テスト

## アーキテクチャ（ASCII）
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

## 使い方
### 1. Lispとして実行
```bash
printf "(+ 1 2)\n" | cargo run -q
```

### 2. `fprolog` + `klisp` で実行（小〜中規模向け）
```bash
{ cat prolog-cl.lisp; fprolog < examples/prolog_small/01_family.pl; } \
  | klisp \
  | awk -f sexp2prolog_stream.awk
```

### 3. 重めの例（8queen / sudoku9）
現状では重めの例は `SBCL` 経路を主なサンプルとしています。

### 4. `fprolog` + SBCL で実行（重めサンプル向け）
```bash
{ cat prolog-cl.lisp; fprolog < examples_sudoku9.pl; } \
  | sbcl --script /dev/stdin \
  | awk -f sexp2prolog_stream.awk
```

### 5. 実行ガード付き実行
```bash
KLISP_MAX_STEPS=100000 cargo run -q < program.lisp
KLISP_TIMEOUT_MS=1000 KLISP_SHOW_STATS=1 cargo run -q < program.lisp
```

### 6. 比較テストだけを実行
```bash
cargo test --test fprolog_swipl_compare -- --nocapture
```

## 小さめの Prolog サンプル
`examples/prolog_small/` に軽量サンプルを入れています。
- `01_family.pl`
  - 事実問い合わせの基本例（`parent/2`）。
  - 実行: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/01_family.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `02_grandparent.pl`
  - 連言による規則合成（`grandparent/2`）。
  - 実行: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/02_grandparent.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `03_ancestor_recursive.pl`
  - 再帰規則による祖先探索（`ancestor/2`）。
  - 実行: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/03_ancestor_recursive.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `04_colleague.pl`
  - 不等号ガード付きの結合的問い合わせ。
  - 実行: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/04_colleague.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `05_path.pl`
  - グラフ到達可能性（再帰＋バックトラック）。
  - 実行: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/05_path.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `06_once.pl`
  - `once/1`（最初の解だけ残す）確認用。
  - 実行: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/06_once.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `07_cut.pl`
  - `!`（cut）の基本動作確認用。
  - 実行: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/07_cut.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `08_true_fail_or.pl`
  - `true/0` `fail/0` `;` の制御フロー例。
  - 実行: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/08_true_fail_or.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `09_disjunction.pl`
  - 規則本体の選言（`A ; B`）例。
  - 実行: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/09_disjunction.pl; } | klisp | awk -f sexp2prolog_stream.awk`
- `10_alias_unify.pl`
  - 同一変数の単一化（`eqpair(X,X)`）例。
  - 実行: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/10_alias_unify.pl; } | klisp | awk -f sexp2prolog_stream.awk`

簡易翻訳のルールベース例:
- `11_translation_en_ja.pl`
  - `svo/3` の最小文型を日本語構造（`ja/3`）に変換する見本です。
  - 実行: `{ cat prolog-cl.lisp; fprolog < examples/prolog_small/11_translation_en_ja.pl; } | klisp | awk -f sexp2prolog_stream.awk`

## サンプル入出力
```bash
$ { cat prolog-cl.lisp; fprolog < examples/prolog_small/01_family.pl; } | klisp | awk -f sexp2prolog_stream.awk
X = bob.
X = liz.
```

```bash
$ { cat prolog-cl.lisp; fprolog < examples/prolog_small/11_translation_en_ja.pl; } | klisp | awk -f sexp2prolog_stream.awk
J = ja(watashi,ringo,suki).
```

## 言語仕様（要点）
### Lispサブセット
- データ: `nil`, `t`, 整数, 文字列, シンボル, リスト
- 構文: クォート `'x`, リスト `(a b c)`, `;` コメント
- 特殊形式: `quote`, `begin`, `if`, `define`, `set!`, `lambda`, `let`
- 組み込み: `cons/car/cdr/list/null?/atom?/pair?/eq?/assoc/append/map/+`

### Prologサブセット（`fprolog`経由）
- 事実/規則/問い合わせ: `fact.`, `head :- body.`, `?- goal.`
- 変数: 大文字始まりまたは `_`
- 制御: `,` `;` `!` `once/1` `true/0` `fail/0`
- リスト: `[a,b]`, `[H|T]`

## 実行ガード（Lispモード）
- `KLISP_TIMEOUT_MS`
- `KLISP_MAX_STEPS`
- `KLISP_MAX_NODES`
- `KLISP_MAX_SOLUTIONS`
- `KLISP_SHOW_STATS=1`

## SBCL / ECL / SWI-Prolog 比較
同じPrologケース（`tests/cases/*.pl`）に対して、以下3経路を自動比較します。
- `fprolog -> prolog-cl.lisp -> SBCL -> awk`
- `fprolog -> prolog-cl.lisp -> ECL -> awk`
- `SWI-Prolog` ネイティブ実行

実行:
```bash
cargo test --test fprolog_swipl_compare -- --nocapture
```

このテストが通れば、対象サブセットで SBCL/ECL 経路の出力が SWI-Prolog と一致していることを確認できます。
`cut`（`!`）の境界的な意味論は実行系差が残る場合があり、現状は処理系依存の挙動として扱います。

## テスト
```bash
cargo test
```

## ライセンス
MIT。詳細は [LICENSE](LICENSE) を参照してください。
