# 関数型言語OCamlで作る代数計算機 サンプルコード

書籍『関数型言語OCamlで作る代数計算機』（2026年11月版）のサンプルコードです。

> 以前の版のサンプルコードは [icchon/ocaml-algebraic-calculator-book](https://github.com/icchon/ocaml-algebraic-calculator-book) にあります。
> 以前の版の本をお持ちの方は、そちらを使ってください。

## 準備

OCaml 5.5.1 を opam で入れます（本書の付録B「環境構築」と同じ手順です）。

```sh
brew install opam       # macOS
sudo apt install opam   # Debian/Ubuntu（Windowsの場合はWSL上で）

opam init
opam switch create default 5.5.1
eval $(opam env)
```

このリポジトリを取ってきて、ビルドします。

```sh
git clone https://github.com/icchon/ocaml-algebraic-calculator.git
cd ocaml-algebraic-calculator
./build.sh
```

## 対話環境で計算する

このフォルダで `ocaml` を起動すると、本書でつくるモジュールがすべて読み込まれた状態になります（`.ocamlinit` が読み込みます）。

```
$ ocaml
# Rational.add (1, 2) (1, 3);;
- : Rational.t = (5, 6)
# Quadratic.solve (1, 1) (0, 1) (-2, 1);;
- : Quadratic.t * Quadratic.t =
(([([2], (1, 1))], []), ([([2], (-1, 1))], []))
```

答えは、表示のための変換をせず、OCaml のデータのまま表示されます。

- 有理数は `(分子, 分母)` の組です。`(5, 6)` は 5/6 のことです。
- 平方根を添加した体（`Q_sqrt`）の元は、「素数のリスト（小さい順）と係数の組」のリストです。
  `[([], (1, 1)); ([2], (1, 1))]` は 1 + √2、`[([2; 3], (5, 1))]` は 5√6 です。空のリスト `[]` は 0 です。
- 二次方程式の解の体（`Quadratic`）の元は、`(実部, 虚部)` の組です。上の答えは x = ±√2 です。

本書では、同じ答えを数式に組んで載せています。

## フォルダの中身

| フォルダ | 中身 |
|---|---|
| `src/` | 本書でつくるモジュール（整数・有理数・複素数・多項式・拡大体・有理関数・二次方程式・微分積分など）。本文のコードの完成形 |

## ライセンス

[LICENSE](LICENSE) を参照してください。
