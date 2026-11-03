(* 本書の代数の値を、式の形に変換するためのライブラリ。本文には出てこない。
   対話環境で、中身が見えない型（<abstr> と表示される型）の答えを読める形にするために使う。 *)
(* 表示用の式ツリー。個々のモジュールの to_string が自分でかっこや係数の
   省略を判断する代わりに、一度この型に変換してから、ここでまとめて
   文字列に組み立てる *)
type t =
  | Num of int
  | Atom of string
  | Neg of t
  | Sum of t list
  | Product of t list
  | Frac of t * t
  | Pow of t * t
  | Fn of string * t list

(* 整数リテラル。負の数はここで Neg に正規化しておくことで、
   以降の処理は「Num は 0 以上」という前提だけを見ればよくなる *)
let num (n: int): t =
  if n = 0 then Num 0
  else if n < 0 then Neg (Num (-n))
  else Num n

(* 変数名や記号など、すでに完成した文字列をそのまま葉として使う。
   注意: \omega のように、後ろに区切り（}や空白）を持たないLaTeXの
   マクロ名をそのまま渡すと、隣の記号と詰めて表示したときに
   \omegav のように1つの（存在しない）コマンド名として解釈されてしまう。
   \sqrt{2} のように必ず } で終わる形にするか、{\omega} のように
   自分自身をブレースで閉じてから渡すこと *)
let atom (s: string): t = Atom s

let neg (x: t): t =
  match x with
  | Neg y -> y
  | Num 0 -> Num 0
  | y -> Neg y

(* 和をとる。0 を取り除き、入れ子になった Sum を1段にまとめる *)
let sum (xs: t list): t =
  let flat = List.concat_map (function Sum ys -> ys | x -> [x]) xs in
  let flat = List.filter (function Num 0 -> false | _ -> true) flat in
  match flat with
  | [] -> Num 0
  | [x] -> x
  | xs -> Sum xs

(* 積をとる。0があれば全体を0に、1は取り除き、負号は総ざらいして
   偶奇でひとつの Neg にまとめる。これにより「係数が1やマイナス1の
   ときだけ表示を省略する」という条件分岐を、この関数の外に一切
   書かずに済ませられる *)
let product (xs: t list): t =
  let flat = List.concat_map (function Product ys -> ys | x -> [x]) xs in
  if List.exists (function Num 0 -> true | _ -> false) flat then Num 0
  else
    let neg_count = ref 0 in
    let stripped =
      flat
      |> List.map (function
           | Neg y -> incr neg_count; y
           | y -> y)
      |> List.filter (function Num 1 -> false | _ -> true)
    in
    let magnitude =
      match stripped with
      | [] -> Num 1
      | [x] -> x
      | xs -> Product xs
    in
    if !neg_count mod 2 = 1 then Neg magnitude else magnitude

(* product と同じく、分子・分母どちらかの符号は外に出す。
   これにより neg した値を frac に渡しても、frac を neg した値と
   同じ見た目 (-\frac{...}{...}) にそろう。符号を外に出した結果、
   分母がちょうど1になったら（例: 1/(-1) → -(1/1)）、\frac{}{}を使わず
   分子だけを返す *)
let frac (n: t) (d: t): t =
  let make (n': t) (d': t): t =
    match d' with
    | Num 1 -> n'
    | _ -> Frac (n', d')
  in
  match n, d with
  | Num 0, _ -> Num 0
  | Neg n', Neg d' -> make n' d'
  | Neg n', d' -> Neg (make n' d')
  | n', Neg d' -> Neg (make n' d')
  | n', d' -> make n' d'
let pow (base: t) (exponent: t): t = Pow (base, exponent)

(* \log(x - a) のような、名前付き関数の適用。名前はLaTeXのマクロ名として
   そのまま使う（例: fn "log" [x] は \log(x) になる）。引数の並びが
   \left(...\right) 自体で区切られるので、Frac/Powと同じく自己完結した
   結合の強さを持つ *)
let fn (name: string) (args: t list): t = Fn (name, args)

(* 各構成要素の「結合の強さ」。数値が大きいほど、かっこなしで
   親の中に置ける範囲が広い *)
let rec render ?(min_prec = 0) (e: t): string =
  let wrap (prec: int) (s: string): string =
    if prec < min_prec then "\\left(" ^ s ^ "\\right)" else s
  in
  match e with
  | Num n -> wrap 3 (string_of_int n)
  | Atom s -> wrap 3 s
  | Frac (n, d) ->
    wrap 3 (Printf.sprintf "\\frac{%s}{%s}" (render n) (render d))
  | Pow (base, exponent) ->
    (* 指数側は {} 自体が区切りになるので、かっこは底側にだけ必要。
       Frac は積の中では \frac{}{} 自体が区切りになるので他の因子と
       並べてもかっこ不要だが、指数の底になる場合だけは
       \frac{1}{2}^{2} のように見た目が紛らわしいので、Num/Atom以外は
       常にかっこを付ける（precのしくみとは別に、ここだけ特別扱いする） *)
    let base_str = match base with
      | Num _ | Atom _ -> render ~min_prec:3 base
      | _ -> "\\left(" ^ render base ^ "\\right)"
    in
    wrap 3 (Printf.sprintf "%s^{%s}" base_str (render exponent))
  | Fn (name, args) ->
    wrap 3 (Printf.sprintf "\\%s\\left(%s\\right)" name (args |> List.map render |> String.concat ", "))
  | Neg x ->
    (* Sum/Product の中では、通常この分岐を経由せず後述の符号処理で
       "-" が直接組み立てられる。ここは単独で Neg が現れた場合のみ通る *)
    wrap 1 ("-" ^ render ~min_prec:1 x)
  | Product fs -> wrap 2 (render_product fs)
  | Sum ts -> wrap 0 (render_sum ts)

and render_product (fs: t list): string =
  match fs with
  | [] -> "1"
  | [x] -> render ~min_prec:2 x
  | x :: (y :: _ as rest) ->
    (* Frac や Pow は \frac{}{} や ^{} 自体がすでに区切りになっているので、
       裸の数字どうしが隣り合うとき（"2 3" のように読み違えかねないとき）
       だけ空白を入れ、それ以外（係数と単項、記号どうしなど）は詰めて書く *)
    let needs_space =
      match x, y with
      | Num _, Num _ -> true
      | _ -> false
    in
    render ~min_prec:2 x ^ (if needs_space then " " else "") ^ render_product rest

and render_sum (ts: t list): string =
  match ts with
  | [] -> "0"
  | first :: rest ->
    let first_str =
      match first with
      | Neg x -> "-" ^ render ~min_prec:1 x
      | x -> render x
    in
    let rest_str =
      rest
      |> List.map (function
           | Neg x -> " - " ^ render ~min_prec:2 x
           | x -> " + " ^ render x)
      |> String.concat ""
    in
    first_str ^ rest_str

let to_string (e: t): string = render e

(* Expr.t への変換さえ用意すれば、どんな型でも to_string できるようにする。
   ファーストクラスモジュールとして (module M) の形で渡す *)
type expr = t

module type CONVERTIBLE = sig
  type t
  val to_expr: t -> expr
end

let to_string_of (type a) (module M: CONVERTIBLE with type t = a) (x: a): string =
  to_string (M.to_expr x)

(* Complexical.Make(F).t = F.t * F.t の形をした型なら何にでも使える変換。
   Complexical.Make自身が「Fが何であっても同じファンクタを適用できる」のと
   同じように、こちらも Inner の CONVERTIBLE さえあれば、四元数・八元数の
   ように何段重ねても変換を作れる *)
module Make_paired_expr (Inner: CONVERTIBLE) (Sym: sig val symbol: string end):
  CONVERTIBLE with type t = Inner.t * Inner.t = struct
  type t = Inner.t * Inner.t
  let to_expr ((re, im): t): expr =
    sum [Inner.to_expr re; product [Inner.to_expr im; atom Sym.symbol]]
end

(* Polynomial.Make(C).t = C.t list（あるいはField_extension.Make(I).t = I.P.t、
   同じくcoeff listの形）に使える変換。多項式にも拡大体の元にも、どちらにも
   同じ形で使い回せる。変数は \log(x) や e^{x} のような式でもよい *)
module Make_poly_expr_of (Coeff: CONVERTIBLE) (Var: sig val var: expr end):
  CONVERTIBLE with type t = Coeff.t list = struct
  type t = Coeff.t list
  let var_power (i: int): expr =
    if i = 0 then num 1
    else if i = 1 then Var.var
    else pow Var.var (num i)
  let to_expr (p: t): expr =
    p
    |> List.mapi (fun i c -> product [Coeff.to_expr c; var_power i])
    |> List.rev
    |> sum
end

(* 変数が名前（x, \sqrt{2} など）のときの省略形 *)
module Make_poly_expr (Coeff: CONVERTIBLE) (Var: sig val name: string end):
  CONVERTIBLE with type t = Coeff.t list =
  Make_poly_expr_of (Coeff) (struct let var = atom Var.name end)

(* ============================================================
   ここから先は、本書の各代数を実際にExpr.tへ変換するための
   コンバータ本体。計算側のモジュール（rational.ml, complexical.ml,
   polynomial.ml, quadratic.ml, q_rootp.ml, field_extension.ml,
   tower.mlなど）からはto_string/term_to_stringを完全に取り除き、
   文字列化に関するコードはすべてここに集約する ============= *)

(* 以下の変換モジュールは、対応する計算モジュールと同じ名前
   （Expr.Integer, Expr.Rationalなど）で定義する。そのため、ある代数の
   変換モジュール本体が、別の代数の「計算モジュール本体」を素の名前で
   参照する箇所では、その名前が後続の同名の変換モジュール定義で
   上書きされてしまう前に、ここでエイリアスしておく必要がある *)
module Integer_real = Integer

module Integer = struct
  type t = Integer_real.t
  let to_expr (n: t): expr = num n
end

module Rational = struct
  type t = Rational.t
  let to_expr ((n, d): t): expr =
    if Integer_real.equal d Integer_real.one then num n
    else frac (num n) (num d)
end

module Complex = Make_paired_expr (Rational) (struct let symbol = "i" end)

(* 四元数・八元数・十六元数は、Complexical.Makeの入れ子構造をそのまま
   Sum/Productで表すこともできる（Make_paired_exprを重ねればよい）が、
   本書では伝統的な i, j, k / e_1, ..., e_15 という基底名に展開した
   表示を採用しているため、その形をここで再現する *)
module Quaternion = struct
  type t = Quaternion.t
  let to_expr ((a, b): t): expr =
    let (r0, r1) = a in
    let (r2, r3) = b in
    sum [
      Rational.to_expr r0;
      product [Rational.to_expr r1; atom "i"];
      product [Rational.to_expr r2; atom "j"];
      product [Rational.to_expr r3; atom "k"];
    ]
end

module Octonion = struct
  type t = Octonion.t
  let to_expr ((a, b): t): expr =
    let ((r0, r1), (r2, r3)) = a in
    let ((r4, r5), (r6, r7)) = b in
    [ r0, ""; r1, "e_1"; r2, "e_2"; r3, "e_3"; r4, "e_4"; r5, "e_5"; r6, "e_6"; r7, "e_7" ]
    |> List.map (fun (c, label) ->
         if label = "" then Rational.to_expr c
         else product [Rational.to_expr c; atom label])
    |> sum
end

module Hexadecanion = struct
  type t = Hexadecanion.t
  let to_expr ((a, b): t): expr =
    let (((r0, r1), (r2, r3)), ((r4, r5), (r6, r7))) = a in
    let (((r8, r9), (r10, r11)), ((r12, r13), (r14, r15))) = b in
    [ r0, ""; r1, "e_1"; r2, "e_2"; r3, "e_3"; r4, "e_4"; r5, "e_5"; r6, "e_6"; r7, "e_7";
      r8, "e_8"; r9, "e_9"; r10, "e_{10}"; r11, "e_{11}"; r12, "e_{12}"; r13, "e_{13}";
      r14, "e_{14}"; r15, "e_{15}" ]
    |> List.map (fun (c, label) ->
         if label = "" then Rational.to_expr c
         else product [Rational.to_expr c; atom label])
    |> sum
end

module Q_poly = Make_poly_expr (Rational) (struct let name = "x" end)

(* Root2_field.K・Root5_fieldなど、Field_extension.Makeの出力はどれも
   coeff listという同じ形（t = I.P.t = C.t list）なので、Make_poly_exprを
   基底元の名前だけ変えて使い回せる。Root2_field.K専用にこの名前で
   用意しておく *)
module Root2_field = Make_poly_expr (Rational) (struct let name = "\\sqrt{2}" end)

(* Q_rootp・Quadratic・Quadratic_poly・Omega_field・Cubicは、
   内部がMap.Make/Set.Makeの出力という抽象型なので、Integer・Rationalの
   ように「実体と構造的に同じ形をそのまま書く」ことができない。かわりに、
   必要な操作だけを要求するファンクタ（サンプルコードリポジトリの
   lib/expr_q_rootp.ml等と同じ設計）にしてあり、呼び出し側はそのときどきの
   実体のモジュールをその場で渡してインスタンス化する
   （例: Expr.Cubic.Make (Q_rootp)） *)
module Q_rootp = struct
  module type Q_ROOTP_LIKE = sig
    type t
    module PrimeSet: sig
      type t
      val is_empty: t -> bool
      val elements: t -> int list
    end
    module BasisMap: sig
      val bindings: t -> (PrimeSet.t * (int * int)) list
    end
  end

  module Make (Q: Q_ROOTP_LIKE) = struct
    type t = Q.t
    let sqrt_term (s: Q.PrimeSet.t): expr option =
      if Q.PrimeSet.is_empty s then None
      else
        Q.PrimeSet.elements s
        |> List.map (fun p -> atom (Printf.sprintf "\\sqrt{%s}" (to_string (Integer.to_expr p))))
        |> product
        |> Option.some
    let to_expr (x: t): expr =
      Q.BasisMap.bindings x
      |> List.map (fun (s, c) ->
           match sqrt_term s with
           | None -> Rational.to_expr c
           | Some sq -> product [Rational.to_expr c; sq])
      |> sum
  end
end

(* Quadratic = 複素構造ファンクタ（Complexical.Make）をQ_rootpに適用した
   ものなので、Complexと同じMake_paired_exprがそのまま使い回せる *)
module Quadratic = struct
  module Make (Q: Q_rootp.Q_ROOTP_LIKE) =
    Make_paired_expr (Q_rootp.Make (Q)) (struct let symbol = "i" end)
end

module Quadratic_poly = struct
  module Make (Q: Q_rootp.Q_ROOTP_LIKE) =
    Make_poly_expr (Quadratic.Make (Q)) (struct let name = "x" end)
end

(* 関数 apply 自体は "自分が+である" "自分がneg である" ことを知らない
   （OCamlの関数値は記号や名前を持たない）ので、記号・呼び方とセットにして
   渡してもらう。apply はこのモジュールの外（呼び出し側）ですでに評価して
   おくのではなく、ここで実行することで、引数と結果を同じ変換 M でまとめて
   文字列化する。

   単項演算には「前置記号（-x）」と「関数呼び出し（\mathrm{name}(x)）」の
   2通りの見せ方があり、二項演算には「中置記号（x1 + x2）」と「関数呼び出し
   （\mathrm{name}(x1, x2)）」の2通りの見せ方がある。この4通りをそれぞれ
   独立なバリアントにしておくことで、「二項演算なのに前置記号を指定してしまう」
   といった取り違えが型の時点で起こらないようにしている *)
(* \mathrm{}の中でも、アンダースコアはLaTeXの数式モードでは添字の
   区切りとして特別扱いされてしまう。div_remのような、アンダースコアを
   含む関数名をテストコード側が気にしなくて済むよう、\mathrm{}へ渡す前に
   ここでエスケープしておく *)
let escape_name (name: string): string =
  String.concat "\\_" (String.split_on_char '_' name)

type unop_style = UPrefix | UCall | UPow of int | UPostfix
type biop_style = BInfix | BCall

type 'a named_unop = {
  u_style: unop_style;
  u_symbol: string;  (* UPrefixなら記号そのもの、UCallなら関数名 *)
  u_apply: 'a -> 'a;
}

type 'a named_biop = {
  b_style: biop_style;
  b_symbol: string;  (* BInfixなら記号そのもの、BCallなら関数名 *)
  b_apply: 'a -> 'a -> 'a;
}

let prefix_unop (symbol: string) (apply: 'a -> 'a): 'a named_unop =
  { u_style = UPrefix; u_symbol = symbol; u_apply = apply }

let call_unop (name: string) (apply: 'a -> 'a): 'a named_unop =
  { u_style = UCall; u_symbol = name; u_apply = apply }

(* x^n のような累乗表示。n=-1を渡せば逆元 x^{-1} の表示にもそのまま使える *)
let pow_unop (n: int) (apply: 'a -> 'a): 'a named_unop =
  { u_style = UPow n; u_symbol = ""; u_apply = apply }

(* x' のような後置記号。微分をprime記法で表示するのに使う *)
let postfix_unop (symbol: string) (apply: 'a -> 'a): 'a named_unop =
  { u_style = UPostfix; u_symbol = symbol; u_apply = apply }

let infix_biop (symbol: string) (apply: 'a -> 'a -> 'a): 'a named_biop =
  { b_style = BInfix; b_symbol = symbol; b_apply = apply }

let call_biop (name: string) (apply: 'a -> 'a -> 'a): 'a named_biop =
  { b_style = BCall; b_symbol = name; b_apply = apply }

(* 乗法を中置の \cdot で見せたいときのための、よく使う組み合わせ *)
let cdot_biop (apply: 'a -> 'a -> 'a): 'a named_biop =
  infix_biop "\\cdot" apply

(* 「(e_1 \cdot e_2) \cdot e_4」のような、実際の値ではなく記号どうしの
   積を見せたいときに使う。\cdot という記法自体はここに閉じ込めておき、
   呼び出し側は記号の並びだけを渡す *)
let cdot_join (labels: string list): string =
  String.concat " \\cdot " labels

let unop_to_string (type a)
    (module M: CONVERTIBLE with type t = a)
    (op: a named_unop)
    (x: a): string =
  let res_str = to_string_of (module M) (op.u_apply x) in
  let lhs = match op.u_style with
    (* xがSum（複数項）だと "-1 - i" のように符号が分配されたかのように
       誤読されるので、render自体のNegケースと同じ強さ(min_prec:1)で
       かっこを要求する。to_string_of(素のrender)をそのまま使わないのは
       このためで、Call側は関数呼び出しの()自体が区切りになるので不要 *)
    | UPrefix -> op.u_symbol ^ render ~min_prec:1 (M.to_expr x)
    | UCall -> Printf.sprintf "\\mathrm{%s}\\left(%s\\right)" (escape_name op.u_symbol) (to_string_of (module M) x)
    | UPow n -> to_string (pow (M.to_expr x) (num n))
    (* x' のように後ろに記号を付けるだけなので、Powの底と同じ理由
       （Num/Atom以外はかっこが要る）でかっこの要否を揃える *)
    | UPostfix ->
      let e = M.to_expr x in
      let arg_str = match e with
        | Num _ | Atom _ -> render e
        | _ -> "\\left(" ^ render e ^ "\\right)"
      in
      arg_str ^ op.u_symbol
  in
  Printf.sprintf "%s = %s" lhs res_str

let biop_to_string (type a)
    (module M: CONVERTIBLE with type t = a)
    (op: a named_biop)
    (x1: a) (x2: a): string =
  let res_str = to_string_of (module M) (op.b_apply x1 x2) in
  let lhs = match op.b_style with
    (* x1やx2がSumだと "1 - i + 2 + 3i" のように、記号(+/-/*など)の
       結合順序が本来と異なって読めてしまう。symbolが何であっても安全な
       よう、Productの結合の強さ(min_prec:2)を要求してかっこを付ける。
       これは "+" のときは過剰にかっこが付くこともあるが、安全側に倒す *)
    | BInfix -> Printf.sprintf "%s %s %s" (render ~min_prec:2 (M.to_expr x1)) op.b_symbol (render ~min_prec:2 (M.to_expr x2))
    | BCall -> Printf.sprintf "\\mathrm{%s}\\left(%s, %s\\right)" (escape_name op.b_symbol) (to_string_of (module M) x1) (to_string_of (module M) x2)
  in
  Printf.sprintf "%s = %s" lhs res_str

(* 旧名。二項演算はすべて中置で書きたいだけの場合はこちらでもよい *)
let equation_to_string (type a)
    (module M: CONVERTIBLE with type t = a)
    (op: a named_biop)
    (x1: a) (x2: a): string =
  biop_to_string (module M) op x1 x2

(* 方程式の求解: 問題（例えば係数の列）の型 a と、解の型 b は別々でよい。
   例えば「多項式 = 0」を解いて「有理数のリスト」を得る場合、多項式と
   有理数は別の型なので、変換モジュールも別々に受け取る *)
let solve_to_string (type a) (type b)
    (module Eq: CONVERTIBLE with type t = a)
    (module Sol: CONVERTIBLE with type t = b)
    ?(var = "x")
    (solve: a -> b list)
    (problem: a): string =
  let lhs = to_string_of (module Eq) problem in
  let solutions = solve problem in
  let rhs = match solutions with
    | [] -> "\\text{解なし}"
    | xs -> xs |> List.map (to_string_of (module Sol)) |> String.concat ",\\ "
  in
  Printf.sprintf "%s = 0 \\quad\\Rightarrow\\quad %s = %s" lhs var rhs

(* 多項式のevalだけに特化した表示。eval(p, a) = ... という汎用の関数呼び出し
   の形（callやbiop_to_stringを流用すると多項式全体が引数欄に書かれてしまい
   読みにくい）ではなく、まず p(x) = ... で多項式そのものを示し、続けて
   p(a) = ... で代入結果だけを表示する。多項式の型 a と、評価点・結果の型 c は
   （係数体そのものが評価点になるとは限らないので）別々に受け取る *)
let poly_eval_to_string (type a) (type c)
    (module P: CONVERTIBLE with type t = a)
    (module C: CONVERTIBLE with type t = c)
    ?(name = "p")
    ?(var = "x")
    (eval: a -> c -> c)
    (poly: a) (point: c): string =
  let definition = Printf.sprintf "%s(%s) = %s" name var (to_string_of (module P) poly) in
  let applied = Printf.sprintf "%s(%s) = %s" name (to_string_of (module C) point) (to_string_of (module C) (eval poly point)) in
  Printf.sprintf "%s \\quad\\Rightarrow\\quad %s" definition applied

(* \int f\,dx = F のような積分の表示。fは被積分関数（呼び出し側で
   組み立てたExpr.t）、varは積分変数名 *)
let integral_to_string (var: string) (integrand: t) (result: string): string =
  Printf.sprintf "\\int %s\\,d%s = %s" (to_string integrand) var result

(* ここから先は、TeXの断片（\[ \]や\documentclassなど）をテストコード側に
   一切書かせないための、表示専用のヘルパー。テストコードは、値と演算を
   Exprに渡すだけで、その結果を画面にどう出すかは考えなくてよい *)

(* 1行を \[ ... \] で囲んで表示する *)
let put (s: string): unit =
  print_endline ("\\[" ^ s ^ "\\]")

(* 複数のExpr文字列を texequation の \quad で並べて1行にする *)
let join (strs: string list): string =
  String.concat " \\quad " strs

(* named_unop/named_biopは「入出力の型が同じ」場合しか使えない。
   div_remのようにタプルを返す関数や、evalのように引数と返り値の型が
   異なる関数は、この形に当てはまらないので、あらかじめ文字列に変換した
   引数・結果を渡すだけの、もっと素朴な関数呼び出し表示を用意しておく *)
let call (name: string) (args: string list) (result: string): string =
  Printf.sprintf "\\mathrm{%s}\\left(%s\\right) = %s" (escape_name name) (String.concat ", " args) result

(* div_remのように、複数のExpr文字列を組として1つの結果にまとめたいときに使う *)
let tuple (strs: string list): string =
  "\\left(" ^ String.concat ",\\ " strs ^ "\\right)"

(* ここから先は微分・積分の章で追加 *)

(* 微分体の塔 K(θ) の元。K(θ)はK[θ]の分数体なので、θの多項式の分数として
   表示する。θの見せ方（x、\log(x - 1)、e^{2x} など）は呼び出し側が渡す *)
module Make_tower_expr
    (L: Tower.S) (K: CONVERTIBLE with type t = L.K.t) (Theta: sig val var: expr end):
  CONVERTIBLE with type t = L.t = struct
  module P = Make_poly_expr_of (K) (Theta)
  type t = L.t
  let to_expr ((n, d): t): expr =
    if L.P.equal d L.P.one then P.to_expr n else frac (P.to_expr n) (P.to_expr d)
end

(* θ = log u と θ = e^u の見せ方 *)
let log_of (u: expr): expr = fn "log" [u]
let exp_of (u: expr): expr = pow (atom "e") u

(* 初等関数で書ける積分の答え v + Σ c_i log w_i *)
module Make_liouville_expr
    (A: Liouville.S)
    (C: CONVERTIBLE with type t = A.constant)
    (F: CONVERTIBLE with type t = A.field):
  CONVERTIBLE with type t = A.t = struct
  type t = A.t
  let to_expr (a: t): expr =
    sum (F.to_expr a.v
         :: List.map (fun (c, w) -> product [C.to_expr c; log_of (F.to_expr w)]) a.logs)
end

(* 積分の答え。初等関数で書けないと判定できたときは、そう書く *)
let result_to_expr (f: 'a -> expr) (r: 'a Integral.result): expr =
  match r with
  | Integral.Elementary a -> f a
  | Integral.Not_elementary -> atom "\\text{初等関数では書けない}"
