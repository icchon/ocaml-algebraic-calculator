(* 対話環境で答えを読める形にする。.ocamlinit が #install_printer で登録する。
   中身が見えない型（Q_rootp.t）だけを対象にする。
   有理数の (5, 6) のような組は、そのまま表示される。 *)
let rec text ?(p = 0) (e : Expr.t) : string =
  let wrap q s = if q < p then "(" ^ s ^ ")" else s in
  match e with
  | Expr.Num n -> string_of_int n
  | Expr.Atom s -> atom s
  | Expr.Frac (n, d) -> wrap 2 (text ~p:3 n ^ "/" ^ text ~p:3 d)
  | Expr.Pow (b, x) -> text ~p:3 b ^ "^" ^ text ~p:3 x
  | Expr.Fn (f, args) -> f ^ "(" ^ String.concat ", " (List.map text args) ^ ")"
  | Expr.Neg x -> wrap 1 ("-" ^ text ~p:2 x)
  | Expr.Product fs -> wrap 2 (String.concat " " (List.map (text ~p:3) fs))
  | Expr.Sum [] -> "0"
  | Expr.Sum (t :: ts) ->
    wrap 1 (String.concat "" (text ~p:1 t :: List.map (function
      | Expr.Neg x -> " - " ^ text ~p:2 x
      | x -> " + " ^ text ~p:1 x) ts))

(* \sqrt{2} → √2 *)
and atom (s : string) : string =
  let prefix = "\\sqrt{" in
  let n = String.length prefix in
  if String.length s > n && String.sub s 0 n = prefix then
    "√" ^ String.sub s n (String.length s - n - 1)
  else s

module Q_rootp_expr = Expr.Q_rootp.Make (Q_rootp)

let q_rootp fmt x = Format.pp_print_string fmt (text (Q_rootp_expr.to_expr x))

(* 定数の体の元（a + bi）と、有理関数体 K(x) の元。中身の組 (1, 0) のままだと、
   有理数の分子と分母（1/0）と見分けがつかないので、式の形で表示する *)
module Quadratic_expr = Expr.Quadratic.Make (Q_rootp)
module Rational_function_expr =
  Expr.Make_tower_expr (Rational_function) (Quadratic_expr) (struct let var = Expr.atom "x" end)

let constant fmt x = Format.pp_print_string fmt (text (Quadratic_expr.to_expr x))
let rational_function fmt x = Format.pp_print_string fmt (text (Rational_function_expr.to_expr x))
