(* 線形微分方程式。
   1階 y' + a y = g は、解の分母を決めてから分子の多項式を求めて解く。
   定数係数の同次方程式 a_n y^(n) + ... + a_1 y' + a_0 y = 0 は、特性方程式
   a_n r^n + ... + a_0 = 0 の根rごとに、解 x^j e^(rx) をもつ。根は定数の体の中に
   求まらなければならないので、特性多項式が有理数の上で2次以下の因数に
   分解できる場合に限る *)

(* 1階: y' + a y = g の有理関数解。aは0でない多項式に限る。
   K(x)に解がなければNone *)
let solve_first_order (a: Rational_function.t) (g: Rational_function.t)
  : Rational_function.t option =
  Rational_integral.solve_rde a g

(* 有理関数体に e^(rx) を添加した微分体の元 c0 + c1 e^(rx) + c2 (e^(rx))^2 + ... を、
   係数のリストで表す。rごとに別の微分体になる *)
type t = { rate: Constant.t; coeffs: Rational_function.t list }

(* 特性多項式の根と多重度 *)
let roots (p: Q_poly.t): (Constant.t * int) list =
  match Q_poly_roots.roots p with
  | Some rs -> rs
  | None ->
    failwith "Linear_ode.solve: the characteristic polynomial has an irreducible factor of degree >= 3"

(* 基本解。根rの多重度がmなら、x^j e^(rx)（j = 0, ..., m - 1）。
   r = 0 なら e^(rx) = 1 なので、x^j そのもの *)
let solve (a: Rational.t list): t list =
  List.concat_map (fun (r, m) ->
    List.init m (fun j ->
      let xj = Rational_function.pow j Rational_function.x in
      if Constant.equal r Constant.zero then { rate = r; coeffs = [xj] }
      else { rate = r; coeffs = [Rational_function.zero; xj] }))
    (roots a)

(* 微分体Fで a_n y^(n) + ... + a_0 y を計算する *)
module Apply (F: Differential.S with type constant = Constant.t) = struct
  let apply (a: Rational.t list) (y: F.t): F.t =
    let rec go (a: Rational.t list) (y: F.t) (acc: F.t): F.t =
      match a with
      | [] -> acc
      | ai :: rest ->
        let term = F.mul (F.of_constant (Constant.of_rational ai)) y in
        go rest (F.differentiate y) (F.add acc term)
    in
    go a y F.zero
end

(* 検算: 解yを、e^(rx)を添加した微分体の中で微分して、方程式の左辺に代入する *)
let check (a: Rational.t list) (y: t): t =
  let module E = Tower.Exp (struct
      module K = Rational_function
      let argument = Rational_function.of_poly [Constant.zero; y.rate]
    end) in
  let module A = Apply (E) in
  { y with coeffs = E.to_poly (A.apply a (E.of_poly y.coeffs)) }
