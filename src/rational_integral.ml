(* 有理関数体 K(x) での積分（部分分数分解）と、リッシュの微分方程式 *)
module F = Rational_function
module A = Liouville.Make(F)
module P = F.P

(* 整数を定数として埋め込む *)
let const (n: int): Constant.t = Constant.of_rational (Rational.of_integer n)

(* x - a を表す多項式 *)
let linear (a: Constant.t): P.t = [Constant.neg a; Constant.one]

(* i次の係数と、c x^i を表す多項式 *)
let coeff (p: P.t) (i: int): Constant.t =
  Option.value (List.nth_opt p i) ~default:Constant.zero
let monomial (c: Constant.t) (i: int): P.t = List.init i (fun _ -> Constant.zero) @ [c]

(* 多項式部分の積分 *)
let integrate_poly (s: P.t): P.t =
  Constant.zero :: List.mapi (fun i c -> Constant.div c (const (i + 1))) s

(* 有理数への変換（分母の因数分解は有理数係数で行う） *)
let to_rational ((re, im): Constant.t): Rational.t =
  match Q_sqrt.to_rational re, Q_sqrt.equal im Q_sqrt.zero with
  | Some q, true -> q
  | _ -> failwith "Rational_integral.integrate: denominator has a non-rational coefficient"

(* 分母qの根と多重度。qが有理数の上で2次以下の因数に分解できる場合に限る *)
let roots_of (q: P.t): (Constant.t * int) list =
  match Q_poly_roots.roots (List.map to_rational (P.normalize q)) with
  | Some rs -> rs
  | None ->
    failwith "Rational_integral.integrate: the denominator has an irreducible factor of degree >= 3"

(* 多重度mの根aの寄与。q = (x-a)^m h と分解すると、部分分数分解の
   c_k/(x-a)^k（k = 1, ..., m）の係数は、g = r/h のテイラー展開から
   c_(m-j) = g^(j)(a)/j! と決まる。c_1/(x-a) の積分は c_1 log(x-a)、
   k >= 2 の c_k/(x-a)^k の積分は -c_k/((k-1)(x-a)^(k-1)) *)
let root_term (r: P.t) (q: P.t) (a: Constant.t) (m: int): A.t =
  let lin = linear a in
  let h = fst (P.div_rem q (P.pow m lin)) in
  let at ((n, d): F.t): Constant.t = Constant.div (P.eval n a) (P.eval d a) in
  let term (k: int) (c: Constant.t): A.t =
    if k = 1 then A.log c (F.of_poly lin)
    else
      A.of_field (F.div (F.of_base (Constant.div (Constant.neg c) (const (k - 1))))
                    (F.of_poly (P.pow (k - 1) lin)))
  in
  (* g^(j)/j! を順に求め、k = m - j の項を足していく *)
  let rec go (j: int) (g: F.t) (fact: int) (acc: A.t): A.t =
    if j = m then acc
    else
      let c = Constant.div (at g) (const fact) in
      go (j + 1) (F.differentiate g) (fact * (j + 1)) (A.add acc (term (m - j) c))
  in
  go 0 (F.div (F.of_poly r) (F.of_poly h)) 1 A.zero

(* 有理関数 p/q の積分。有理関数はいつも初等関数で積分できる *)
let integrate ((p, q): F.t): A.t Integral.result =
  let (s, r) = P.div_rem p q in
  Integral.Elementary
    (List.fold_left (fun acc (a, m) -> A.add acc (root_term r q a m))
       (A.of_field (F.of_poly (integrate_poly s))) (roots_of q))

(* y' + a y = g の有理関数解y。aは0でない多項式に限る（θ = e^u のuが多項式のとき）。
   g = n/b のとき、yの分母は gcd(b, b') で割り切れる形に決まるので、
   y = z/d（d = gcd(b, b')）とおくと、多項式zについての方程式
   d z' + (a d - d') z = g d^2 になる。zは最高次の係数から順に決まる *)
let solve_rde (a: F.t) (g: F.t): F.t option =
  if P.degree (snd a) > 0 then failwith "Rational_integral.solve_rde: u' must be a polynomial";
  let a = fst a in
  let d = P.gcd (snd g) (P.differentiate (snd g)) in
  let ad = P.mul a d in
  let lhs (z: P.t): P.t =
    P.add (P.mul d (P.differentiate z)) (P.mul (P.sub ad (P.differentiate d)) z) in
  let (c, c_den) = F.mul g (F.of_poly (P.mul d d)) in
  (* 残りrの、zのk次の係数で決まる項を順に消していく。最後に残りが0でなければ解はない *)
  let rec back (r: P.t) (k: int) (z: P.t): P.t option =
    if k < 0 then (if P.equal r P.zero then Some z else None)
    else
      let term = monomial (Constant.div (coeff r (k + P.degree ad)) (P.leading_coeff ad)) k in
      back (P.sub r (lhs term)) (k - 1) (P.add z term)
  in
  if P.degree c_den > 0 then None (* 右辺が多項式でなければ解はない *)
  else
    back c (P.degree c - P.degree ad) P.zero
    |> Option.map (fun z -> F.div (F.of_poly z) (F.of_poly d))
