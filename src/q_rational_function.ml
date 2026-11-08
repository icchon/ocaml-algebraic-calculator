(* 有理数係数の有理関数体 Q(x): 多項式環 Q[x] の分数体 *)
module P = struct
  include Q_poly
  (* 全順序 *)
  let rec compare (x1: t) (x2: t): int =
    match x1, x2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | c1 :: r1, c2 :: r2 ->
      let c = Rational.compare c1 c2 in
      if c <> 0 then c else compare r1 r2
end

include Fraction.Make(P)

(* 有理数係数の多項式を有理関数とみなす *)
let of_poly (p: P.t): t = (P.normalize p, P.one)

let x: t = of_poly [Rational.zero; Rational.one]
