(* 有理数係数の有理関数体 Q(x): 多項式環 Q[x] の分数体 *)
include Fraction.Make(Q_poly)

(* 有理数係数の多項式を有理関数とみなす *)
let of_poly (p: Q_poly.t): t = (Q_poly.normalize p, Q_poly.one)

let x: t = of_poly [Rational.zero; Rational.one]
