include Complexical.Make(Q_sqrt)

(* 大小比較（Fraction.Makeに渡すための便宜的な全順序） *)
let compare ((a1, b1): t) ((a2, b2): t): int =
  let c = Q_sqrt.compare a1 a2 in
  if c <> 0 then c else Q_sqrt.compare b1 b2

(* 有理数の埋め込み *)
let of_rational (q : Rational.t) : t =
  (Q_sqrt.of_rational q, Q_sqrt.zero)

(* 平方根（q < 0なら虚部に、q >= 0なら実部にQ_sqrtの平方根を埋め込む） *)
let of_rational_sqrt (q : Rational.t) : t =
  if Rational.compare q Rational.zero >= 0 then
    (Q_sqrt.of_rational_sqrt q, Q_sqrt.zero)
  else
    (Q_sqrt.zero, Q_sqrt.of_rational_sqrt (Rational.neg q))

(* 2次方程式の求解 *)
let solve (a : Rational.t) (b : Rational.t) (c : Rational.t) : t * t =
  let disc =
    Rational.sub (Rational.mul b b)
      (Rational.mul (Rational.of_integer 4) (Rational.mul a c))
  in
  let sqrt_disc = of_rational_sqrt disc in
  let neg_b = of_rational (Rational.neg b) in
  let two_a = of_rational (Rational.mul (Rational.of_integer 2) a) in
  (div (add neg_b sqrt_disc) two_a, div (sub neg_b sqrt_disc) two_a)
