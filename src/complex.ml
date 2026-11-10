include Complexical.Make(Rational)

(* 共役 *)
let conj ((a, b): t): t = (a, Rational.neg b)
