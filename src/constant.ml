(* 定数の体: 2次方程式の章でつくった体（√pとiを含む）。微分はすべて0 *)
include Quadratic
type constant = Quadratic.t
let of_constant (c: constant): t = c

(* 微分 *)
let differentiate (_: t): t = zero
