(* 定数の体: 2次方程式の章でつくった体（√pとiを含む）。微分はすべて0 *)
include Quadratic                   (* 2次方程式の解が閉じる体の演算 *)
type constant = Quadratic.t         (* 定数の体は自分自身 *)
let of_constant (c: constant): t = c

(* 微分 *)
let differentiate (_: t): t = zero
