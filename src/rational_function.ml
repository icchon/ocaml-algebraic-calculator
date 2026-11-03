(* 有理関数体 K(x): 定数の体に、x' = 1 となる元xを添加する *)
include Tower.Variable(Constant)

let x: t = theta

(* 有理数係数の多項式を有理関数とみなす *)
let of_q_poly (p: Q_poly.t): t = of_poly (List.map Constant.of_rational p)
