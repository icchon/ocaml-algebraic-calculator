(* 1階の線形微分方程式 y' + a y = g を、有理関数体 K(x) の中で解く。
   解の分母を決めてから、分子の多項式を最高次の係数から順に決める *)

(* 1階: y' + a y = g の有理関数解。aは0でない多項式に限る。
   K(x)に解がなければNone *)
let solve_first_order (a: Rational_function.t) (g: Rational_function.t)
  : Rational_function.t option =
  Rational_integral.solve_rde a g
