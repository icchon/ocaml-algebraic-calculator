module type MIN = sig
  type t
  val one: t
  val mul: t -> t -> t
  val inv: t -> t
end

module Extend(M: MIN) = struct
  module Monoid = Mul_monoid.Extend(M)
  (* 除法 *)
  let div (x1: M.t) (x2: M.t): M.t =
    M.mul x1 (M.inv x2)
  (* 累乗：負の指数は、逆元の累乗 *)
  let pow (n: int) (x: M.t): M.t =
    if n >= 0 then Monoid.pow n x
    else Monoid.pow (-n) (M.inv x)
end

module type S = sig
  include Mul_monoid.S
  val inv: t -> t
  val div: t -> t -> t
end
