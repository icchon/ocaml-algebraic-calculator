(* 微分体: 微分をそなえた体。どの微分体にも、微分すると0になる定数の体が
   埋め込まれている *)
module type S = sig
  include Field.S                (* 体の演算 *)
  type constant                  (* 定数の体の型 *)
  val differentiate: t -> t      (* 微分 *)
  val of_constant: constant -> t (* 定数をこの体の元にする *)
end
