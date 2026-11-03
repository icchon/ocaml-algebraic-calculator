(* 微分体: 微分をそなえた体。どの微分体にも、微分すると0になる定数の体が
   埋め込まれている *)
module type S = sig
  include Field.S
  type constant
  val compare: t -> t -> int (* 分数体ファンクタに渡すための便宜的な全順序 *)
  val differentiate: t -> t
  val of_constant: constant -> t
end
