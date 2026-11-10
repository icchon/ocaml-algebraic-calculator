(* 共役をもつ体 *)
module type FIELD = sig
  include Field.S
  val conj: t -> t
end

module type FUNCTOR_TYPE =
  functor (F: FIELD) -> (FIELD with type t = F.t * F.t)

(* 複素構造ファンクタの共役・乗法・逆元を、ケーリーディクソンの構成法の
   式に置き換える。除法と累乗は、置き換えた乗法と逆元からつくり直す *)
module Make: FUNCTOR_TYPE = functor (F: FIELD) -> struct
  include Complexical.Make(F)
  (* 共役 *)
  let conj ((a, b): t): t = (F.conj a, F.neg b)
  (* 乗法 *)
  let mul ((a1, b1): t) ((a2, b2): t): t =
    (F.sub (F.mul a1 a2) (F.mul (F.conj b2) b1),
    F.add (F.mul b2 a1) (F.mul b1 (F.conj a2)))
  (* 逆元 *)
  let inv (x: t): t =
    let norm = mul x (conj x) |> fst in
    let (a, b) = conj x in
    (F.div a norm, F.div b norm)
  include Mul_group.Extend(struct
    type nonrec t = t
    let one, mul, inv = one, mul, inv
  end)
end
