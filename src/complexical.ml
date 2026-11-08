module type FIELD = sig
  include Field.S
  val conj: t -> t
end

module type FUNCTOR_TYPE =
  functor (F: FIELD) -> (FIELD with type t = F.t * F.t)

module Make: FUNCTOR_TYPE = functor (F: FIELD) -> struct
  type t = F.t * F.t
  (* 正規化：実部と虚部をそれぞれ正規化する *)
  let normalize ((a, b): t): t =
    (F.normalize a, F.normalize b)
  include Normal_form.Extend(struct
    type nonrec t = t
    let normalize = normalize
  end)
  let zero: t = (F.zero, F.zero)
  let one: t = (F.one, F.zero)
  (* 加法 *)
  let add ((a1, b1): t) ((a2, b2): t): t =
    (F.add a1 a2, F.add b1 b2)
  (* 符号反転 *)
  let neg ((a, b): t): t = (F.neg a, F.neg b)
  (* 共役 *)
  let conj ((a, b): t): t = (F.conj a, F.neg b)
  (* 乗法 *)
  let mul ((a1, b1): t) ((a2, b2): t): t =
    (F.sub (F.mul a1 a2) (F.mul (F.conj b2) b1),
    F.add (F.mul b2 a1) (F.mul b1 (F.conj a2)))

  (* ノルムの2乗 *)
  let norm2 (x: t): F.t = mul x (conj x) |> fst
  (* 逆元 *)
  let inv (x: t): t =
    let norm = norm2 x in
    let (a, b) = conj x in
    (F.div a norm, F.div b norm)
  include Add_group.Extend(struct
    type nonrec t = t
    let add, neg = add, neg
  end)
  include Mul_group.Extend(struct
    type nonrec t = t
    let one, mul, inv = one, mul, inv
  end)
end
