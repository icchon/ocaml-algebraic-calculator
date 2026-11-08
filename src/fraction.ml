
module type RING = sig
  include Ring.S
  val compare: t -> t -> int
  val div_rem: t -> t -> t * t
  val canonical: t -> t
  val gcd: t -> t -> t
end

module type FUNCTOR_TYPE = functor (R: RING) ->
  (sig
     include Field.S
     val compare: t -> t -> int
   end with type t = R.t * R.t)

module Make: FUNCTOR_TYPE = functor (R: RING) -> struct
  type t = R.t * R.t
  (* 約分し、分母をそろえる *)
  let normalize ((n, d): t): t =
    let g = R.gcd n d in
    let n' = R.div_rem n g |> fst in
    let d' = R.div_rem d g |> fst in
    (* 分母の符号・最高次の係数の分 *)
    let u = R.div_rem d' (R.canonical d') |> fst in
    (R.normalize (R.div_rem n' u |> fst), R.normalize (R.canonical d'))
  include Normal_form.Extend(struct
    type nonrec t = t
    let normalize = normalize
  end)
  let zero: t = (R.zero, R.one)
  let one: t = (R.one, R.one)
  (* 加法 *)
  let add ((n1, d1): t) ((n2, d2): t): t =
    let d = R.mul d1 d2 in
    let n = R.add (R.mul n1 d2) (R.mul n2 d1) in
    (n, d) |> normalize
  (* 符号反転 *)
  let neg ((n, d): t): t =
    (R.neg n, d) |> normalize
  (* 乗法 *)
  let mul ((n1, d1): t) ((n2, d2): t): t =
    let n = R.mul n1 n2 in
    let d = R.mul d1 d2 in
    (n, d) |> normalize
  (* 逆元 *)
  let inv ((n, d): t): t =
    if R.equal n R.zero then
      failwith "Division by zero"
    else
      (d, n) |> normalize
  (* 全順序 *)
  let compare ((n1, d1): t) ((n2, d2): t): int =
    R.compare (R.mul n1 d2) (R.mul n2 d1)

  include Add_group.Extend(struct
    type nonrec t = t
    let add, neg = add, neg
  end)
  include Mul_group.Extend(struct
    type nonrec t = t
    let one, mul, inv = one, mul, inv
  end)
end
