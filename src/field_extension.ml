
module type IRREDUCIBLE_POLY = sig
  module C: Field.S
  module P: Polynomial.S with type coeff = C.t
  val poly: P.t
end

module type FUNCTOR_TYPE =
  functor (I: IRREDUCIBLE_POLY) -> (Field.S with type t = I.P.t)

module Make: FUNCTOR_TYPE = functor (I: IRREDUCIBLE_POLY) -> struct
  type t = I.P.t
  (* 正規化：既約多項式で割った余り *)
  let normalize (p: t): t =
    I.P.div_rem p I.poly |> snd |> I.P.normalize
  include Normal_form.Extend(struct
    type nonrec t = t
    let normalize = normalize
  end)
  let zero: t = I.P.zero
  let one: t = I.P.one
  (* 加法 *)
  let add (x: t) (y: t): t =
    I.P.add x y |> normalize
  (* 符号反転 *)
  let neg (x: t): t =
    I.P.neg x |> normalize
  (* 乗法 *)
  let mul (x: t) (y: t): t =
    I.P.mul x y |> normalize
  (* 逆元 *)
  let inv (x: t): t =
    let x_norm = normalize x in
    if equal x_norm zero then
      failwith "Division by zero"
    else
      let (g, u, _) = I.P.ext_gcd x_norm I.poly in
      let lc = I.P.leading_coeff g in
      let x_inv = I.P.div_scalar lc u in
      normalize x_inv

  include Add_group.Extend(struct
    type nonrec t = t
    let add, neg = add, neg
  end)
  include Mul_group.Extend(struct
    type nonrec t = t
    let one, mul, inv = one, mul, inv
  end)
end
