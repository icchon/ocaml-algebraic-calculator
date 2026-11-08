type t = int
(* 正規化：整数の表し方は、もともと1とおり *)
let normalize (x: t): t =
  x
let zero: t = 0
let one: t = 1
(* 加法 *)
let add (x1: t) (x2: t): t =
  x1 + x2
(* 符号反転 *)
let neg (x: t): t =
  -x
(* 乗法 *)
let mul (x1: t) (x2: t): t =
  x1 * x2
(* 商と余り *)
let div_rem (x1: t) (x2: t): t * t =
  (x1 / x2, x1 mod x2)

(* 大小比較 *)
let compare (x1: t) (x2: t): int =
  x1 - x2
(* 絶対値 *)
let abs (x: t): t =
  if compare x zero >= 0 then x else neg x
(* 符号のそろえ方（正にする） *)
let canonical (x: t): t =
  abs x

include Normal_form.Extend(struct
  type nonrec t = t
  let normalize = normalize
end)
include Add_group.Extend(struct
  type nonrec t = t
  let add, neg = add, neg
end)
include Mul_monoid.Extend(struct
  type nonrec t = t
  let one, mul = one, mul
end)
include Euclidean.Extend(struct
  type nonrec t = t
  let equal, zero, one, sub, mul, div_rem, canonical =
    equal, zero, one, sub, mul, div_rem, canonical
end)

