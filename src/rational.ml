include Fraction.Make(Integer)
(* 整数の埋め込み *)
let of_integer (x: Integer.t): t =
  (x, Integer.one)
(* 共役 *)
let conj (x: t): t = x
(* ここから先は2次方程式の章で追加 *)

(* k乗根 *)
let root (k : int) ((n, d) : t) : t option =
  if Integer.equal n Integer.zero then Some zero
  else
    let d_pow = Integer.pow (k - 1) d in
    let target = Integer.mul n d_pow in
    match Integer.root k target with
    | Some num -> Some (mul (num, d) one)
    | None -> None

