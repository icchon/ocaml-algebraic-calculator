include Fraction.Make(Integer)
(* 整数の埋め込み *)
let of_integer (x: Integer.t): t =
  (x, Integer.one)
(* 大小比較：分母は正なので、通分した分子どうしを比べる *)
let compare ((n1, d1): t) ((n2, d2): t): int =
  Integer.compare (Integer.mul n1 d2) (Integer.mul n2 d1)
