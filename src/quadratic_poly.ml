include Polynomial.Make(Quadratic)

(* ここから先は微分・積分の章で追加 *)

(* 大小比較（Fraction.Makeに渡すための便宜的な全順序） *)
let rec compare (x1: t) (x2: t): int =
  match x1, x2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | c1 :: r1, c2 :: r2 ->
    let c = Quadratic.compare c1 c2 in
    if c <> 0 then c else compare r1 r2
