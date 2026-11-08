(* 有理数係数の多項式の根と多重度を、定数の体（√pとiを含む体）の中に求める。
   有理数の根を探し、残りから2次の因数を探して、2次方程式の解の公式で解く。
   有理数の上で3次以上の既約な因数が残れば、根は定数の体に入らないのでNone *)

let linear (r: Rational.t): Q_poly.t = [Rational.neg r; Rational.one]

(* 2次の因数を探す（クロネッカーの方法）。g(0), g(1), g(-1) の候補 c, s, t から
   g = a x^2 + b x + c（2a = s + t - 2c、2b = s - t）を決め、割り切れるものを探す *)
let quadratic_factor (p: Q_poly.t): Q_poly.t option =
  let p = List.map Rational.of_integer (Q_poly.to_integer_poly p) in
  let ds (x: Integer.t): Integer.t list =
    Q_poly.divisors (fst (Q_poly.eval p (Rational.of_integer x))) in
  let make (c: Integer.t) (s: Integer.t) (t: Integer.t): Q_poly.t option =
    let a2 = Integer.sub (Integer.add s t) (Integer.add c c) in
    if Integer.equal a2 Integer.zero then None
    else Some (Q_poly.normalize [(c, 1); (Integer.sub s t, 2); (a2, 2)])
  in
  List.concat_map (fun c ->
    List.concat_map (fun s -> List.filter_map (make c s) (ds (-1))) (ds 1))
    (ds 0)
  |> List.find_opt (fun g -> Q_poly.equal (snd (Q_poly.div_rem p g)) Q_poly.zero)

exception Irreducible

(* 1次・2次の因数を1つずつ取り出して、根を（重複を含めて）並べる *)
let rec all_roots (p: Q_poly.t): Constant.t list =
  match Q_poly.normalize p with
  | [] | [_] -> []
  | [c; b; a] when Q_poly.solve_equation [c; b; a] = [] ->
    let (x1, x2) = Quadratic.solve a b c in [x1; x2]
  | p ->
    match Q_poly.solve_equation p with
    | r :: _ ->
      Constant.of_rational r :: all_roots (fst (Q_poly.div_rem p (linear r)))
    | [] ->
      match quadratic_factor p with
      | Some g -> all_roots g @ all_roots (fst (Q_poly.div_rem p g))
      | None -> raise Irreducible

(* 同じ根をまとめて、根と多重度の組にする *)
let roots (p: Q_poly.t): (Constant.t * int) list option =
  match all_roots p with
  | exception Irreducible -> None
  | rs ->
    Some (List.fold_left (fun acc r ->
      match List.partition (fun (s, _) -> Constant.equal r s) acc with
      | [(_, m)], rest -> rest @ [(r, m + 1)]
      | _ -> acc @ [(r, 1)])
      [] rs)
