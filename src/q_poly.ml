include Polynomial.Make(Rational)

(* 整数係数にそろえる：分母の最小公倍数を掛ける *)
let to_integer_poly (p: t): Integer.t list =
  let d =
    List.fold_left (fun acc (_, den) -> Integer.lcm acc den) Integer.one p in
  List.map
    (fun (num, den) -> Integer.div_rem (Integer.mul num d) den |> fst) p

(* 約数（正と負） *)
let divisors (n: Integer.t): Integer.t list =
  let m = Integer.abs n in
  List.init m (fun i -> i + 1)
  |> List.filter (fun d ->
       Integer.equal (Integer.div_rem m d |> snd) Integer.zero)
  |> List.concat_map (fun d -> [d; Integer.neg d])

(* 有理数の根：候補 p/q（pは定数項の約数、qは最高次の係数の正の約数）を
   代入して確かめる *)
let rec solve_equation (p: t): Rational.t list =
  match to_integer_poly (trim p) with
  | [] | [_] -> []
  | a0 :: _ when Integer.equal a0 Integer.zero ->
    let x = [Rational.zero; Rational.one] in
    Rational.zero :: solve_equation (div_rem p x |> fst)
  | a0 :: _ as ints ->
    let an = List.hd (List.rev ints) in
    List.filter (fun q -> Integer.compare q Integer.zero > 0) (divisors an)
    |> List.concat_map (fun q -> List.map (fun n -> (n, q)) (divisors a0))
    |> List.sort_uniq Rational.compare
    |> List.filter (fun c -> Rational.equal (eval p c) Rational.zero)
