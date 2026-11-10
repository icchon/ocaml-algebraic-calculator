module Coeff = Rational

(* キー：相異なる素数を小さい順に並べたリスト。[2; 3]は√(2・3)、[]は1 *)
type basis = Integer.t list
(* 元：キーと係数の組を、キーの順に並べたリスト *)
type t = (basis * Coeff.t) list

(* キーの順序：素数のリストを先頭から比べる（辞書式順序） *)
let compare_basis (s1: basis) (s2: basis): int =
  List.compare Integer.compare s1 s2

(* 正規化：キーの順に並べ、同じキーの係数をまとめ、係数0の項を取り除く *)
let normalize (terms: (basis * Coeff.t) list): t =
  let by_basis (s1, _) (s2, _) = compare_basis s1 s2 in
  let rec merge = function
    | (s1, c1) :: (s2, c2) :: rest when compare_basis s1 s2 = 0 ->
        merge ((s1, Coeff.add c1 c2) :: rest)
    | (_, c) :: rest when Coeff.equal c Coeff.zero -> merge rest
    | (s, c) :: rest -> (s, Coeff.normalize c) :: merge rest
    | [] -> []
  in
  merge (List.sort by_basis terms)

include Normal_form.Extend(struct
  type nonrec t = t
  let normalize = normalize
end)

let zero: t = []
let one: t = [([], Coeff.one)]

(* 加法 *)
let add (v1: t) (v2: t): t =
  normalize (v1 @ v2)

(* 符号反転 *)
let neg (v: t): t =
  List.map (fun (s, c) -> (s, Coeff.neg c)) v

(* キーどうしの積：共通部分の積が外に出て、対称差が根号の中に残る *)
let mul_basis (s1: basis) (s2: basis): Coeff.t * basis =
  let common = List.filter (fun p -> List.mem p s2) s1 in
  let only1 = List.filter (fun p -> not (List.mem p s2)) s1 in
  let only2 = List.filter (fun p -> not (List.mem p s1)) s2 in
  (Coeff.of_integer (List.fold_left Integer.mul Integer.one common),
   List.sort Integer.compare (only1 @ only2))

(* 乗法 *)
let mul (v1: t) (v2: t): t =
  normalize (List.concat_map (fun (s1, c1) ->
    List.map (fun (s2, c2) ->
      let (k, s) = mul_basis s1 s2 in
      (s, Coeff.mul k (Coeff.mul c1 c2))
    ) v2
  ) v1)

include Add_group.Extend(struct
  type nonrec t = t
  let add, neg = add, neg
end)

(* 有理数の埋め込み *)
let of_rational (q: Coeff.t): t =
  normalize [([], q)]

(* 正の整数mを s^2 * (相異なる素数の積) に分け、sと素数のリスト（小さい順）を返す *)
let split_square (m: Integer.t): Integer.t * basis =
  let two = Integer.add Integer.one Integer.one in
  let rec aux p m s primes =
    if Integer.compare (Integer.mul p p) m > 0 then
      if Integer.equal m Integer.one then (s, List.rev primes)
      else (s, List.rev (m :: primes))
    else
      let (q2, r2) = Integer.div_rem m (Integer.mul p p) in
      let (q1, r1) = Integer.div_rem m p in
      let next = Integer.add p Integer.one in
      if Integer.equal r2 Integer.zero then aux p q2 (Integer.mul s p) primes
      else if Integer.equal r1 Integer.zero then aux next q1 s (p :: primes)
      else aux next m s primes
  in
  aux two m Integer.one []

(* 有理数の平方根（qは0以上）。√(n/d) = √(nd)/d = (s/d)√(素数の積) *)
let of_rational_sqrt (q: Coeff.t): t =
  if Coeff.equal q Coeff.zero then zero
  else
    let (n, d) = q in
    let (s, primes) = split_square (Integer.mul n d) in
    normalize [(primes, Coeff.div (Coeff.of_integer s) (Coeff.of_integer d))]

(* 登場する素数全体 *)
let collect_primes (v: t): basis =
  List.sort_uniq Integer.compare (List.concat_map fst v)

(* 素数pの有無による分解：v = a + b√p *)
let split_by_prime (p: Integer.t) (v: t): t * t =
  let (with_p, without_p) = List.partition (fun (s, _) -> List.mem p s) v in
  let remove_p (s, c) = (List.filter (fun q -> q <> p) s, c) in
  (without_p, normalize (List.map remove_p with_p))

(* 逆元 *)
let rec inv (v: t): t =
  match v, collect_primes v with
  | [], _ -> failwith "Division by zero"
  | (_, c) :: _, [] -> of_rational (Coeff.inv c)
  | _, p :: _ ->
      let (a, b) = split_by_prime p v in
      let conj = sub a (mul b [([p], Coeff.one)]) in
      let norm = mul v conj in
      mul conj (inv norm)

include Mul_group.Extend(struct
  type nonrec t = t
  let one, mul, inv = one, mul, inv
end)

(* 共役 *)
let conj (x: t): t = x

(* 有理数への変換。√pを含むならNone *)
let to_rational (x: t): Coeff.t option =
  match x with
  | [] -> Some Coeff.zero
  | [([], c)] -> Some c
  | _ -> None
