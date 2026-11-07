module Coeff = Rational

(* 基底：相異なる素数を小さい順に並べたリスト。[2; 3]は√(2・3)、[]は1 *)
type basis = Integer.t list
(* 元：基底と係数の組を、基底の順に並べたリスト *)
type t = (basis * Coeff.t) list

(* 基底の順序：素数のリストを先頭から比べる（辞書式順序） *)
let compare_basis (s1 : basis) (s2 : basis) : int =
  List.compare Integer.compare s1 s2

(* 正規化：基底の順に並べ、同じ基底の係数をまとめ、係数0の項を取り除く *)
let normalize (terms : (basis * Coeff.t) list) : t =
  let by_basis (s1, _) (s2, _) = compare_basis s1 s2 in
  let rec merge = function
    | (s1, c1) :: (s2, c2) :: rest when compare_basis s1 s2 = 0 ->
        merge ((s1, Coeff.add c1 c2) :: rest)
    | (_, c) :: rest when Coeff.equal c Coeff.zero -> merge rest
    | term :: rest -> term :: merge rest
    | [] -> []
  in
  merge (List.sort by_basis terms)

let zero: t = []
let one: t = [([], Coeff.one)]

(* 加法 *)
let add (v1 : t) (v2 : t) : t =
  normalize (v1 @ v2)

(* 符号反転 *)
let neg (v : t) : t =
  List.map (fun (s, c) -> (s, Coeff.neg c)) v

(* 基底同士の積：共通部分の積が外に出て、対称差が根号の中に残る *)
let mul_basis (s1 : basis) (s2 : basis) : Coeff.t * basis =
  let common = List.filter (fun p -> List.mem p s2) s1 in
  let only1 = List.filter (fun p -> not (List.mem p s2)) s1 in
  let only2 = List.filter (fun p -> not (List.mem p s1)) s2 in
  (Coeff.of_integer (List.fold_left Integer.mul Integer.one common),
   List.sort Integer.compare (only1 @ only2))

(* 乗法 *)
let mul (v1 : t) (v2 : t) : t =
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
include Mul_monoid.Extend(struct
  type nonrec t = t
  let one, mul = one, mul
end)

(* 有理数の埋め込み *)
let of_rational (q : Coeff.t) : t =
  normalize [([], q)]

(* 正の整数mを s^2 * (相異なる素数の積) に分け、sと素数のリスト（小さい順）を返す *)
let split_square (m : Integer.t) : Integer.t * basis =
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
let of_rational_sqrt (q : Coeff.t) : t =
  if Coeff.equal q Coeff.zero then zero
  else
    let (n, d) = q in
    let (s, primes) = split_square (Integer.mul n d) in
    normalize [(primes, Coeff.div (Coeff.of_integer s) (Coeff.of_integer d))]

(* 全順序 *)
let compare (x1 : t) (x2 : t) : int =
  List.compare (fun (s1, c1) (s2, c2) ->
    let o = compare_basis s1 s2 in
    if o <> 0 then o else Coeff.compare c1 c2
  ) x1 x2

(* 等価判定 *)
let equal (x1 : t) (x2 : t) : bool =
  compare x1 x2 = 0

(* 登場する素数全体 *)
let collect_primes (v : t) : basis =
  List.sort_uniq Integer.compare (List.concat_map fst v)

(* 素数pの有無による分解：v = a + b√p *)
let split_by_prime (p : Integer.t) (v : t) : t * t =
  let (with_p, without_p) = List.partition (fun (s, _) -> List.mem p s) v in
  let remove_p (s, c) = (List.filter (fun q -> q <> p) s, c) in
  (without_p, normalize (List.map remove_p with_p))

(* 逆元 *)
let rec inv (v : t) : t =
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

(* ここから先は3次方程式の章で追加 *)

(* スカラー倍 *)
let mul_scalar (k : Coeff.t) (v : t) : t =
  normalize (List.map (fun (s, c) -> (s, Coeff.mul k c)) v)

(* スカラーでの除法 *)
let div_scalar (k : Coeff.t) (v : t) : t =
  mul_scalar (Coeff.inv k) v

(* 登場する素数の最大値 *)
let max_prime (x : t) : Integer.t option =
  List.fold_left (fun _ p -> Some p) None (collect_primes x)

(* √pの生成 *)
let make_sqrt_prime (p : Integer.t) : t =
  [([p], Coeff.one)]

(* 有理数への変換 *)
let to_rational (x : t) : Coeff.t option =
  match x with
  | [] -> Some Coeff.zero
  | [([], c)] -> Some c
  | _ -> None

(* 有理数判定 *)
let is_rational (x : t) : bool =
  match to_rational x with
  | Some _ -> true
  | None -> false

(* 立方根 *)
let rec cbrt (x : t) : t option =
    if equal x zero then Some zero
    else if is_rational x then
      match to_rational x with
      | Some q -> (
          match Rational.root 3 q with
          | Some r -> Some (of_rational r)
          | None -> None
        )
      | None -> None
    else
      match max_prime x with
      | None -> None
      | Some p ->
          let a_elem, b = split_by_prime p x in
          let sqrt_p = make_sqrt_prime p in
          match to_rational a_elem with
          | None -> None
          | Some a ->
              (* ノルム N(x) = a^2 - b^2 * p *)
              let b2 = mul b b in
              let p_elem = mul b2 (of_rational (Coeff.of_integer p)) in
              let norm = sub (of_rational (Coeff.mul a a)) p_elem in

              match cbrt norm with
              | None -> None
              | Some n_y_elem -> (
                  match to_rational n_y_elem with
                  | None -> None
                  | Some n_y ->
                      (* cの方程式: 4c^3 - 3*N_y*c - a = 0 *)
                      let four = Coeff.of_integer 4 in
                      let three = Coeff.of_integer 3 in
                      let poly_c = [
                        Coeff.neg a;
                        Coeff.neg (Coeff.mul three n_y);
                        Coeff.zero;
                        four
                      ] in

                      let c_candidates = Q_poly.solve_equation poly_c in

                      (* dの復元と検算 *)
                      let try_construct q_c =
                        if Coeff.equal q_c Coeff.zero then None
                        else
                          let c = of_rational q_c in
                          let q_c2 = Coeff.mul q_c q_c in
                          let q_denom = Coeff.sub (Coeff.mul four q_c2) n_y in
                          if Coeff.equal q_denom Coeff.zero then None
                          else
                            let d = div_scalar q_denom b in
                            let result = add c (mul d sqrt_p) in
                            let result3 = mul (mul result result) result in
                            if equal result3 x then Some result
                            else None
                      in

                      List.find_map try_construct c_candidates
                )
