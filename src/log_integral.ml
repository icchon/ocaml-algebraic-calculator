(* Kで積分できれば、θ = log u を添加した K(θ) でも積分できる。
   被積分関数はθの多項式 p0 + p1 θ + p2 θ^2 + ...（p_i ∈ K）に限る *)
module type FUNCTOR_TYPE =
  functor (I: Integral.S) (L: Tower.ELEMENTARY with module K = I.F) ->
    (Integral.S with module F = L)

module Make: FUNCTOR_TYPE =
  functor (I: Integral.S) (L: Tower.ELEMENTARY with module K = I.F) -> struct
  module F = L
  module A = Liouville.Make(L)

  (* 整数をKの元として埋め込む *)
  let rec of_integer (n: int): I.F.t = if n = 0 then I.F.zero else I.F.add I.F.one (of_integer (n - 1))

  (* Kでの答えを K(θ) へ持ち上げる。c log u は c θ にまとめる *)
  let lift (b: I.A.t): A.t =
    List.fold_left (fun acc (c, w) ->
      if I.F.equal w L.argument then
        A.add acc (A.of_field (L.mul (L.of_constant c) L.theta))
      else A.add acc (A.log c (L.of_base w)))
      (A.of_field (L.of_base b.v)) b.logs

  (* Kでの答え s + c log u を、sとcに分ける。u以外のlogは扱わない *)
  let split (b: I.A.t): I.F.t * I.F.t =
    List.fold_left (fun (s, c) (c', w) ->
      if I.F.equal w L.argument then (s, I.F.add c (I.F.of_constant c'))
      else failwith "Log_integral.integrate: a new logarithm appears in a coefficient")
      (b.v, I.F.zero) b.logs

  (* 最高次の項から部分積分する。∫p_n = s + c θ のとき、
     (s θ^n + c/(n+1) θ^(n+1))' の最高次の項は p_n θ^n なので、
     これを引いた残りの次数は n より下がる *)
  let rec integrate_poly (p: L.P.t): A.t Integral.result =
    match List.rev (L.P.normalize p) with
    | [] -> Integral.Elementary A.zero
    | [p0] -> Integral.map lift (I.integrate p0)
    | p_n :: _ ->
      match I.integrate p_n with
      | Integral.Not_elementary -> Integral.Not_elementary
      | Integral.Elementary b ->
        let n = L.P.degree p in
        let (s, c) = split b in
        let top = List.init n (fun _ -> I.F.zero) @ [s; I.F.div c (of_integer (n + 1))] in
        Integral.map (A.add (A.of_field (L.of_poly top)))
          (integrate_poly (L.P.sub p (L.P.differentiate top)))

  let integrate (f: L.t): A.t Integral.result = integrate_poly (L.to_poly f)

  (* θを含まない a, g なら、Kでの解がそのまま使える *)
  let solve_rde (a: L.t) (g: L.t): L.t option =
    Option.map L.of_base (I.solve_rde (L.to_base a) (L.to_base g))
end
