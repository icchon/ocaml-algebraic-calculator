(* Kで積分でき、Kでリッシュの微分方程式が解ければ、θ = e^u を添加した
   K(θ) でも積分できるかどうかが決まる。被積分関数はθの多項式
   Σ p_i θ^i（p_i ∈ K）に限る *)
module type FUNCTOR_TYPE =
  functor (I: Integral.S) (E: Tower.ELEMENTARY with module K = I.F) ->
    (Integral.S with module F = E)

module Make: FUNCTOR_TYPE =
  functor (I: Integral.S) (E: Tower.ELEMENTARY with module K = I.F) -> struct
  module F = E
  module A = Liouville.Make(E)

  (* 整数をKの元として埋め込む *)
  let rec of_int (n: int): I.F.t = if n = 0 then I.F.zero else I.F.add I.F.one (of_int (n - 1))

  (* Kでの答えを K(θ) へ持ち上げる *)
  let lift (b: I.A.t): A.t =
    List.fold_left (fun acc (c, w) -> A.add acc (A.log c (E.of_base w)))
      (A.of_field (E.of_base b.v)) b.logs

  (* p_i θ^i（i ≥ 1）の積分は y θ^i の形で、
     (y θ^i)' = (y' + i u' y) θ^i なので、y' + i u' y = p_i を解く。
     解がなければ初等関数では書けない *)
  let integrate (f: E.t): A.t Integral.result =
    match E.to_poly f with
    | [] -> Integral.Elementary A.zero
    | p0 :: rest ->
      let u' = I.F.differentiate E.argument in
      let ys =
        List.mapi
          (fun i p_i -> I.solve_rde (I.F.mul (of_int (i + 1)) u') p_i) rest
      in
      if List.mem None ys then Integral.Not_elementary
      else
        let y = E.of_poly (I.F.zero :: List.map Option.get ys) in
        Integral.map (fun b -> A.add (lift b) (A.of_field y)) (I.integrate p0)

  (* θを含まない a, g なら、Kでの解がそのまま使える *)
  let solve_rde (a: E.t) (g: E.t): E.t option =
    Option.map E.of_base (I.solve_rde (E.to_base a) (E.to_base g))
end
