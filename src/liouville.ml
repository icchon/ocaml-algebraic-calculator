(* 初等関数で書ける積分の答えの形（リウヴィルの定理）: 体Fの元vと、
   定数c_i、Fの元w_iを使って v + Σ c_i log w_i と書ける *)

module type S = sig
  type field
  type constant
  type t = { v: field; logs: (constant * field) list }
  val zero: t
  val of_field: field -> t
  val log: constant -> field -> t
  val add: t -> t -> t
  val differentiate: t -> field (* v' + Σ c_i w_i'/w_i *)
end

module type FUNCTOR_TYPE = functor (F: Differential.S) ->
  (S with type field = F.t and type constant = F.constant)

module Make: FUNCTOR_TYPE = functor (F: Differential.S) -> struct
  type field = F.t
  type constant = F.constant
  type t = { v: field; logs: (constant * field) list }
  let zero: t = { v = F.zero; logs = [] }
  let of_field (v: field): t = { v; logs = [] }
  let log (c: constant) (w: field): t = { v = F.zero; logs = [(c, w)] }
  (* 加法 *)
  let add (a: t) (b: t): t = { v = F.add a.v b.v; logs = a.logs @ b.logs }
  (* 微分: (c log w)' = c w'/w *)
  let differentiate (a: t): field =
    List.fold_left (fun acc (c, w) ->
      F.add acc (F.mul (F.of_constant c) (F.div (F.differentiate w) w)))
      (F.differentiate a.v) a.logs
end
