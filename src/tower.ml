(* 微分体Kに、超越的な元θを1つ添加した微分体 K(θ)。K(θ)はK[θ]の分数体なので、
   体の演算は多項式ファンクタと分数体ファンクタにまかせる。新しく決めるのはθ'だけで、
   それをθの多項式として与える。微分は、K[θ]ではライプニッツ則から、
   K(θ)では商の微分法から決まる *)

(* K[θ]と、その微分 *)
module type POLY = sig
  include Polynomial.S
  val differentiate: t -> t
end

module type S = sig
  module K: Differential.S
  module P: POLY with type coeff = K.t (* K[θ] *)
  include Differential.S with type t = P.t * P.t and type constant = K.constant
  val theta: t
  val of_base: K.t -> t
  val of_poly: P.t -> t
  val to_poly: t -> P.t (* 分母がθを含まないときだけ *)
  val to_base: t -> K.t (* θを含まないときだけ *)
end

module type DERIVATIVE = sig
  module K: Differential.S
  val derivative: K.t list (* θ' = f(θ) の f の係数 *)
end

module type FUNCTOR_TYPE =
  functor (D: DERIVATIVE) -> (S with module K = D.K)

module Make: FUNCTOR_TYPE = functor (D: DERIVATIVE) -> struct
  module K = D.K
  module P = struct
    include Polynomial.Make(K)
    (* 整数を1つ下の体の元として埋め込む *)
    let rec of_integer (n: int): K.t =
      if n <= 0 then K.zero else K.add K.one (of_integer (n - 1))
    (* θについてのべき乗則: c0 + c1 θ + c2 θ^2 + ... から
       c1 + 2 c2 θ + 3 c3 θ^2 + ... をつくる *)
    let power_rule (p: t): t =
      match p with
      | [] -> []
      | _ :: rest -> List.mapi (fun i c -> K.mul c (of_integer (i + 1))) rest
    (* 微分: (c0 + c1 θ + ...)' =
         (c0' + c1' θ + ...) + (c1 + 2 c2 θ + ...) θ' *)
    let differentiate (p: t): t =
      add (List.map K.differentiate p) (mul (power_rule p) D.derivative)
    (* 最大公約数。定数倍の分だけ選び方が残るので、最高次の係数をbとそろえる。
       分数体ファンクタは分子と分母をgcd n dで割るので、分母の最高次の係数が
       1になり、分数の表し方が1つに決まる *)
    let gcd (a: t) (b: t): t =
      match normalize (gcd a b), normalize b with
      | [], _ | _, [] -> gcd a b
      | g, b -> mul_scalar (K.div (leading_coeff b) (leading_coeff g)) g
    (* 全順序 *)
    let rec compare (x1: t) (x2: t): int =
      match x1, x2 with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | c1 :: r1, c2 :: r2 ->
        let c = K.compare c1 c2 in
        if c <> 0 then c else compare r1 r2
  end
  include Fraction.Make(P)
  type constant = K.constant

  let of_poly (p: P.t): t = (P.normalize p, P.one)
  let of_base (k: K.t): t = of_poly [k]
  let of_constant (c: constant): t = of_base (K.of_constant c)
  let theta: t = of_poly [K.zero; K.one]

  (* 微分: 商の微分法 (n/d)' = (n'd - nd')/d^2 *)
  let differentiate ((n, d): t): t =
    let n' = P.differentiate n and d' = P.differentiate d in
    div (of_poly (P.sub (P.mul n' d) (P.mul n d'))) (of_poly (P.mul d d))

  (* θの多項式とみなす。分母がθを含むなら扱わない *)
  let to_poly ((n, d): t): P.t =
    match P.normalize d with
    | [d0] -> P.div_scalar d0 (P.normalize n)
    | _ -> failwith "Tower.to_poly: theta appears in the denominator"

  (* Kの元とみなす。θを含むなら扱わない *)
  let to_base (x: t): K.t =
    match to_poly x with
    | [] -> K.zero
    | [k] -> k
    | _ -> failwith "Tower.to_base: the element contains theta"
end

(* log u や e^u のように、Kの元uから作る段（初等的な拡大）。uを覚えておく *)
module type ELEMENTARY = sig
  include S
  val argument: K.t (* u *)
end

module type ARGUMENT = sig
  module K: Differential.S
  val argument: K.t
end

(* 変数x: x' = 1。定数の体の上に、log/expと同じファンクタでつくる *)
module Variable(K: Differential.S): S with module K = K =
  Make(struct
    module K = K
    let derivative: K.t list = [K.one]
  end)

(* θ = log u: θ' = u'/u *)
module Log(A: ARGUMENT): ELEMENTARY with module K = A.K = struct
  include Make(struct
    module K = A.K
    let derivative: K.t list = [K.div (K.differentiate A.argument) A.argument]
  end)
  let argument: K.t = A.argument
end

(* θ = e^u: θ' = u'θ *)
module Exp(A: ARGUMENT): ELEMENTARY with module K = A.K = struct
  include Make(struct
    module K = A.K
    let derivative: K.t list = [K.zero; K.differentiate A.argument]
  end)
  let argument: K.t = A.argument
end
