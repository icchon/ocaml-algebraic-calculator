(* 微分体Fに超越的な元θを1つ添加した微分体 F(θ)。体の演算は
   多項式ファンクタと分数体ファンクタにまかせ、θ'だけを新しく決める *)

(* F[θ]と、その微分 *)
module type POLY = sig
  include Polynomial.S
  val differentiate: t -> t
end

module type S = sig
  module F: Differential.S
  module P: POLY with type coeff = F.t (* F[θ] *)
  include Differential.S with type t = P.t * P.t
                          and type constant = F.constant
  val theta: t
  val of_base: F.t -> t
  val of_poly: P.t -> t
  val to_poly: t -> P.t (* 分母がθを含まないときだけ *)
  val to_base: t -> F.t (* θを含まないときだけ *)
end

module type DERIVATIVE = sig
  module F: Differential.S
  val derivative: F.t list (* θ' = f(θ) の f の係数 *)
end

module type FUNCTOR_TYPE =
  functor (D: DERIVATIVE) -> (S with module F = D.F)

module Make: FUNCTOR_TYPE = functor (D: DERIVATIVE) -> struct
  module F = D.F
  module P = struct
    include Polynomial.Make(F)
    (* 整数を1つ下の体の元として埋め込む *)
    let rec of_integer (n: int): F.t =
      if n <= 0 then F.zero else F.add F.one (of_integer (n - 1))
    (* θについてのべき乗則: c0 + c1 θ + c2 θ^2 + ... から
       c1 + 2 c2 θ + 3 c3 θ^2 + ... をつくる *)
    let power_rule (p: t): t =
      match p with
      | [] -> []
      | _ :: rest -> List.mapi (fun i c -> F.mul c (of_integer (i + 1))) rest
    (* 微分: (c0 + c1 θ + ...)' =
         (c0' + c1' θ + ...) + (c1 + 2 c2 θ + ...) θ' *)
    let differentiate (p: t): t =
      add (List.map F.differentiate p) (mul (power_rule p) D.derivative)
  end
  include Fraction.Make(P)
  type constant = F.constant

  (* 埋め込み：θの多項式・1つ下の体の元・定数を、分母1の分数とみなす *)
  let of_poly (p: P.t): t = (P.trim p, P.one)
  let of_base (k: F.t): t = of_poly [k]
  let of_constant (c: constant): t = of_base (F.of_constant c)
  (* 添加した元θ（θの多項式 0 + 1θ） *)
  let theta: t = of_poly [F.zero; F.one]

  (* 微分: 商の微分法 (n/d)' = (n'd - nd')/d^2 *)
  let differentiate ((n, d): t): t =
    let n' = P.differentiate n and d' = P.differentiate d in
    div (of_poly (P.sub (P.mul n' d) (P.mul n d'))) (of_poly (P.mul d d))

  (* θの多項式とみなす。分母がθを含むなら扱わない *)
  let to_poly ((n, d): t): P.t =
    match P.trim d with
    | [d0] -> P.div_scalar d0 (P.trim n)
    | _ -> failwith "Tower.to_poly: theta appears in the denominator"

  (* Fの元とみなす。θを含むなら扱わない *)
  let to_base (x: t): F.t =
    match to_poly x with
    | [] -> F.zero
    | [k] -> k
    | _ -> failwith "Tower.to_base: the element contains theta"
end

(* log u や e^u のように、Fの元uからつくる段。uを覚えておく *)
module type ELEMENTARY = sig
  include S
  val argument: F.t (* u *)
end

module type ARGUMENT = sig
  module F: Differential.S
  val argument: F.t
end

(* 変数x: x' = 1。定数の体の上に、log/expと同じファンクタでつくる *)
module Variable(K: Differential.S): S with module F = K =
  Make(struct
    module F = K
    let derivative: F.t list = [F.one]
  end)

(* θ = log u: θ' = u'/u *)
module Log(A: ARGUMENT): ELEMENTARY with module F = A.F = struct
  include Make(struct
    module F = A.F
    let derivative: F.t list = [F.div (F.differentiate A.argument) A.argument]
  end)
  let argument: F.t = A.argument
end

(* θ = e^u: θ' = u'θ *)
module Exp(A: ARGUMENT): ELEMENTARY with module F = A.F = struct
  include Make(struct
    module F = A.F
    let derivative: F.t list = [F.zero; F.differentiate A.argument]
  end)
  let argument: F.t = A.argument
end
