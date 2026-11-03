(* 積分の答え: 初等関数で書けるか、書けないと判定できたか *)
type 'a result = Elementary of 'a | Not_elementary

let map (f: 'a -> 'b) (r: 'a result): 'b result =
  match r with
  | Elementary a -> Elementary (f a)
  | Not_elementary -> Not_elementary

(* 2つの部分の答えを合わせる。どちらかが書けなければ、全体も書けない *)
let map2 (f: 'a -> 'a -> 'a) (r1: 'a result) (r2: 'a result): 'a result =
  match r1, r2 with
  | Elementary a1, Elementary a2 -> Elementary (f a1 a2)
  | _ -> Not_elementary

(* 初等関数で書けるときの答え *)
let get (r: 'a result): 'a =
  match r with
  | Elementary a -> a
  | Not_elementary -> failwith "Integral.get: not elementary"

(* 積分できる微分体: 体Fと、Fの元の積分。e^uを添加した上の段で積分するには、
   y' + a y = g をFで解くこと（リッシュの微分方程式）も必要になる *)
module type S = sig
  module F: Differential.S
  module A: Liouville.S with type field = F.t and type constant = F.constant
  val integrate: F.t -> A.t result
  val solve_rde: F.t -> F.t -> F.t option (* y' + a y = g の解y。なければNone *)
end
