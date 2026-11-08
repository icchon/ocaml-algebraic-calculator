module type MIN = sig
  type t
  val normalize: t -> t
end

(* 本書の代数すべての土台：表し方を1つに決める正規化と、等価判定 *)
module type S = sig
  include MIN
  val equal: t -> t -> bool
end

module Extend(M: MIN) = struct
  (* 等価判定：正規化してから、データをそのまま比べる *)
  let equal (x1: M.t) (x2: M.t): bool =
    M.normalize x1 = M.normalize x2
end
