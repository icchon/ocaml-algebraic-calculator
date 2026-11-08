module type MIN = sig
  type t
  val normalize: t -> t
end

module Extend(M: MIN) = struct
  (* 等価判定：正規化してから、データをそのまま比べる *)
  let equal (x1: M.t) (x2: M.t): bool =
    M.normalize x1 = M.normalize x2
end
