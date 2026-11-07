
module type MATRIX_DIM = sig
    val rows: int
    val cols: int
end

module type S = sig
  include Vector_space.S
  val rows: int
  val cols: int
end

module type FUNCTOR_TYPE =
  functor (D: MATRIX_DIM) (F: Field.S) ->
    (S with type scalar = F.t and type t = F.t list list)

module Make: FUNCTOR_TYPE = functor (D: MATRIX_DIM) (F: Field.S) -> struct
  module Container = struct
    type 'a t = 'a list list
    (* 定数行列の生成 *)
    let make (x: 'a): 'a t =
      List.init D.rows (fun _ -> List.init D.cols (fun _ -> x))
    (* 写像 *)
    let map (f: 'a -> 'b) (m: 'a t): 'b t =
      List.map (List.map f) m
    (* Zip合成 *)
    let map2 (f: 'a -> 'b -> 'c) (m1: 'a t) (m2: 'b t): 'c t =
      List.map2 (List.map2 f) m1 m2
    (* 全称判定 *)
    let for_all2 (f: 'a -> 'b -> bool) (m1: 'a t) (m2: 'b t): bool =
      List.for_all2 (List.for_all2 f) m1 m2
  end

  include Vector_space.Make(F)(Container)
  (* 行数 *)
  let rows = D.rows
  (* 列数 *)
  let cols = D.cols
end
