
module type DIM = sig
  val dim: int
end

module type S = sig
  include Matrix.S
  val transpose: t -> t
  include Mul_monoid.S with type t := t
end

module type FUNCTOR_TYPE =
  functor (D: DIM) (F: Field.S) ->
    (S with type scalar = F.t and type t = F.t list list)

module Make: FUNCTOR_TYPE = functor (D: DIM) (F: Field.S) -> struct
  include Matrix.Make(struct
    let cols = D.dim
    let rows = D.dim
  end)(F)
  (* 単位行列 *)
  let one =
    List.init D.dim (fun i ->
      List.init D.dim (fun j -> if i = j then F.one else F.zero)
  )
  (* 転置 *)
  let transpose m =
    List.init D.dim (fun i ->
      List.init D.dim (fun j ->
        List.nth (List.nth m j) i
      )
  )
  (* 乗法 *)
  let mul a b =
    let b_trans = transpose b in
    List.map (fun row_a ->
      List.map (fun col_b ->
        List.fold_left2 (fun acc x y -> 
          F.mul x y |> F.add acc
        ) F.zero row_a col_b
      ) b_trans
    ) a  
  include Mul_monoid.Extend(struct
    type nonrec t = t
    let one, mul = one, mul
  end)
end
