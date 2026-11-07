module type S = sig
  type t
  val equal: t -> t -> bool
  include Add_group.S with type t := t
  include Mul_group.S with type t := t
end
