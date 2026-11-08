module type S = sig
  type t
  val normalize: t -> t
  val equal: t -> t -> bool
  include Add_group.S with type t := t
  include Mul_monoid.S with type t := t
end
