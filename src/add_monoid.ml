

module type S = sig
  include Normal_form.S
  val zero: t
  val add: t -> t -> t
end
