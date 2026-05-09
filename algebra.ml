module type OfInt = sig
  type t
  val of_int : int -> t
end

module type PreRing = sig
  type t
  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( * ) : t -> t -> t
  val to_string : t -> string
end

module type Ring = sig
  include PreRing
  include OfInt with type t := t
end

module type PreField = sig
  include PreRing 
  val ( / ) : t -> t -> t
end

module type Field = sig
  include PreField
  include OfInt with type t := t
end

module RingOfPreRing (R: PreRing) = (struct
  include R
  (* not optimal, but works *)
  let rec of_int n =
    if n > 0 then R.(of_int (n - 1) + one) else
    if n < 0 then of_int (0 - n) else
    R.zero
end : Ring with type t = R.t)

module FieldOfPreField (F: PreField) = (struct
  module R : (OfInt with type t := F.t) = RingOfPreRing(F)
  include F
  include R
  let of_int = raise (Invalid_argument "asdf")
end : Field)

module IntPreRing = struct
  type t = int
  let zero = 0
  let one = 1
  let ( + ) = ( + )
  let ( ~- ) = ( ~- )
  let ( * ) = ( * )
  let to_string = string_of_int
end

module IntRing: Ring = RingOfPreRing(IntPreRing)

module IntPreField = struct
  include IntPreRing
  let ( / ) = ( / )
end


module IntField : Field = FieldOfPreField(IntPreField)

module FloatPreRing = struct
  type t = float
  let zero = 0.
  let one = 1.
  let ( + ) = ( +. )
  let ( ~- ) = ( ~-. )
  let ( * ) = ( *. )
  let to_string = string_of_float
end

module FloatRing : Ring = RingOfPreRing(FloatPreRing)

module FloatPreField = struct
  include FloatPreRing
  let (/) = (/.)
end

module FloatField : Field = FieldOfPreField(FloatPreField)


module Fraction (R: Ring) = (struct
  type t = R.t * R.t
  let zero = R.zero, R.one
  let one = R.one, R.one
  let ( + ) (a, b) (c, d) = R.((a * d) + (c * b), b * d)
  let ( ~- ) (a, b) = R.(-a, b)
  let ( * ) (a, b) (c, d) = R.(a * c, b * d)
  let ( / ) (a, b) (c, d) = (a, b) * (d, c)
  let to_string (a, b) = R.("(" ^ to_string a ^ "/" ^ to_string b ^ ")")
  let of_int n = R.of_int n, R.one
end: Field)


module IntRational : Field = Fraction(IntRing)

module FloatRational : Field = Fraction(FloatRing)
