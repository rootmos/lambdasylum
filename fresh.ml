module Gen(F: sig
  type t
  val mk: int -> t
end): sig
  val next: unit -> F.t
end = struct
  let counter = ref 0
  let next () = let c = !counter in counter := c + 1; F.mk c
end
