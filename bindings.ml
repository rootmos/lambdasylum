open Core_kernel.Std

type error = Unbound of string
exception Bindings_exception of error * string

let explain subsystem = function
  Unbound n -> sprintf "%s is unbound during %s" n subsystem

module Make(V: sig
  type t
  val subsystem: string
end) = struct
  type bindings = (string * V.t) list
  type t = {
    bindings: bindings
  }

  let empty = { bindings = [] }

  let bind ctx n v = { bindings = (n, v) :: ctx.bindings }

  let lookup ctx n =
    match List.Assoc.find ~equal:(=) ctx.bindings n with
    | Some v -> v
    | None -> raise @@ Bindings_exception (Unbound n, V.subsystem)
end
