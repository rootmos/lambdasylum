open Core_kernel.Std

type error = Binding of string | ApplicationError
exception Ulambda_exception of error

type pattern = [`Ident of string | `Wildcard]
[@@deriving sexp]

type term = [
  `App of term * term
| `Lambda of pattern * term
| `Ident of string
| `Int of int]
[@@deriving sexp]

type value = [
  `Lambda of pattern * term
| `Int of int]
[@@deriving sexp]

module Ctx = struct
  type t = {
    bindings: (string * value) list
  } [@@deriving sexp]

  let empty = { bindings = [] }

  let bind ctx n v = { bindings = (n, v) :: ctx.bindings }

  let lookup ctx n =
    match List.Assoc.find ~equal:(=) ctx.bindings n with
    | Some v -> v
    | None -> raise @@ Ulambda_exception (Binding n)

  let pretty_print ctx =
    sexp_of_t ctx |> Sexp.to_string_hum |> print_endline
end

let rec reduce ctx = function
  `Int i -> `Int i
| `Lambda x -> `Lambda x
| `Ident n -> Ctx.lookup ctx n
| `App (f, a) ->
    let a' = reduce ctx a in
    begin match reduce ctx f with
    | `Lambda (`Wildcard, t) -> reduce ctx t
    | `Lambda (`Ident n, t) -> reduce (Ctx.bind ctx n a') t
    | _ -> raise @@ Ulambda_exception ApplicationError
    end
