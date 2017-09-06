open Core_kernel.Std

type error = Binding of string | ApplicationError
exception Ulambda_exception of error

type pattern = [`Ident of string | `Wildcard]
[@@deriving sexp]

type term = [
  `App of term * term
| `Lambda of pattern * term
| `Ident of string]
[@@deriving sexp]

type value = [
  `Lambda of pattern * term
]
[@@deriving sexp]

let rec church t =
  let rec go ?(acc=`Ident "x") = function
    | 0 -> `Lambda (`Ident "f", `Lambda (`Ident "x", acc))
    | i -> go ~acc:(`App (`Ident "f", acc)) (i-1) in
  match t with
  | `Int i -> go i
  | `Ident n -> `Ident n
  | `Lambda (p, t) -> `Lambda (p, church t)
  | `App (t1, t2) -> `App (church t1, church t2)

let plus =
  `Lambda (`Ident "m",
  `Lambda (`Ident "n",
  `Lambda (`Ident "f",
  `Lambda (`Ident "x",
  `App (`App (`Ident "m", `Ident "f"),
    `App ( `App (`Ident "n", `Ident "f"), `Ident "x")
  )))))

module Ctx = struct
  type t = {
    bindings: (string * value) list
  } [@@deriving sexp]

  let empty = { bindings = [] }
  let predef = { bindings = ["plus", plus] }

  let bind ctx n v = { bindings = (n, v) :: ctx.bindings }

  let lookup ctx n =
    match List.Assoc.find ~equal:(=) ctx.bindings n with
    | Some v -> v
    | None -> raise @@ Ulambda_exception (Binding n)

  let pretty_print ctx =
    sexp_of_t ctx |> Sexp.to_string_hum |> print_endline
end

let rec reduce ctx = function
| `Lambda x -> `Lambda x
| `Ident n -> Ctx.lookup ctx n
| `App (f, a) ->
    let a' = reduce ctx a in
    begin match reduce ctx f with
    | `Lambda (`Wildcard, t) -> reduce ctx t
    | `Lambda (`Ident n, t) -> reduce (Ctx.bind ctx n a') t
    end
