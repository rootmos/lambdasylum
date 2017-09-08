open Core_kernel.Std
open Sexplib.Std

type pattern = [`Ident of string | `Wildcard]
[@@deriving sexp]

type term = [
  `App of term * term
| `Lambda of pattern * term
| `Ident of string
| `Int of int
| `Bottom
| `Thunk of term
| `Force of term
]
[@@deriving sexp]

type program = term
[@@deriving sexp]

let pretty p = sexp_of_program p |> Sexp.to_string_hum |> print_endline
