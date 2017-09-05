open Sexplib.Std

type pattern = [`Ident of string | `Wildcard]
[@@deriving sexp]

type term = [
  `App of term * term
| `Lambda of pattern * term
| `Ident of string
| `Int of int]
[@@deriving sexp]

type program = term
[@@deriving sexp]
