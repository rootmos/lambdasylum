open Sexplib.Std

type ty = [`Int | `Bool | `Fun of ty * ty]
[@@deriving sexp]

type pattern = [`Ident of string * ty | `Wildcard of ty]
[@@deriving sexp]

type term = [
  `App of term * term
| `Lambda of pattern * term
| `Ident of string
| `Int of int
| `Bool of bool
| `Bottom
| `Thunk of term
| `Force of term
]
[@@deriving sexp]
