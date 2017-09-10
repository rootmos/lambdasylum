open Sexplib.Std

type ty = [
  `Int
| `Bool
| `Fun of ty * ty
| `Thunk of ty
| `Bottom
| `TyIdent of string
| `Forall of string * ty
]
[@@deriving sexp]

type pattern = [`Ident of string | `Wildcard]
[@@deriving sexp]

type term = [
  `App of term * term
| `TyApp of term * ty
| `Lambda of pattern * ty * term
| `TyLambda of string * term
| `Ident of string
| `Int of int
| `Bool of bool
| `Bottom
| `Thunk of term
| `Force of term
]
[@@deriving sexp]
