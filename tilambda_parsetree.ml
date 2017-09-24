type mono = [
  `Int
| `Bool
| `Fun of mono * mono
| `Thunk of mono
| `Bottom
| `TyVar of string
| `TyFun of string * mono list
]

type ty = [
  mono
| `Forall of string * ty
]

type pattern = [`Ident of string | `Wildcard]

type term = [
  `App of term * term
| `Lambda of pattern * mono option * term
| `Let of pattern * term * term
| `Att of term * mono
| `Ident of string
| `Int of int
| `Bool of bool
| `Bottom
| `Thunk of term
| `Force of term
]
