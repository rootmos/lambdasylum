type ty = [
  `Int
| `Bool
| `Fun of ty * ty
| `Thunk of ty
| `Bottom
]

type pattern = [`Ident of string | `Wildcard]

type term = [
  `App of term * term
| `Lambda of pattern * ty option * term
| `Att of term * ty
| `Ident of string
| `Int of int
| `Bool of bool
| `Bottom
| `Thunk of term
| `Force of term
]
