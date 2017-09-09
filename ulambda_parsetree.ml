type pattern = [`Ident of string | `Wildcard]

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
