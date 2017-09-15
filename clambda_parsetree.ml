type pattern = [`Ident of string | `Wildcard]

type term = [
  `App of term * term
| `Lambda of pattern * term
| `Ident of string
| `Bottom
| `Thunk of term
| `Force of term
]

type value = [
  `Lambda of pattern * term
| `Thunk of term
]
