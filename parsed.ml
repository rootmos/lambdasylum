open Sexplib.Std

type pattern = [`Ident of string | `Wildcard]
[@@deriving sexp]

type term = [`Lambda of pattern * term | `Ident of string]
[@@deriving sexp]

type value = [`Lambda of pattern * term]
[@@deriving sexp]

type program = [`Lambda of pattern * term]
[@@deriving sexp]
