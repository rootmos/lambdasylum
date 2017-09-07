open Printf
open Core_kernel.Std

type error = Binding of string | ApplicationError | UnchurchError of string
exception Ulambda_exception of error

type pattern = [`Ident of string | `Wildcard]

type term = [
  `App of term * term
| `Lambda of pattern * term
| `Ident of string]

type value = [
  `Lambda of pattern * term
]

let rec church t =
  let plus =
    `Lambda (`Ident "m",
  `Lambda (`Ident "n",
  `Lambda (`Ident "f",
  `Lambda (`Ident "x",
  `App (`App (`Ident "m", `Ident "f"),
    `App ( `App (`Ident "n", `Ident "f"), `Ident "x")
    ))))) in
  let rec go ?(acc=`Ident "x") = function
    | 0 -> `Lambda (`Ident "f", `Lambda (`Ident "x", acc))
    | i -> go ~acc:(`App (`Ident "f", acc)) (i-1) in
  match t with
  | `Int i -> go i
  | `Ident "+" -> plus
  | `Ident n -> `Ident n
  | `Lambda (p, t) -> `Lambda (p, church t)
  | `App (t1, t2) -> `App (church t1, church t2)

module type Value = sig
  type t
end

module Ctx(V: Value) = struct
  type bindings = (string * V.t) list
  type t = {
    bindings: bindings
  }

  let empty = { bindings = [] }

  let bind ctx n v = { bindings = (n, v) :: ctx.bindings }

  let lookup ctx n =
    match List.Assoc.find ~equal:(=) ctx.bindings n with
    | Some v -> v
    | None -> raise @@ Ulambda_exception (Binding n)
end

let rec substitute n (t: 'a) (t0: 'a): 'a = match t0 with
  `Ident n' when n = n' -> (t :> 'a)
| (`Lambda (`Ident n', _) as t') when n = n' -> t'
| `Lambda (p, t') -> `Lambda (p, substitute n t t')
| `App (t1, t2) -> `App (substitute n t t1, substitute n t t2)
| _ as t' -> t'

module Ctx_term = Ctx(struct type t = value end)

let predef: Ctx_term.t = {
  bindings = [
    "if", `Lambda (`Ident "x", `Ident "x");
    "true", `Lambda (`Ident "x", `Lambda (`Ident "y", `Ident "x"));
    "false", `Lambda (`Ident "x", `Lambda (`Ident "y", `Ident "y"));
  ]
  }

let rec reduce ctx = function
| `Lambda x -> `Lambda x
| `Ident n -> Ctx_term.lookup ctx n
| `App (f, a) ->
    let a' = reduce ctx a in
    begin match reduce ctx f with
    | `Lambda (`Wildcard, t) -> reduce ctx t
    | `Lambda (`Ident n, t) ->
        reduce (Ctx_term.bind ctx n a') (substitute n (a' :> term) t)
    end

let pretty v =
  let rec go = function
    | `Lambda (`Wildcard, t) -> sprintf "λ_.%s" (go t)
    | `Lambda (`Ident n, t) -> sprintf "λ%s.%s" n (go t)
    | `App (t1, t2) -> sprintf "(%s) (%s)" (go t1) (go t2)
    | `Ident n -> n in
  match v with
  | `Lambda (`Wildcard, t) -> sprintf "λ_.%s" (go t)
  | `Lambda (`Ident n, t) -> sprintf "λ%s.%s" n (go t)


type term_arith = [
  `App of term_arith * term_arith
| `Lambda of pattern * term_arith
| `Ident of string
| `Succ
| `Int of int]

module Ctx_term_arith = Ctx(struct type t = term_arith end)

let rec reduce_church ctx = function
| `Succ | `Int _ | `Lambda _ as t' -> t'
| `Ident n -> Ctx_term_arith.lookup ctx n
| `App (f, a) ->
    match reduce_church ctx f, reduce_church ctx a with
      | `Succ, `Int i -> `Int (succ i)
      | `Lambda (`Wildcard, t), _ -> reduce_church ctx t
      | `Lambda (`Ident n, t), a' ->
          reduce_church (Ctx_term_arith.bind ctx n a') (substitute n a' t)
      | _ -> raise @@ Ulambda_exception (UnchurchError "ill-typed application")

let unchurch_int (t: value) =
  let t' = match t with
  | `Lambda (`Ident fi, `Lambda (`Ident x, t1)) ->
    substitute fi (`Succ) (t1 :> term_arith) |> substitute x (`Int 0)
  | _ -> raise @@ Ulambda_exception
           (UnchurchError "church numeral should have arity 2")
  in
  match reduce_church Ctx_term_arith.empty t' with
  | `Int i -> i
  | _ -> raise @@ Ulambda_exception
           (UnchurchError "term did not reduce to a number")
