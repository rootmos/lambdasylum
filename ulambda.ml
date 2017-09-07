open Printf
open Core_kernel.Std

type error = Binding of string | ApplicationError | UnchurchError of string
exception Ulambda_exception of error

type pattern = [`Ident of string | `Wildcard]
[@@deriving sexp]

type term = [
  `App of term * term
| `Lambda of pattern * term
| `Ident of string]
[@@deriving sexp]

type value = [
  `Lambda of pattern * term
]
[@@deriving sexp]

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
  type t = {
    bindings: (string * V.t) list
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
    | `Lambda (`Wildcard, t) -> sprintf "λ_.(%s)" (go t)
    | `Lambda (`Ident n, t) -> sprintf "λ%s.(%s)" n (go t)
    | `App (t1, t2) -> sprintf "(%s) (%s)" (go t1) (go t2)
    | `Ident n -> n in
  match v with
  | `Lambda (`Wildcard, t) -> sprintf "λ_.(%s)" (go t)
  | `Lambda (`Ident n, t) -> sprintf "λ%s.(%s)" n (go t)


type term_arith = [
  `App of term_arith * term_arith
| `Lambda of pattern * term_arith
| `Ident of string
| `Succ
| `Int of int]

let unchurch_int (t: value) =
  let module Ctx_term_arith = Ctx(struct type t = term_arith end) in
  let open Ctx_term_arith in
  let t' = match t with
  | `Lambda (`Ident fi, `Lambda (`Ident x, t1)) ->
    substitute fi (`Succ) (t1 :> term_arith)
      |> substitute x (`Int 0)
  | _ -> raise @@ Ulambda_exception
           (UnchurchError "church numeral should have arity 2")
  in
  let rec go ctx = function
    | `Succ | `Int _ | `Lambda _ as t' -> t'
    | `Ident n -> (lookup ctx n :> term_arith)
    | `App (f, a) ->
        let a' = (go ctx (a :> term_arith) :> term_arith) in
        begin match go ctx f, a' with
          | `Succ, `Int i -> `Int (succ i)
          | `Lambda (`Wildcard, t), _ -> go ctx t
          | `Lambda (`Ident n, t), a' ->
              go (bind ctx n a') (substitute n (a' :> term_arith) t)
          | _ -> raise @@ Ulambda_exception
                   (UnchurchError "ill-typed application")
        end
  in
  match go empty t' with
  | `Int i -> i
  | _ -> raise @@ Ulambda_exception
           (UnchurchError "term did not reduce to a number")