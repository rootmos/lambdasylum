open Printf
open Core_kernel.Std

type error =
  Binding of string
| ApplicationError
| UnchurchError of string
| ReachedBottom
exception Ulambda_exception of error

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

module Vars = struct
  let f = `Ident "f"
  let g = `Ident "g"
  let h = `Ident "h"
  let m = `Ident "m"
  let n = `Ident "n"
  let p = `Ident "p"
  let q = `Ident "q"
  let u = `Ident "u"
  let x = `Ident "x"
  let y = `Ident "y"
end

let rec church t =
  let open Vars in
  let rec go ?(acc=x) = function
    | 0 -> `Lambda (f, `Lambda (x, acc))
    | i -> go ~acc:(`App (f, acc)) (i-1) in
  match t with
  | `Int i -> go i
  | `Ident n -> `Ident n
  | `Lambda (p, t) -> `Lambda (p, church t)
  | `App (t1, t2) -> `App (church t1, church t2)
  | `Bottom as t' -> t'
  | `Thunk t -> `Thunk (church t)
  | `Force t -> `Force (church t)

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
| `Thunk t' -> `Thunk (substitute n t t')
| `Force t' -> `Force (substitute n t t')
| `App (t1, t2) -> `App (substitute n t t1, substitute n t t2)
| _ as t' -> t'

module Ctx_term = Ctx(struct type t = value end)

let predef: Ctx_term.t = {
  bindings =
    let open Vars in [
      "if", `Lambda (x, x);
      "true", `Lambda (x, `Lambda (y, x));
      "false", `Lambda (x, `Lambda (y, y));
      "and", `Lambda (p, `Lambda (q, `App (`App (p, q), p)));
      "or", `Lambda (p, `Lambda (q, `App (`App (p, p), q)));

      "+", `Lambda (m, `Lambda (n, `Lambda (f, `Lambda (x,
        `App (`App (m, f), `App (`App (n, f), x))))));
      "*", `Lambda (m, `Lambda (n, `Lambda (f, `Lambda (x,
        `App(`App (m, `App (n, f)), x)))));
      "-", `Lambda (m, `Lambda (n,
        (`App (`App (n, `Ident "pred"), m))));
      "zero?", `Lambda (n,
        `App (`App (n, (`Lambda (`Wildcard, `Ident "false"))), `Ident "true"));
      "succ", `Lambda (n, `Lambda (f, `Lambda (x,
        `App (f, `App (`App (n, f), x)))));
      "pred", `Lambda (n, `Lambda (f, `Lambda (x,
        `App (
          `App (
            `App (n, `Lambda (g, `Lambda (h, `App (h, `App (g, f))))),
            `Lambda (`Wildcard, x)
          ),
          `Lambda (u, u)))));
      "leq?", `Lambda (m, `Lambda (n, `App (`Ident "zero?",
        `App (`App (`Ident "-", m), n))));
      "eq?", `Lambda (m, `Lambda (n,
        `App (`App (`Ident "and", `App (`App (`Ident "leq?", m), n)),
          `App (`App (`Ident "leq?", n), m))));

      "fix", `Lambda (f, `Lambda (x,
        `App (`App (f, `Thunk (`App (`Ident "fix", f))), x)))
    ]
  }

let rec pretty_term = function
| `Lambda (`Wildcard, t) -> sprintf "λ_.%s" (pretty_term t)
| `Lambda (`Ident n, t) -> sprintf "λ%s.%s" n (pretty_term t)
| `App (t1, t2) -> sprintf "(%s) (%s)" (pretty_term t1) (pretty_term t2)
| `Ident n -> n
| `Thunk t -> sprintf "{%s}" (pretty_term t)
| `Force t -> sprintf "%s!" (pretty_term t)
| `Bottom -> "⊥"

let pretty v =
  match v with
  | `Lambda (`Wildcard, t) -> sprintf "λ_.%s" (pretty_term t)
  | `Lambda (`Ident n, t) -> sprintf "λ%s.%s" n (pretty_term t)
  | `Thunk t -> sprintf "{%s}" (pretty_term t)

let rec reduce ctx ~k t =
  (*pretty_term t |> print_endline;*)
  match t with
| `Lambda _ | `Thunk _ as t -> k t
| `Ident n -> k @@ Ctx_term.lookup ctx n
| `Bottom -> raise @@ Ulambda_exception ReachedBottom
| `Force t ->
    reduce ctx t ~k:(function
      | `Thunk t -> reduce ctx ~k t
      | t -> k t)
| `App (f, a) ->
    reduce ctx a ~k:(fun a' ->
      reduce ctx f ~k:(fun f' ->
        let rec l = function
          | `Thunk t -> reduce ctx ~k:l t
          | `Lambda (`Wildcard, t) -> reduce ctx ~k t
          | `Lambda (`Ident n, t) ->
              let ctx' = Ctx_term.bind ctx n a'
              and t' = substitute n (a' :> term) t in
              reduce ctx' ~k t' in
        l f'))



type term_arith = [
  `App of term_arith * term_arith
| `Lambda of pattern * term_arith
| `Force of term_arith
| `Thunk of term_arith
| `Ident of string
| `Succ
| `Bottom
| `Int of int]

module Ctx_term_arith = Ctx(struct type t = term_arith end)

let rec reduce_church ctx = function
| `Succ | `Int _ | `Lambda _ as t' -> t'
| `Bottom -> raise @@ Ulambda_exception ReachedBottom
| `Thunk _ | `Force _ -> raise @@ Ulambda_exception
  (UnchurchError "encountered thunk (ill-formed church numeral")
| `Ident n -> Ctx_term_arith.lookup ctx n
| `App (f, a) ->
    match reduce_church ctx f, reduce_church ctx a with
      | `Succ, `Int i -> `Int (succ i)
      | `Bottom, _ -> raise @@ Ulambda_exception ReachedBottom
      | `Lambda (`Wildcard, t), _ -> reduce_church ctx t
      | `Lambda (`Ident n, t), a' ->
          reduce_church (Ctx_term_arith.bind ctx n a') (substitute n a' t)
      | _ -> raise @@ Ulambda_exception (UnchurchError "ill-typed application")

let unchurch_int (t: value) =
  let t' = match t with
  | `Lambda (`Ident fi, `Lambda (`Ident x, t1)) ->
    substitute fi (`Succ) (t1 :> term_arith) |> substitute x (`Int 0)
  | _ -> raise @@ Ulambda_exception
           (UnchurchError "church numeral should be a lambda of arity 2")
  in
  match reduce_church Ctx_term_arith.empty t' with
  | `Int i -> i
  | _ -> raise @@ Ulambda_exception
           (UnchurchError "term did not reduce to a number")
