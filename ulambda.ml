open Core_kernel.Std
open Printf
open Substitute

type error =
  Lexing of string
| Parsing
| UnchurchError of string
exception Ulambda_exception of error

let explain = function
  Parsing -> sprintf "parsing error"
| Lexing s -> sprintf "lexing error: %s" s
| UnchurchError s -> sprintf "unchurch error: %s" s

let parse s =
  try Lexing.from_string s |> Ulambda_parser.program Lexer.read with
  | Ulambda_parser.Error -> raise @@ Ulambda_exception Parsing
  | Lexer.Syntax_error msg -> raise @@ Ulambda_exception (Lexing msg)

let rec church t =
  let open Vars in
  let rec go ?(acc=x) = function
    | 0 -> `Lambda (f, `Lambda (x, acc))
    | i -> go ~acc:(`App (f, acc)) (i-1) in
  match t with
  | `Int i -> go i
  | `Bool true -> `Lambda (x, `Lambda (y, x))
  | `Bool false -> `Lambda (x, `Lambda (y, y))
  | `Ident n -> `Ident n
  | `Lambda (p, t) -> `Lambda (p, church t)
  | `App (t1, t2) -> `App (church t1, church t2)
  | `Bottom as t' -> t'
  | `Thunk t -> `Thunk (church t)
  | `Force t -> `Force (church t)

let church_predef: Clambda.Ctx.t = {
  bindings =
    let open List in [
      "if", "λx.x";
      "and", "λp.λq.(p q) p";
      "or", "λp.λq.(p p) q";

      "+", "λm.λn.λf.λx.((m f) ((n f) x))";
      "*", "λm.λn.λf.λx.(m (n f)) x";
      "-", "λm.λn.(n pred) m";
      "zero?", "λn.n (λ_.λx.λy.y) (λx.λy.x)";
      "succ", "λn.λf.λx.f ((n f) x)";
      "pred", "λn.λf.λx.n (λg.λh.(h (g f))) (λ_.x) (λu.u)";
      "leq?", "λm.λn.zero? (- m n)";
      "eq?", "λm.λn.and (leq? m n) (leq? n m)";

      "fix", "λf.λx.f {fix f} x"
    ] >>| fun (i, s) -> i, Clambda.parse_value s
  }

type pattern = [`Ident of string | `Wildcard]

type term_arith = [
  `App of term_arith * term_arith
| `Lambda of pattern * term_arith
| `Force of term_arith
| `Thunk of term_arith
| `Ident of string
| `Succ
| `Bottom
| `Int of int
| `Bool of bool
]

type value_arith = [
| `Lambda of pattern * term_arith
| `Thunk of term_arith
| `Int of int
| `Bool of bool
]

module Ctx_term_arith = Bindings.Make(struct
  type t = term_arith
  let subsystem = "reducing Chruch-encoded ulambda term"
end)

open Result
open Let_syntax

let rec reduce_church ctx = function
| `Succ | `Int _ | `Bool _ | `Lambda _ as t' -> return t'
| `Bottom -> fail "reached bottom"
| `Thunk _ | `Force _ -> fail "encountered thunk (ill-formed church numeral)"
| `Ident n -> return @@ Ctx_term_arith.lookup ctx n
| `App (f, a) ->
    let%bind f' = reduce_church ctx f
    and a' = reduce_church ctx a in
    match f', a' with
      | `Succ, `Int i -> return @@ `Int (succ i)
      | `Bottom, _ -> fail "reached bottom"
      | `Lambda (`Wildcard, t), _ -> reduce_church ctx t
      | `Lambda (`Ident n, t), a' ->
          reduce_church (Ctx_term_arith.bind ctx n a') (substitute n a' t)
      | _ -> fail "ill-typed application"

let unchurch_int (t: Clambda.value) =
  let call_with_succ = match t with
    | `Lambda (`Ident fi, `Lambda (`Ident x, t1)) -> return (
      substitute fi (`Succ) (t1 :> term_arith) |> substitute x (`Int 0)
    )
    | _ -> fail "church numeral should be a lambda of arity 2" in
  let assume_int = function
    | `Int i -> return i
    | _ -> fail "term did not reduce to a number" in
  call_with_succ >>= reduce_church Ctx_term_arith.empty >>= assume_int

let unchurch_bool (t: Clambda.value) =
  let call_with_true_and_false = match t with
    | `Lambda (`Ident t, `Lambda (`Ident f, t1)) -> return (
      substitute t (`Bool true) (t1 :> term_arith)
        |> substitute f (`Bool false)
    )
    | _ -> fail "church numeral should be a lambda of arity 2" in
  let assume_bool = function
    | `Bool b -> return b
    | _ -> fail "term did not reduce to a boolean" in
  call_with_true_and_false
    >>= reduce_church Ctx_term_arith.empty
    >>= assume_bool

let unchurch (t: Clambda.value) =
  let i = unchurch_int t and b = unchurch_bool t in
  match b, i with
  | Ok b, _ -> `Bool b
  | _, Ok i -> `Int i
  | _ -> (t :> value_arith)

let ok_exn r = r
    |> map_error ~f:(fun r -> Ulambda_exception (UnchurchError r))
    |> ok_exn

let rec pretty_term = function
| `Ident n -> n, 0
| `Bottom -> "⊥", 0
| `Int i -> string_of_int i, 0
| `Bool true -> "#t", 0
| `Bool false -> "#f", 0
| `Succ -> "succ", 0
| `Thunk t -> let p, _ = pretty_term t in sprintf "{%s}" p, 0
| `Force t -> let p, size = pretty_term t in sprintf "%s!" p, succ size
| `Lambda (`Wildcard, t) ->
    let p, size = pretty_term t in sprintf "λ_.%s" p, succ size
| `Lambda (`Ident n, t) ->
    let p, size = pretty_term t in sprintf "λ%s.%s" n p, succ size
| `App (t1, t2) ->
    let p1, s1 = pretty_term t1
    and p2, s2 = pretty_term t2 in
    let parenthesize p = function 0 -> p | _ -> sprintf "(%s)" p in
    sprintf "%s %s" (parenthesize p1 s1) (parenthesize p2 s2),
      s1 + s2 + 1

let pretty v =
  match v with
  | `Lambda (`Wildcard, t) -> sprintf "λ_.%s" (pretty_term t |> fst)
  | `Lambda (`Ident n, t) -> sprintf "λ%s.%s" n (pretty_term t |> fst)
  | `Thunk t -> sprintf "{%s}" (pretty_term t |> fst)
  | `Int i -> string_of_int i
  | `Bool true -> "#t"
  | `Bool false -> "#f"

let compile s = s
    |> parse
    |> church
    |> Clambda.reduce church_predef ~k:(fun x -> x)
