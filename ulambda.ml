open Core_kernel.Std
open Printf

type error =
  Lexing of string
| Parsing
| Binding of string
| ApplicationError
| UnchurchError of string
| ReachedBottom
exception Ulambda_exception of error

let explain = function
  Parsing -> sprintf "parsing error"
| Lexing s -> sprintf "lexing error: %s" s
| Binding n -> sprintf "ill-scoped identifier: %s" n
| ApplicationError -> "trying to apply non-lambda"
| UnchurchError s -> sprintf "unchurch error: %s" s
| ReachedBottom -> "code raised runtime error"

let parse s =
  try Lexing.from_string s |> Ulambda_parser.program Lexer.read with
  | Ulambda_parser.Error -> raise @@ Ulambda_exception Parsing
  | Lexer.Syntax_error msg -> raise @@ Ulambda_exception (Lexing msg)

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

let rec substitute n (t: 'a) (t0: 'a): 'a = match t0 with
  `Ident n' when n = n' -> (t :> 'a)
| (`Lambda (`Ident n', _) as t') when n = n' -> t'
| `Lambda (p, t') -> `Lambda (p, substitute n t t')
| `Thunk t' -> `Thunk (substitute n t t')
| `Force t' -> `Force (substitute n t t')
| `App (t1, t2) -> `App (substitute n t t1, substitute n t t2)
| _ as t' -> t'

module Ctx_term = Bindings.Make(struct
  type t = value
  let subsystem = "reducing ulambda"
end)

let predef: Ctx_term.t = {
  bindings =
    let open Vars in [
      "if", `Lambda (x, x);
      "and", `Lambda (p, `Lambda (q, `App (`App (p, q), p)));
      "or", `Lambda (p, `Lambda (q, `App (`App (p, p), q)));

      "+", `Lambda (m, `Lambda (n, `Lambda (f, `Lambda (x,
        `App (`App (m, f), `App (`App (n, f), x))))));
      "*", `Lambda (m, `Lambda (n, `Lambda (f, `Lambda (x,
        `App(`App (m, `App (n, f)), x)))));
      "-", `Lambda (m, `Lambda (n,
        (`App (`App (n, `Ident "pred"), m))));
      "zero?", `Lambda (n,
        `App (
          `App (n, (`Lambda (`Wildcard, `Lambda (x, `Lambda (y, y))))),
          `Lambda (x, `Lambda (y, x))));
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


module Church = struct

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

  let unchurch_int (t: value) =
    let call_with_succ = match t with
      | `Lambda (`Ident fi, `Lambda (`Ident x, t1)) -> return (
        substitute fi (`Succ) (t1 :> term_arith) |> substitute x (`Int 0)
      )
      | _ -> fail "church numeral should be a lambda of arity 2" in
    let assume_int = function
      | `Int i -> return i
      | _ -> fail "term did not reduce to a number" in
    call_with_succ >>= reduce_church Ctx_term_arith.empty >>= assume_int

  let unchurch_bool (t: value) =
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

  let unchurch (t: value) =
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

end

let compile s = s |> parse |> church |> reduce predef ~k:(fun x -> x)
