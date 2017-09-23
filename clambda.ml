open Core_kernel.Std
open Substitute
open Printf
include Clambda_parsetree

type error =
  Lexing of string
| Parsing
| ReachedBottom
| IllScopedIdentifier of string
exception Clambda_exception of error

let explain = function
  Parsing -> sprintf "parsing error"
| Lexing s -> sprintf "lexing error: %s" s
| ReachedBottom -> "code raised runtime error"
| IllScopedIdentifier n -> sprintf "ill-scoped identifier: %s" n

let parse s =
  try Lexing.from_string s |> Clambda_parser.program Lexer.read with
  | Clambda_parser.Error -> raise @@ Clambda_exception Parsing
  | Lexer.Syntax_error msg -> raise @@ Clambda_exception (Lexing msg)

let parse_value s =
  try Lexing.from_string s |> Clambda_parser.value Lexer.read with
  | Clambda_parser.Error -> raise @@ Clambda_exception Parsing
  | Lexer.Syntax_error msg -> raise @@ Clambda_exception (Lexing msg)

module Ctx = Bindings.Make(struct
  type t = value
  let subsystem = "reducing clambda"
end)

let reduce ctx t =
  let rec go ~ctx ~k = function
    | `Lambda _ | `Thunk _ as t -> k t
    | `Ident n -> k @@ Ctx.lookup_exn ctx n
    | `Bottom -> raise @@ Clambda_exception ReachedBottom
    | `Force t ->
        go ~ctx t ~k:(function
          | `Thunk t -> go ~ctx ~k t
          | t -> k t)
    | `App (f, a) ->
        go ~ctx a ~k:(fun a' ->
          go ~ctx f ~k:(fun f' ->
            let rec l = function
              | `Thunk t -> go ~ctx ~k:l t
              | `Lambda (`Wildcard, t) -> go ~ctx ~k t
              | `Lambda (`Ident n, t) ->
                  let ctx = Ctx.bind ctx n a'
                  and t' = substitute n (a' :> term) t
                  in go ~ctx ~k t'
            in l f'))
  in go ~ctx ~k:(fun x -> x) t

let rec de_brujin_term ~ctx = function
| `App (t0, t1) -> `App (de_brujin_term ~ctx t0, de_brujin_term ~ctx t1)
| `Bottom -> `Bottom
| `Thunk t -> `Thunk (de_brujin_term ~ctx t)
| `Force t -> `Force (de_brujin_term ~ctx t)
| `Ident n ->
    begin match List.find_mapi ctx ~f:(fun i -> function
      | n' when n = n -> Some i
      | _ -> None
    ) with
    | Some i -> `Ident i
    | None -> raise @@ Clambda_exception (IllScopedIdentifier n)
    end
| `Lambda (`Wildcard, t) ->
    let ctx = "_" :: ctx in `Lambda (`Wildcard, de_brujin_term ~ctx t)
| `Lambda (`Ident n, t) ->
    let ctx = n :: ctx in `Lambda (`Wildcard, de_brujin_term ~ctx t)

let rec de_brujin = function
| `Lambda (`Wildcard, t) ->
    let ctx = ["_"] in `Lambda (`Wildcard, de_brujin_term ~ctx t)
| `Lambda (`Ident n, t) ->
    let ctx = [n] in `Lambda (`Wildcard, de_brujin_term ~ctx t)
| `Thunk t ->
    let ctx = [] in `Thunk (de_brujin_term ~ctx t)

let alpha_equivalent v0 v1 =
  (de_brujin v0) = (de_brujin v1)

let compile s = s
  |> parse
  |> reduce Ctx.empty
