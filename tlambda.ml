open Core_kernel.Std
open Printf

type ty = Tlambda_parsetree.ty

type error =
  Lexing of string
| Parsing
| IllTypedApplication of ty * ty
| ForcingNonThunk of ty
exception Tlambda_exception of error

let explain = function
  Parsing -> sprintf "parsing error"
| Lexing s -> sprintf "lexing error: %s" s
| IllTypedApplication (_, _) -> sprintf "ill-typed application"
| ForcingNonThunk _ -> sprintf "forcing non-thunk"

let parse s =
  try Lexing.from_string s |> Tlambda_parser.program Lexer.read with
  | Tlambda_parser.Error -> raise @@ Tlambda_exception Parsing
  | Lexer.Syntax_error msg -> raise @@ Tlambda_exception (Lexing msg)

let parse_type s = Lexing.from_string s |> Tlambda_parser.ty_eof Lexer.read

module TyCtx = Bindings.Make(struct
  type t = ty
  let subsystem = "typechecking tlambda"
end)

let predef = TyCtx.(
  {
    bindings = [
      "and", parse_type "bool->bool->bool";
      "or", parse_type "bool->bool->bool";
      "+", parse_type "int->int->int";
      "-", parse_type "int->int->int";
      "*", parse_type "int->int->int";
      "zero?", parse_type "int->bool";
      "eq?", parse_type "int->int->bool";
      "leq?", parse_type "int->int->bool";
      "succ", parse_type "int->int";
      "pred", parse_type "int->int";
    ]
  }
)

let rec typecheck ~ctx = function
  `Int _ -> `Int
| `Bool _ -> `Bool
| `Ident n -> TyCtx.lookup ctx n
| `Thunk e -> `Thunk (typecheck ~ctx e)
| `Force e ->
    begin match typecheck ~ctx e with
    | `Thunk ty -> ty
    | ty -> raise @@ Tlambda_exception (ForcingNonThunk ty)
    end
| `Bottom -> `Bottom
| `Lambda (`Ident n, ty, e) ->
    let ctx = TyCtx.bind ctx n ty in `Fun (ty, typecheck ~ctx e)
| `Lambda (`Wildcard, ty, e) -> `Fun (ty, typecheck ~ctx e)
| `App (f, a) ->
    match typecheck ~ctx f, typecheck ~ctx a with
    | `Fun (a0, ty), a1 when a0 = a1 -> ty
    | t0, t1 -> raise @@ Tlambda_exception (IllTypedApplication (t0, t1))

let rec erase = function
  `Int _ | `Bool _ | `Ident _ | `Bottom as t -> t
| `Thunk t -> `Thunk (erase t)
| `Force t -> `Force (erase t)
| `Lambda (p, _, t) -> `Lambda (p, erase t)
| `App (t0, t1) -> `App (erase t0, erase t1)

let front_end s =
  let tl = parse s in
  let _ = typecheck predef tl in
  tl

let via_ulambda tl = tl
  |> erase
  |> Ulambda.church
  |> Clambda.reduce Ulambda.church_predef ~k:(fun x -> x)

let compile s = s |> front_end |> via_ulambda

let embed_into_flambda (tl: Tlambda_parsetree.term) =
  (tl :> Flambda_parsetree.term)

let compile_via_flambda s = s |> front_end |> embed_into_flambda |> Flambda.via_ulambda
