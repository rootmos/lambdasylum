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
| IllTypedApplication (_, _) ->
    sprintf "ill-typed application" (* TODO: add pretty types *)
| ForcingNonThunk _ ->
    sprintf "forcing non-thunk" (* TODO: add pretty types *)

let parse s =
  try Lexing.from_string s |> Tlambda_parser.program Lexer.read with
  | Tlambda_parser.Error -> raise @@ Tlambda_exception Parsing
  | Lexer.Syntax_error msg -> raise @@ Tlambda_exception (Lexing msg)

module TyCtx = Bindings.Make(struct
  type t = ty
  let subsystem = "typechecking tlambda"
end)

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
