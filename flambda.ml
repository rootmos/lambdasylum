open Core_kernel.Std
open Printf

type ty = Flambda_parsetree.ty

type error =
  Lexing of string
| Parsing
| IllTypedApplication of ty * ty
| ForcingNonThunk of ty
| IllTypedTypeApplication
| IllScopedType of string
exception Flambda_exception of error

let parse s =
  try Lexing.from_string s |> Flambda_parser.program Lexer.read with
  | Flambda_parser.Error -> raise @@ Flambda_exception Parsing
  | Lexer.Syntax_error msg -> raise @@ Flambda_exception (Lexing msg)

let parse_type s = Lexing.from_string s |> Flambda_parser.ty_eof Lexer.read

let explain = function
  Parsing -> sprintf "parsing error"
| Lexing s -> sprintf "lexing error: %s" s
| IllTypedApplication (_, _) -> sprintf "ill-typed application"
| ForcingNonThunk _ -> sprintf "forcing non-thunk"
| IllTypedTypeApplication -> sprintf "ill-typed application"
| IllScopedType n -> sprintf "ill-scoped type %s" n

module TyCtx = Bindings.Make(struct
  type t = ty
  let subsystem = "typechecking flambda"

end)

let predef = TyCtx.(
  {
    bindings = [
      "if", parse_type "∀T.bool->T->T->T"
    ]
  }
)

let rec substitute i ty = function
  `Fun (t0, t1) -> `Fun (substitute i ty t0, substitute i ty t1)
| `Thunk t -> `Thunk (substitute i ty t)
| `TyIdent i' when i = i' -> ty
| `Forall (i', t) when i <> i' -> `Forall (i', substitute i ty t)
| t -> t

let rec de_brujin_type ?(ctx=[]) = function
  `Int | `Bool | `Bottom as t -> t
| `Fun (t0, t1) -> `Fun (de_brujin_type ~ctx t0, de_brujin_type ~ctx t1)
| `Thunk t -> `Thunk (de_brujin_type ~ctx t)
| `TyIdent n ->
    begin match List.find_mapi ctx ~f:(fun i -> function
      | n' when n = n -> Some i
      | _ -> None
    ) with
    | Some i -> `TyIdent i
    | None -> raise @@ Flambda_exception (IllScopedType n)
    end
| `Forall (n, t) ->
    let ctx = n :: ctx in `Forall (de_brujin_type ~ctx t)

let rec subtype t s =
  match t, s with
  | `Bottom, _ -> true
  | `Thunk t', `Thunk s' -> subtype t' s'
  | _, _ when t = s -> true
  | _ -> false

let rec typecheck ~ctx = function
  `Int _ -> `Int
| `Bool _ -> `Bool
| `TyLambda (i, t) ->
    let ty = typecheck ~ctx t in
    `Forall (i, ty)
| `TyApp (t, ty) ->
    begin match typecheck ~ctx t with
    | `Forall (i, t') -> substitute i ty t'
    | _ -> raise @@ Flambda_exception IllTypedTypeApplication
    end
| `Ident n -> TyCtx.lookup ctx n
| `Thunk e -> `Thunk (typecheck ~ctx e)
| `Force e ->
    begin match typecheck ~ctx e with
    | `Thunk ty -> ty
    | ty -> raise @@ Flambda_exception (ForcingNonThunk ty)
    end
| `Bottom -> `Bottom
| `Lambda (`Ident n, ty, e) ->
    let ctx = TyCtx.bind ctx n ty in `Fun (ty, typecheck ~ctx e)
| `Lambda (`Wildcard, ty, e) -> `Fun (ty, typecheck ~ctx e)
| `App (f, a) ->
    match typecheck ~ctx f, typecheck ~ctx a with
    | `Fun (a0, ty), a1 when
      subtype (de_brujin_type a1) (de_brujin_type a0) -> ty
    | t0, t1 -> raise @@ Flambda_exception (IllTypedApplication (t0, t1))

let rec erase = function
  `TyLambda (_, t) -> `Thunk (erase t)
| `TyApp (t, _) -> `Force (erase t)
| `App (t0, t1) -> `App (erase t0, erase t1)
| `Lambda (p, _, t) -> `Lambda (p, erase t)
| `Thunk t -> `Thunk (erase t)
| `Force t -> `Force (erase t)
| `Int _ | `Bool _ | `Bottom | `Ident _ as t -> t

let compile s =
  let fl = parse s in
  let _ = typecheck predef fl in
  erase fl
  |> Ulambda.church
  |> Ulambda.reduce Ulambda.predef ~k:(fun x -> x)