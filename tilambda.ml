open Core_kernel.Std
open Printf

type mono = [
  `Int
| `Bool
| `Fun of mono * mono
| `Thunk of mono
| `Bottom
| `TyVar of string
| `TyFun of string * mono list
]

type ty = [
  mono
| `Forall of string * ty
]

type pattern = [`Ident of string | `Wildcard]

type term = [
  `App of term * term * mono
| `Lambda of pattern * mono * term
| `Att of term * mono
| `Ident of string
| `Int of int
| `Bool of bool
| `Bottom
| `Thunk of term
| `Force of term * mono
]

module FreshTyVar = Fresh.Gen(struct
  type t = mono
  let mk i = `TyVar (sprintf "_%d" i)
end)

type error =
  Lexing of string
| Parsing
| IllTypedApplication of ty * ty
| ForcingNonThunk of ty
| Unification_failed
exception Tilambda_exception of error

let explain = function
  Parsing -> "parsing error"
| Lexing s -> sprintf "lexing error: %s" s
| IllTypedApplication (_, _) -> "ill-typed application"
| ForcingNonThunk _ -> "forcing non-thunk"
| Unification_failed -> "unification failed"

let parse s =
  try Lexing.from_string s |> Tilambda_parser.program Lexer.read with
  | Tilambda_parser.Error -> raise @@ Tilambda_exception Parsing
  | Lexer.Syntax_error msg -> raise @@ Tilambda_exception (Lexing msg)

let parse_type s =
  Lexing.from_string s |> Tilambda_parser.ty_eof Lexer.read

module TyCtx = Bindings.Make(struct
  type t = ty
  let subsystem = "typechecking tilambda"
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

      "if", parse_type "∀T.bool->T->T->T";
      "nil", parse_type "∀T.List T";
      "nil?", parse_type "∀T.(List T)->bool";
      "cons", parse_type "∀T.T->(List T)->(List T)";
      "head", parse_type "∀T.(List T)->T";
      "tail", parse_type "∀T.(List T)->List T";
    ]
  }
)

let rec sub_ty i t: mono -> mono = function
  | `TyVar j when i = j -> t
  | `Thunk ty -> `Thunk (sub_ty i t ty)
  | `Fun (ty0, ty1) -> `Fun (sub_ty i t ty0, sub_ty i t ty1)
  | `TyFun (f, args) -> `TyFun (f, List.(args >>| sub_ty i t))
  | s -> s

let rec sub_ty' i (t: mono): ty -> ty = function
  | `Forall (j, ty) when i <> j -> `Forall (j, sub_ty' i t ty)
  | `TyVar j when i = j -> (t :> ty)
  | `Thunk ty -> `Thunk (sub_ty i t ty)
  | `Fun (ty0, ty1) -> `Fun (sub_ty i t ty0, sub_ty i t ty1)
  | `TyFun (f, args) -> `TyFun (f, List.(args >>| sub_ty i t))
  | s -> s

let rec inst: ty -> mono = function
  | `Forall (i, ty) ->
      let tv = FreshTyVar.next () in
      inst (sub_ty' i tv ty)
  | `Int | `Bool | `Fun _ | `Thunk _ | `Bottom | `TyVar _
  | `TyFun _ as ty -> ty

let rec introduce_tyvars: Tilambda_parsetree.term -> term  = function
  | `Lambda (p, None, t) ->
      `Lambda (p, FreshTyVar.next (), introduce_tyvars t)
  | `Lambda (p, Some ty, t) ->
      `Lambda (p, inst ty, introduce_tyvars t)
  | `App (t0, t1) ->
      let ty = FreshTyVar.next () in
      `App (introduce_tyvars t0, introduce_tyvars t1, ty)
  | `Att (t, ty) -> `Att (introduce_tyvars t, inst ty)
  | `Thunk t -> `Thunk (introduce_tyvars t)
  | `Force t -> `Force (introduce_tyvars t, FreshTyVar.next ())
  | `Ident _ | `Int _ | `Bool _ | `Bottom as t -> t

let derive_constraints t =
  let rec go ~ctx cs = function
    | `Int _ -> `Int, cs
    | `Bool _ -> `Bool, cs
    | `Bottom -> `Bottom, cs
    | `Ident n -> TyCtx.lookup ctx n |> inst, cs
    | `Thunk t ->
        let ty, cs' = go ~ctx cs t in `Thunk ty, cs'
    | `Att (t, ty) ->
        let ty', cs' = go ~ctx cs t in
        ty, (ty, ty') :: cs'
    | `Force (t, ty) ->
        let ty', cs' = go ~ctx cs t in
        ty, (`Thunk ty, ty') :: cs'
    | `Lambda (`Ident n, ty0, t) ->
        let ctx = TyCtx.bind ctx n (ty0 :> ty) in
        let ty1, cs' = go ~ctx cs t in
        `Fun (ty0, ty1), cs'
    | `Lambda (`Wildcard, ty0, t) ->
        let ty1, cs' = go ~ctx cs t in
        `Fun (ty0, ty1), cs'
    | `App (t0, t1, ty) ->
        let ty0, cs' = go ~ctx cs t0 in
        let ty1, cs'' = go ~ctx cs' t1 in
        ty, (ty0, `Fun (ty1, ty)) :: cs''
  in go ~ctx:predef [] t |> snd

let rec occurs i = function
  | `TyVar j when i = j -> true
  | `Thunk ty -> occurs i ty
  | `Fun (ty0, ty1) -> occurs i ty0 || occurs i ty1
  | `TyFun (_, args) -> List.exists ~f:(occurs i) args
  | _ -> false

let rec unify = function
  | [] -> (fun x -> x)
  | (s, t) :: cs when s = t -> unify cs
  | (`TyVar i, t) :: cs when not (occurs i t) ->
      let cs' = List.(cs >>| fun (a, b) -> sub_ty i t a, sub_ty i t b) in
      Fn.compose (unify cs') (sub_ty i t)
  | (s, `TyVar j) :: cs when not (occurs j s) ->
      let cs' = List.(cs >>| fun (a, b) -> sub_ty j s a, sub_ty j s b) in
      Fn.compose (unify cs') (sub_ty j s)
  | (`Fun (s0, s1), `Fun (t0, t1)) :: cs ->
      unify @@ (s0, t0) :: (s1, t1) :: cs
  | (`TyFun (f, fa), `TyFun (g, ga)) :: cs when f = g ->
      begin match List.zip fa ga with
      | Some cs' -> unify (List.append cs' cs)
      | None -> raise @@ Tilambda_exception Unification_failed
      end
  | (`Bottom, _) :: cs -> unify cs
  | (_, `Bottom) :: cs -> unify cs
  | _ -> raise @@ Tilambda_exception Unification_failed

let sub_ty_in_term sub t =
  let rec go = function
    | `App (t0, t1, ty) -> `App (go t0, go t1, sub ty)
    | `Lambda (p, ty, t) -> `Lambda (p, sub ty, go t)
    | `Att (t, ty) -> `Att (go t, sub ty)
    | `Force (t, ty) -> `Force (go t, sub ty)
    | `Thunk t -> `Thunk (go t)
    | t -> t
  in go t

let rec erase = function
  `Int _ | `Bool _ | `Ident _ | `Bottom as t -> t
| `Att (t, _) -> erase t
| `Thunk t -> `Thunk (erase t)
| `Force (t, _) -> `Force (erase t)
| `Lambda (p, _, t) -> `Lambda (p, erase t)
| `App (t0, t1, _) -> `App (erase t0, erase t1)

let front_end s =  s
  |> parse
  |> introduce_tyvars
  |> fun tl -> sub_ty_in_term (derive_constraints tl |> unify) tl

let via_ulambda tl = tl
  |> erase
  |> Ulambda.church
  |> Clambda.reduce Ulambda.church_predef ~k:(fun x -> x)

let compile s = s |> front_end |> via_ulambda
