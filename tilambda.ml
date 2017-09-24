open Core_kernel.Std
open Printf

type mono = [
  `Int
| `Bool
| `Bottom
| `Fun of mono * mono
| `Thunk of mono
| `TyVar of string
| `TyFun of string * mono list
]
[@@deriving sexp]

type ty = [
  mono
| `Forall of string * ty
]
[@@deriving sexp]

let pretty_type (ty: ty) =
  let parenthesize s = function 0 -> s | _ -> sprintf "(%s)" s in
  let rec go: ty -> string * int = function
  | `Int -> "int", 0
  | `Bool -> "bool", 0
  | `Fun (t0, t1) ->
      let s0, p0 = go (t0 :> ty) and s1, p1 = go (t1 :> ty) in
      sprintf "%s->%s" (parenthesize s0 p0) s1, p0 + p1 + 1
  | `Thunk t -> sprintf "{%s}" (go (t :> ty) |> fst), 0
  | `Bottom -> "⊥", 0
  | `TyVar n -> n, 0
  | `Forall (n, ty) ->
      let s0, p0 = go ty in
      sprintf "∀%s.%s" n s0, p0+1
  | `TyFun (n, args) ->
      List.( args
        >>| (fun s -> let s, p = go (s :> ty) in parenthesize s p)
        |> cons n
        |> intersperse ~sep:" "
        |> String.concat, length args
      )
  in go ty |> fst

type pattern = [`Ident of string | `Wildcard]

type term = [
  `App of term * term * mono
| `Lambda of pattern * mono * term
| `Let of pattern * term * term
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

let introduce_tyvars (t: Tilambda_parsetree.term): term =
  let rec go = function
    | `Lambda (p, None, t) -> `Lambda (p, FreshTyVar.next (), go t)
    | `Lambda (p, Some ty, t) -> `Lambda (p, ty, go t)
    | `Let (p, e, b) -> `Let (p, go e, go b)
    | `App (t0, t1) -> let ty = FreshTyVar.next () in `App (go t0, go t1, ty)
    | `Att (t, ty) -> `Att (go t, ty)
    | `Thunk t -> `Thunk (go t)
    | `Force t -> `Force (go t, FreshTyVar.next ())
    | `Ident _ | `Int _ | `Bool _ | `Bottom as t -> t
  in go t

module Acc = struct
  include Base.Monad.Make(struct
    type 'a t = 'a * (mono * mono) list
    let return a = a, []
    let bind (a, cs) ~f = let (b, cs') = f a in (b, cs @ cs')
    let map = `Define_using_bind
  end)

  let tell c = (), [c]
  let tell_many cs = (),cs
end

module TySet = Set.Make(struct
  type t = mono [@@deriving sexp]
  let compare = Pervasives.compare
end)

let free_ty ty =
  let open TySet in
  let rec go = function
    | `Forall (n, ty) -> remove (go ty) (`TyVar n)
    | `Fun (ty0, ty1) -> union (go (ty0 :> ty)) (go (ty1 :> ty))
    | `Thunk ty -> go (ty :> ty)
    | `TyVar _ as t -> singleton t
    | `TyFun (f, args) -> union_list (List.map ~f:(fun m -> go (m :> ty)) args)
    | `Int | `Bool | `Bottom -> empty
  in go ty

let free_ctx ctx =
  TyCtx.to_list ctx ~f:(fun (_, ty) -> free_ty ty) |> TySet.union_list

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
  | (`Thunk s, `Thunk t) :: cs -> unify @@ (s, t) :: cs
  | (`Bottom, _) :: cs | (_, `Bottom) :: cs -> unify cs
  | _ -> raise @@ Tilambda_exception Unification_failed

let sub_ty_in_term sub t =
  let rec go = function
    | `App (t0, t1, ty) -> `App (go t0, go t1, sub ty)
    | `Lambda (p, ty, t) -> `Lambda (p, sub ty, go t)
    | `Let (p, e, b) -> `Let (p, go e, go b)
    | `Att (t, ty) -> `Att (go t, sub ty)
    | `Force (t, ty) -> `Force (go t, sub ty)
    | `Thunk t -> `Thunk (go t)
    | t -> t
  in go t

let rec inst: ty -> mono = function
  | `Forall (i, ty) ->
      let tv = FreshTyVar.next () in
      inst (sub_ty' i tv ty)
  | `Int | `Bool | `Fun _ | `Thunk _ | `Bottom | `TyVar _
  | `TyFun _ as ty -> ty

let derive_constraints t =
  let open Acc in
  let open Let_syntax in
  let rec go ~ctx = function
    | `Int _ -> return `Int
    | `Bool _ -> return `Bool
    | `Bottom -> return `Bottom
    | `Ident n -> TyCtx.lookup_exn ctx n |> inst |> return
    | `Thunk t -> let%map ty = go ~ctx t in `Thunk ty
    | `Att (t, ty) ->
        let%bind ty' = go ~ctx t in
        let%map () = tell (ty, ty') in
        ty
    | `Force (t, ty) ->
        let%bind ty' = go ~ctx t in
        let%map () = tell (`Thunk ty, ty') in
        ty
    | `Lambda (`Ident n, ty0, t) ->
        let ctx = TyCtx.bind ctx n (ty0 :> ty) in
        let%map ty1 = go ~ctx t in
        `Fun (ty0, ty1)
    | `Lambda (`Wildcard, ty0, t) ->
        let%map ty1 = go ~ctx t in
        `Fun (ty0, ty1)
    | `Let (`Ident n, e, b) ->
        let te, cs = go ~ctx e in
        let sub = unify cs in (*TODO: use this unification to simplify ctx, cs*)
        let te = sub te in
        let fs = TySet.diff (free_ty (te :> ty)) (free_ctx ctx) in
        let gen_te = TySet.fold_right fs ~init:(te :> ty) ~f:(fun t s ->
            match t with
            | `TyVar n -> `Forall (n, s)
            | _ -> s
        ) in
        let ctx = TyCtx.bind ctx n gen_te in
        (b, cs) >>= go ~ctx
    | `Let (`Wildcard, e, b) ->
        let%bind _ = go ~ctx e in go ~ctx t
    | `App (t0, t1, ty) ->
        let%bind ty0 = go ~ctx t0 in
        let%bind ty1 = go ~ctx t1 in
        let%map () = tell (ty0, `Fun (ty1, ty)) in
        ty
  in go ~ctx:predef t

let rec erase = function
  `Int _ | `Bool _ | `Ident _ | `Bottom as t -> t
| `Att (t, _) -> erase t
| `Thunk t -> `Thunk (erase t)
| `Force (t, _) -> `Force (erase t)
| `Lambda (p, _, t) -> `Lambda (p, erase t)
| `Let (p, e, b) -> `App (`Lambda (p, erase b), erase e)
| `App (t0, t1, _) -> `App (erase t0, erase t1)

let front_end s = s
  |> parse
  |> introduce_tyvars
  |> fun tl ->
      let ty, cs = derive_constraints tl in
      let sub = unify cs in
      sub ty, sub_ty_in_term sub tl

let via_ulambda tl = tl
  |> erase
  |> Ulambda.church
  |> Clambda.reduce Ulambda.church_predef

let type_of s = s |> front_end |> fst |> fun ty -> pretty_type (ty :> ty)

let compile s = s |> front_end |> snd |> via_ulambda
