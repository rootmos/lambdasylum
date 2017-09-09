open Sexplib

let p s = s |> Sexp.to_string_hum |> print_endline
let pretty_term t = Tlambda_parsetree.sexp_of_term t |> p
let pretty_ty t = Tlambda_parsetree.sexp_of_ty t |> p


let c s =
  Tlambda.parse s
  |> Tlambda.typecheck Tlambda.TyCtx.empty
  |> pretty_ty

let run = fun _ ->
  Errors.run_with_pretty_errors ~err:(fun _ -> exit 1) (fun () ->
    c "λx:int.x";
    c "{0}!";
    c "(λ_:int.#t) 7";
    c "(λx:int->bool.x)";
    c "(λx:((int)->bool).x)";
    c "(λx:int->bool->int.x)";
    c "(λx:(int->bool)->int.x)";
    c "(λx:int->(bool->int).x)";
  )
