open Sexplib

let pretty t =
  Tlambda_parsetree.sexp_of_term t
  |> Sexp.to_string_hum
  |> print_endline

let run = fun _ ->
  Errors.run_with_pretty_errors ~err:(fun _ -> exit 1) (fun () ->
    Tlambda.parse "λx:int.x" |> pretty;
    Tlambda.parse "λ_:int.x" |> pretty;
    Tlambda.parse "(λx:int->bool.x)" |> pretty;
    Tlambda.parse "(λx:((int)->bool).x)" |> pretty;
    Tlambda.parse "(λx:int->bool->int.x)" |> pretty;
    Tlambda.parse "(λx:(int->bool)->int.x)" |> pretty;
    Tlambda.parse "(λx:int->(bool->int).x)" |> pretty;
  )
