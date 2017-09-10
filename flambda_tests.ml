open Core_kernel.Std

let p s = s |> Sexp.to_string_hum |> print_endline
let pretty_term t = Flambda_parsetree.sexp_of_term t |> p
let pretty_ty t = Flambda_parsetree.sexp_of_ty t |> p

let c s =
  let fl = Flambda.parse s in
  let _ = Flambda.typecheck Flambda.predef fl in

  Flambda.erase fl
  |> Ulambda.church
  |> Ulambda.reduce Ulambda.predef ~k:(fun x -> x)

module T = Test_suite.Make2(struct
  let name = "flambda"
  let compile = c
  let cases = [
    "(λx:int.x) 0", `Int 0;
    "(ΛT.λx:T.x) [int] 0", `Int 0;
    "(ΛT.λx:T.x) [bool] 0", `TypeError;
    "(λf:∀T.T->T.f [int] 0) (ΛA.λa:A.a)", `Int 0;
    "(λf:∀T.∀T.T->T.f [bool] [int] 0) (ΛB.ΛA.λa:A.a)", `Int 0;
    "if [int] #t 0 1", `Int 0;
    "if [int] #f 0 1", `Int 1;
    "(if [{int}] #t {0} {⊥})!", `Int 0;
    "(if [{int}] #f {⊥} {1})!", `Int 1;
  ]
end)

let run () = Errors.run_with_pretty_errors ~err:(fun _ -> exit 1) T.run
