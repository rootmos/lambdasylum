open Core_kernel.Std

let p s = s |> Sexp.to_string_hum |> print_endline
let pretty_term t = Flambda_parsetree.sexp_of_term t |> p
let pretty_ty t = Flambda_parsetree.sexp_of_ty t |> p

module T = Test_suite.Make2(struct
  let name = "flambda"
  include Test_suite.Stdout(struct let name = name end)
  let compile = Flambda.compile
  let cases = [
    "(λx:int.x) 0", `Int 0;
    "(ΛT.λx:T.x) [int] 0", `Int 0;
    "(ΛT.λx:T.x) [bool] 0", `TypeError;
    "(λf:∀T.T->T.f [int] 0) (ΛA.λa:A.a)", `Int 0;
    "(λf:∀T.∀T.T->T.f [bool] [int] 0) (ΛA.ΛA.λa:A.a)", `Int 0;
    "if [int] #t 0 1", `Int 0;
    "if [int] #f 0 1", `Int 1;
    "(if [{int}] #t {0} {⊥})!", `Int 0;
    "(if [{int}] #f {⊥} {1})!", `Int 1;

    "nil? [int] (nil [int])", `Bool true;
    "nil? [bool] (nil [bool])", `Bool true;
    "nil? [bool] (nil [int])", `TypeError;
    "nil? [int] (cons [int] 0 (nil [int]))", `Bool false;
    "nil? [bool] (cons [bool] #t (nil [bool]))", `Bool false;
    "head [int] (nil [int])", `Bottom;
    "head [int] (cons [int] 0 (nil [int]))", `Int 0;
    "head [bool] (cons [bool] #f (nil [bool]))", `Bool false;
    "nil? [int] (tail [int] (cons [int] 0 (nil [int])))", `Bool true;
    "head [int] (tail [int] (cons [int] 0 (cons [int] 1 (nil [int]))))", `Int 1;
  ]
end)

let run = T.(run stdout)
let markdown out = T.(run (markdown out) ())
