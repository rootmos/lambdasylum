open Core_kernel.Std

let p s = s |> Sexp.to_string_hum |> print_endline
let pretty_term t = Flambda_parsetree.sexp_of_term t |> p
let pretty_ty t = Flambda_parsetree.sexp_of_ty t |> p

module T = Test_suite.Make2(struct
  let name = "flambda"
  include Test_suite.Stdout(struct let name = name end)
  let compile = Flambda.compile
  let cases = [
    "(λx:int.x) 0", `Int 0, None;
    "(ΛT.λx:T.x) [int] 0", `Int 0, None;
    "(ΛT.λx:T.x) [bool] 0", `TypeError, None;
    "(λf:∀T.T->T.f [int] 0) (ΛA.λa:A.a)", `Int 0, None;
    "(λf:∀T.∀T.T->T.f [bool] [int] 0) (ΛA.ΛA.λa:A.a)", `Int 0, None;

    "(λx:int.x) ⊥", `Bottom, Some "⊥ is a subtype of any type";

    "if [int] #t 0 1", `Int 0, Some "`if: ∀T.bool->T->T->T`" ;
    "if [int] #f 0 1", `Int 1, None;
    "(if [{int}] #t {0} {⊥})!", `Int 0, None;
    "(if [{int}] #f {⊥} {1})!", `Int 1, None;

    "nil? [int] (nil [int])", `Bool true, Some "`nil: ∀T.∀Z.(T->Z->Z)->Z->Z`";
    "nil? [bool] (nil [bool])", `Bool true,
      Some "`nil?: ∀T.(∀Z.(T->Z->Z)->Z->Z)->bool`";
    "nil? [bool] (nil [int])", `TypeError, None;
    "nil? [int] (cons [int] 0 (nil [int]))", `Bool false,
      Some "`cons: ∀T.T->(∀Z.(T->Z->Z)->Z->Z)->(∀Z.(T->Z->Z)->Z->Z)`";
    "nil? [bool] (cons [bool] #t (nil [bool]))", `Bool false, None;
    "(cons [bool] #t (nil [int]))", `TypeError, None;
    "(cons [int] 0 (nil [bool]))", `TypeError, None;
    "head [int] (nil [int])", `Bottom,
      Some "`head: ∀T.(∀Z.(T->Z->Z)->Z->Z)->T`";
    "head [int] (cons [int] 0 (nil [int]))", `Int 0, None;
    "head [bool] (cons [bool] #f (nil [bool]))", `Bool false, None;
    "nil? [int] (tail [int] (cons [int] 0 (nil [int])))", `Bool true,
      Some "`tail: ∀T.(∀Z.(T->Z->Z)->Z->Z)->(∀Z.(T->Z->Z)->Z->Z)`";
    "head [int] (tail [int] (cons [int] 0 (cons [int] 1 (nil [int]))))",
      `Int 1, None;
  ]
end)

let run = T.(run stdout)
let markdown out = T.(run (markdown out) ())
