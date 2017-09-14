open Core_kernel.Std
open Sexplib

let p s = s |> Sexp.to_string_hum |> print_endline
let pretty_term t = Tlambda_parsetree.sexp_of_term t |> p
let pretty_ty t = Tlambda_parsetree.sexp_of_ty t |> p

module T = Test_suite.Make2(struct
  let name = "tlambda"
  include Test_suite.Stdout(struct let name = name end)
  let compile = Tlambda.compile
  let cases = [
    "(λx:int.x) 0", `Int 0;
    "(λf:(int->int).f 1) (λx:int.x)", `Int 1;
    "(λx:int.λy:bool.x) 0 #t", `Int 0;
    "(λx:bool.λy:int.y) #t 0", `Int 0;
    "(λx:int.λy:int.x) 0 1", `Int 0;
    "(λx:int.λy:int.y) 0 1", `Int 1;
    "(λx:bool.x) 0", `TypeError;
    "0!", `TypeError;
    "{0}!", `Int 0;
  ]
end)

let run = T.(run stdout)
let markdown out = T.(run (markdown out) ())
