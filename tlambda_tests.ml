open Core_kernel.Std
open Sexplib

let p s = s |> Sexp.to_string_hum |> print_endline
let pretty_term t = Tlambda_parsetree.sexp_of_term t |> p
let pretty_ty t = Tlambda_parsetree.sexp_of_ty t |> p

let cases = [
  "(λx:int.x) 0", `Int 0, None;
  "(λf:(int->int).f 1) (λx:int.x)", `Int 1, None;
  "(λx:int.λy:bool.x) 0 #t", `Int 0, None;
  "(λx:bool.λy:int.y) #t 0", `Int 0, None;
  "(λx:int.λy:int.x) 0 1", `Int 0, None;
  "(λx:int.λy:int.y) 0 1", `Int 1, None;
  "(λx:bool.x) 0", `TypeError, None;

  "0!", `TypeError, Some "can not force a non-thunk in the typed setting";
  "{λx:int.x} 0", `TypeError, Some (
    "contrary to the untyped calculs, thunks in function position are " ^
    "not forced");
  "{0}!", `Int 0, None;

  "and #t #f", `Bool false, None;
  "or #t #f", `Bool true, None;
  "1+2", `Int 3, None;
  "2-1", `Int 1, None;
  "2*3", `Int 6, None;
  "zero? 0", `Bool true, None;
  "zero? 1", `Bool false, None;
  "eq? 1 7", `Bool false, None;
  "eq? 7 7", `Bool true, None;
  "leq? 1 7", `Bool true, None;
  "leq? 7 7", `Bool true, None;
  "leq? 8 7", `Bool false, None;
  "succ 2", `Int 3, None;
  "pred 2", `Int 1, None;
]

module T = Test_suite.Make2(struct
  let name = "tlambda"
  include Test_suite.Stdout(struct let name = name end)
  let compile = Tlambda.compile
  let cases = cases
end)

module T2 = Test_suite.Make2(struct
  let name = "tlambda (embedded into flambda)"
  include Test_suite.Stdout(struct let name = name end)
  let compile = Tlambda.compile_via_flambda
  let cases = cases
end)

let run () = T.(run stdout) (); T2.(run stdout) ()
let markdown out = T.(run (markdown out) ())
