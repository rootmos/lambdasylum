module T = Test_suite.Make2(struct
  let name = "tilambda"
  include Test_suite.Stdout(struct let name = name end)
  let compile = Tilambda.compile
  let cases = [
    "(λx:int.x+1) 0", `Int 1;
    "(λx.x+1) 0", `Int 1;
    "(λx.x+1) #t", `TypeError;
    "0:int", `Int 0;
    "⊥:int", `Bottom;
    "0:bool", `TypeError;
    "(⊥:int->bool) 7", `Bottom;

    "if #t 0 1", `Int 0;

    "nil? nil", `Bool true;
    "nil? (cons 0 nil)", `Bool false;
    "head nil", `Bottom;
    "head (cons 0 nil)", `Int 0;
    "nil? (tail (cons 0 nil))", `Bool true;
    "head (tail (cons 0 (cons 1 nil)))", `Int 1;
    "nil", `AlphaEqv "λx.λy.y";
    "nil? (tail nil)", `Bool true;
    "cons #t (cons 0 nil)", `TypeError;
    "cons 0 (cons #f nil)", `TypeError;
  ]
end)

let run = T.(run stdout)
let markdown out = T.(run (markdown out) ())
