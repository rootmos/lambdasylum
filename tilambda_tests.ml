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
  ]
end)

let run = T.(run stdout)
let markdown out = T.(run (markdown out) ())
