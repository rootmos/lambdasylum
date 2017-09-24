module T = Test_suite.Make2(struct
  let name = "tilambda"
  include Test_suite.Stdout(struct let name = name end)
  let compile = Tilambda.compile
  let cases = [
    "(λx:int.x+1) 0", `Int 1, None;
    "(λx.x+1) 0", `Int 1, None;
    "(λx.x+1) #t", `TypeError, None;
    "0:int", `Int 0, None;
    "⊥:int", `Bottom, Some ("⊥ can be unified to any type");
    "0:bool", `TypeError, None;
    "(⊥:int->bool) 7", `Bottom, None;

    "0!", `TypeError, Some ("can not force a non-thunk in the typed setting");
    "{λx:int.x} 0", `TypeError, Some (
      "contrary to the untyped calculs, thunks in function position are " ^
      "not forced");
    "{0}!", `Int 0, None;

    "if #t 0 1", `Int 0, Some (
      "`if: ∀T.bool->T->T->T` (poly-type or rank-1 (prenex) polymorphism)");
    "(if #t {0} {⊥})!", `Int 0, None;

    "nil? nil", `Bool true, None;
    "nil? (cons 0 nil)", `Bool false, None;
    "head nil", `Bottom, None;
    "head (cons 0 nil)", `Int 0, None;
    "nil? (tail (cons 0 nil))", `Bool true, None;
    "head (tail (cons 0 (cons 1 nil)))", `Int 1, None;
    "nil", `AlphaEqv "λx.λy.y", None;
    "nil? (tail nil)", `Bool true, None;
    "cons #t (cons 0 nil)", `TypeError, None;
    "cons 0 (cons #f nil)", `TypeError, None;

    "λf.(λ_.f 0) (f #t)", `TypeError, None;
    "let x = 1 in x+1", `Int 2, None;
    "let f = λy.y+1 in f 1", `Int 2, None;
    "let f = λx.x in (λ_.f 0) (f #t)", `Int 0, Some "let-polymorphism";
    "let f = λx.x in let g = f in (λ_.g 0) (g #t)", `Int 0, None;
    "let f = λx.x in (λg.(λ_.g 0) (g #t)) f", `TypeError, Some
      "rank-2 polymorphism not supported";
    "let f = λx.λtl.cons x tl in head (f 0 nil)", `Int 0, None;
    "let f = λx.λtl.cons x tl in head (f 0 1)", `TypeError, None;
    "λx.let f = λy.y+x in and x x", `TypeError, None;
  ]
end)

let run = T.(run stdout)
let markdown out = T.(run (markdown out) ())
