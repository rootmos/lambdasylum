open Core_kernel.Std
open Printf

module T = Test_suite.Make2(struct
  let name = "ulambda"
  include Test_suite.Stdout(struct let name = name end)
  let compile = Ulambda.compile
  let cases = [
    "0", `Int 0, None;
    "1", `Int 1, None;
    "(λx.x) 1", `Int 1, None;
    "(\\lambda x.x) 1", `Int 1, None;

    "#t", `Bool true, None;
    "#f", `Bool false, None;
    "#t", `AlphaEqv "λx.λy.x", None;
    "#t", `AlphaEqv "λx.λy.y", None;

    "1+2", `Int 3, None;
    "2+1", `Int 3, None;
    "0+1", `Int 1, None;
    "1+0", `Int 1, None;

    "7-2", `Int 5, None;
    "0-0", `Int 0, None;
    "1-0", `Int 1, None;
    "0-1", `Int 0, Some "subtraction is floored at 0";
    "2-2", `Int 0, None;

    "0*4", `Int 0, None;
    "3*4", `Int 12, None;

    "0", `AlphaEqv "λf.λx.x", None;
    "1", `AlphaEqv "λf.λx.f x", None;
    "2", `AlphaEqv "λf.λx.f (f x)", None;

    "succ 0", `Int 1, None;
    "succ 1", `Int 2, None;
    "succ 7", `Int 8, None;

    "pred 0", `Int 0, None;
    "pred 1", `Int 0, None;
    "pred 2", `Int 1, None;
    "pred 7", `Int 6, None;

    "if #t 1 2", `Int 1, None;
    "if #f 1 2", `Int 2, None;

    "and #t #t", `Bool true, None;
    "and #f #t", `Bool false, None;
    "and #t #f", `Bool false, None;
    "and #f #f", `Bool false, None;

    "if (and #t #t) 1 2", `Int 1, None;
    "if (and #f #t) 1 2", `Int 2, None;
    "if (and #t #f) 1 2", `Int 2, None;
    "if (and #f #f) 1 2", `Int 2, None;

    "if (or #t #t) 1 2", `Int 1, None;
    "if (or #f #t) 1 2", `Int 1, None;
    "if (or #t #f) 1 2", `Int 1, None;
    "if (or #f #f) 1 2", `Int 2, None;

    "if (zero? 0) 1 2", `Int 1, None;
    "if (zero? 1) 1 2", `Int 2, None;
    "if (zero? 7) 1 2", `Int 2, None;

    "if (leq? 3 4) 1 2", `Int 1, None;
    "if (leq? 3 3) 1 2", `Int 1, None;
    "if (leq? 4 3) 1 2", `Int 2, None;

    "if (eq? 3 4) 1 2", `Int 2, None;
    "if (eq? 3 3) 1 2", `Int 1, None;
    "if (eq? 4 3) 1 2", `Int 2, None;

    "⊥", `Bottom, None;
    "\\bot", `Bottom, None;

    "{⊥}", `Thunk, None;
    "{{⊥}}", `Thunk, None;
    "{0}!", `Int 0, None;
    "{{0}}!", `Thunk, None;
    "{{0}}!!", `Int 0, None;
    "0!", `Int 0, None;

    "{λx.x} 2", `Int 2, None;

    "if #t 0 {⊥}", `Int 0, None;
    "if #f {⊥} 1", `Int 1, None;
    "if #f {0} ⊥", `Bottom, None;
    "(if #t {0} {⊥})!", `Int 0, None;
    "(if #f {0} {⊥})!", `Bottom, None;
    "(if #t 1 {⊥})!", `Int 1, None;
    "((if #t 1 {2})!)+1", `Int 2, None;
    "((if #f 1 {2})!)+1", `Int 3, None;

    "(fix (λk.λn.(if (eq? n 1) 1 {(k (n-1))*n})!)) 5", `Int 120, Some
      "factorial function";
    "(fix (λk.λn.(if (leq? n 1) 1 {(k (n-1))+(k (n-2))})!)) 7", `Int 21, Some
      "naive Fibonacci sequence";

    "nil? nil", `Bool true, None;
    "nil? (cons 0 nil)", `Bool false, None;
    "head nil", `Bottom, None;
    "head (cons 0 nil)", `Int 0, None;
    "nil? (tail (cons 0 nil))", `Bool true, None;
    "head (tail (cons 0 (cons 1 nil)))", `Int 1, None;
    "nil", `AlphaEqv "λx.λy.y", None;
    "nil? (tail nil)", `Bool true, Some "`tail nil` reduces to `nil`";
  ]
end)

let run = T.(run stdout)
let markdown out = T.(run (markdown out) ())
