open Core_kernel.Std
open Printf

module T = Test_suite.Make2(struct
  let name = "ulambda"
  include Test_suite.Stdout(struct let name = name end)
  let compile = Ulambda.compile
  let cases = [
    "0", `Int 0;
    "1", `Int 1;
    "(λx.x) 1", `Int 1;
    "(\\lambda x.x) 1", `Int 1;

    "#t", `Bool true;
    "#f", `Bool false;

    "1+2", `Int 3;
    "2+1", `Int 3;
    "0+1", `Int 1;
    "1+0", `Int 1;

    "7-2", `Int 5;
    "0-0", `Int 0;
    "1-0", `Int 1;
    "0-1", `Int 0;
    "2-2", `Int 0;

    "3*4", `Int 12;

    "succ 0", `Int 1;
    "succ 1", `Int 2;
    "succ 7", `Int 8;

    "pred 0", `Int 0;
    "pred 1", `Int 0;
    "pred 2", `Int 1;
    "pred 7", `Int 6;

    "if #t 1 2", `Int 1;
    "if #f 1 2", `Int 2;

    "and #t #t", `Bool true;
    "and #f #t", `Bool false;
    "and #t #f", `Bool false;
    "and #f #f", `Bool false;

    "if (and #t #t) 1 2", `Int 1;
    "if (and #f #t) 1 2", `Int 2;
    "if (and #t #f) 1 2", `Int 2;
    "if (and #f #f) 1 2", `Int 2;

    "if (or #t #t) 1 2", `Int 1;
    "if (or #f #t) 1 2", `Int 1;
    "if (or #t #f) 1 2", `Int 1;
    "if (or #f #f) 1 2", `Int 2;

    "if (zero? 0) 1 2", `Int 1;
    "if (zero? 1) 1 2", `Int 2;
    "if (zero? 7) 1 2", `Int 2;

    "if (leq? 3 4) 1 2", `Int 1;
    "if (leq? 3 3) 1 2", `Int 1;
    "if (leq? 4 3) 1 2", `Int 2;

    "if (eq? 3 4) 1 2", `Int 2;
    "if (eq? 3 3) 1 2", `Int 1;
    "if (eq? 4 3) 1 2", `Int 2;

    "⊥", `Bottom;
    "\\bot", `Bottom;

    "{⊥}", `Thunk;
    "{{⊥}}", `Thunk;
    "{0}!", `Int 0;
    "{{0}}!", `Thunk;
    "{{0}}!!", `Int 0;
    "0!", `Int 0;

    "{λx.x} 2", `Int 2;

    "if #t 0 {⊥}", `Int 0;
    "if #f {⊥} 1", `Int 1;
    "if #f {0} ⊥", `Bottom;
    "(if #t {0} {⊥})!", `Int 0;
    "(if #f {0} {⊥})!", `Bottom;
    "(if #t 1 {⊥})!", `Int 1;
    "((if #t 1 {2})!)+1", `Int 2;
    "((if #f 1 {2})!)+1", `Int 3;

    "(fix (λk.λn.(if (eq? n 1) 1 {(k (n-1))*n})!)) 5", `Int 120;
    "(fix (λk.λn.(if (leq? n 1) 1 {(k (n-1))+(k (n-2))})!)) 7", `Int 21;

    "nil? nil", `Bool true;
    "nil? (cons 0 nil)", `Bool false;
    "head nil", `Bool false;
    "head (cons 0 nil)", `Int 0;
    "nil? (tail (cons 0 nil))", `Bool true;
    "head (tail (cons 0 (cons 1 nil)))", `Int 1;
  ]
end)

let run = T.(run stdout)
let markdown out = T.(run (markdown out) ())
