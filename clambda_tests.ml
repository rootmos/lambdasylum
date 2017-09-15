open Core_kernel.Std
open Printf

module T = Test_suite.Make2(struct
  let name = "clambda"
  include Test_suite.Stdout(struct let name = name end)
  let compile = Clambda.compile
  let cases = [
    "(λx.x)", `AlphaEqv "λy.y";
    "(λx.x) (λy.y y)", `AlphaEqv "λz.z z";
    "(λx.λy.x) (λa.a) (λb.b b)", `AlphaEqv "λa.a";
    "(λx.λy.y) (λa.a) (λb.b b)", `AlphaEqv "λb.b b";
    "((λx.{x}) (λa.a))!", `AlphaEqv "λa.a";
    "((λx.(λx.{x}) (λa.a)) (λb.b b))!", `AlphaEqv "λa.a";
    "((λx.(λy.{x}) (λa.a)) (λb.b b))!", `AlphaEqv "λb.b b";
  ]
end)

let run = T.(run stdout)
let markdown out = T.(run (markdown out) ())
