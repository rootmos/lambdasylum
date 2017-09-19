open Core_kernel.Std
open Printf

module T = Test_suite.Make2(struct
  let name = "clambda"
  include Test_suite.Stdout(struct let name = name end)
  let compile = Clambda.compile
  let cases = [
    "λx.x", `AlphaEqv "λy.y", None;
    "(λx.x) (λy.y y)", `AlphaEqv "λz.z z", None;
    "(λx.λ_.x)", `AlphaEqv "λa.λb.a", None;
    "(λ_.λy.y)", `AlphaEqv "λa.λb.b", None;
    "(λx.λy.x) (λa.a) (λb.b b)", `AlphaEqv "λa.a", None;
    "(λx.λy.y) (λa.a) (λb.b b)", `AlphaEqv "λb.b b", None;
    "((λx.{x}) (λa.a))!", `AlphaEqv "λa.a", None;
    "((λx.(λx.{x}) (λa.a)) (λb.b b))!", `AlphaEqv "λa.a", None;
    "((λx.(λy.{x}) (λa.a)) (λb.b b))!", `AlphaEqv "λb.b b", None;

    "λx.(λy.y) x", `AlphaEqv "λa.(λb.b) a", Some (
      "reduction stops at λ, no full β-reduction " ^
      "(which would have yielded `λx.x`)");

    "(λx.λ_.x) ((λy.y) (λz.z z))", `AlphaEqv "λ_.λz.z z", Some(
      "reduction eagerly evaluates arguments " ^
      "(when starting with the β-reduction: `λ_.(λy.y) (λz.z z)`)");

    "⊥", `Bottom, None;
    "{⊥}", `Thunk, None;
    "{{⊥}}", `Thunk, None;
    "{{⊥}}!", `Thunk, None;
    "{{⊥}}!!", `Bottom, None;

    "{λx.x}!", `AlphaEqv "λx.x", None;
    "(λx.x)!", `AlphaEqv "λx.x", Some (
      "forcing a non-thunk is accepted (in the untyped setting)");

    "{λx.x} (λy.y y)", `AlphaEqv "λy.y y", Some (
      "thunks showing up function position are forced (in the untyped setting)");

    "{(λx.x) (λx.λy.x)}!", `AlphaEqv "λx.λ_.x", None;
    "((λx.x) (λx.λy.x))!", `AlphaEqv "λx.λ_.x", None;
  ]
end)

let run = T.(run stdout)
let markdown out = T.(run (markdown out) ())
