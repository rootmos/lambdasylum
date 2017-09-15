# Lambdasylum
[![Build Status](https://travis-ci.org/rootmos/lambdasylum.svg?branch=master)](https://travis-ci.org/rootmos/lambdasylum)

The lambda asylum is a place to try out different kinds of lambda calculi.
Currently the following calculi are implemented:
* [untyped lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) (in [`ulambda`](../master/ulambda.ml))
  with [Church encoded](https://en.wikipedia.org/wiki/Church_encoding) natural numbers and booleans
* [simply type lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) (in [`tlambda`](../master/tlambda.ml))
* [System F](https://en.wikipedia.org/wiki/System_F) (in [`flambda`](../master/flambda.ml))

## Usage
Simplest way to try it out is by using Docker:
```
docker run -it rootmos/lambdasylum
```

## Examples for `clambda`
`(λx.x)` ⟶ `λy.y`

`(λx.x) (λy.y y)` ⟶ `λz.z z`

`(λx.λy.x) (λa.a) (λb.b b)` ⟶ `λa.a`

`(λx.λy.y) (λa.a) (λb.b b)` ⟶ `λb.b b`

`((λx.{x}) (λa.a))!` ⟶ `λa.a`

`((λx.(λx.{x}) (λa.a)) (λb.b b))!` ⟶ `λa.a`

`((λx.(λy.{x}) (λa.a)) (λb.b b))!` ⟶ `λb.b b`


## Examples for `ulambda`
`0` ⟶ `0`

`1` ⟶ `1`

`(λx.x) 1` ⟶ `1`

`(\lambda x.x) 1` ⟶ `1`

`#t` ⟶ `true`

`#f` ⟶ `false`

`1+2` ⟶ `3`

`2+1` ⟶ `3`

`0+1` ⟶ `1`

`1+0` ⟶ `1`

`7-2` ⟶ `5`

`0-0` ⟶ `0`

`1-0` ⟶ `1`

`0-1` ⟶ `0`

`2-2` ⟶ `0`

`3*4` ⟶ `12`

`succ 0` ⟶ `1`

`succ 1` ⟶ `2`

`succ 7` ⟶ `8`

`pred 0` ⟶ `0`

`pred 1` ⟶ `0`

`pred 2` ⟶ `1`

`pred 7` ⟶ `6`

`if #t 1 2` ⟶ `1`

`if #f 1 2` ⟶ `2`

`and #t #t` ⟶ `true`

`and #f #t` ⟶ `false`

`and #t #f` ⟶ `false`

`and #f #f` ⟶ `false`

`if (and #t #t) 1 2` ⟶ `1`

`if (and #f #t) 1 2` ⟶ `2`

`if (and #t #f) 1 2` ⟶ `2`

`if (and #f #f) 1 2` ⟶ `2`

`if (or #t #t) 1 2` ⟶ `1`

`if (or #f #t) 1 2` ⟶ `1`

`if (or #t #f) 1 2` ⟶ `1`

`if (or #f #f) 1 2` ⟶ `2`

`if (zero? 0) 1 2` ⟶ `1`

`if (zero? 1) 1 2` ⟶ `2`

`if (zero? 7) 1 2` ⟶ `2`

`if (leq? 3 4) 1 2` ⟶ `1`

`if (leq? 3 3) 1 2` ⟶ `1`

`if (leq? 4 3) 1 2` ⟶ `2`

`if (eq? 3 4) 1 2` ⟶ `2`

`if (eq? 3 3) 1 2` ⟶ `1`

`if (eq? 4 3) 1 2` ⟶ `2`

`⊥` ⟶ `reached bottom`

`\bot` ⟶ `reached bottom`

`{⊥}` ⟶ `{..}`

`{{⊥}}` ⟶ `{..}`

`{0}!` ⟶ `0`

`{{0}}!` ⟶ `{..}`

`{{0}}!!` ⟶ `0`

`0!` ⟶ `0`

`{λx.x} 2` ⟶ `2`

`if #t 0 {⊥}` ⟶ `0`

`if #f {⊥} 1` ⟶ `1`

`if #f {0} ⊥` ⟶ `reached bottom`

`(if #t {0} {⊥})!` ⟶ `0`

`(if #f {0} {⊥})!` ⟶ `reached bottom`

`(if #t 1 {⊥})!` ⟶ `1`

`((if #t 1 {2})!)+1` ⟶ `2`

`((if #f 1 {2})!)+1` ⟶ `3`

`(fix (λk.λn.(if (eq? n 1) 1 {(k (n-1))*n})!)) 5` ⟶ `120`

`(fix (λk.λn.(if (leq? n 1) 1 {(k (n-1))+(k (n-2))})!)) 7` ⟶ `21`

`nil? nil` ⟶ `true`

`nil? (cons 0 nil)` ⟶ `false`

`head nil` ⟶ `false`

`head (cons 0 nil)` ⟶ `0`

`nil? (tail (cons 0 nil))` ⟶ `true`

`head (tail (cons 0 (cons 1 nil)))` ⟶ `1`


## Examples for `tlambda`
`(λx:int.x) 0` ⟶ `0`

`(λf:(int->int).f 1) (λx:int.x)` ⟶ `1`

`(λx:int.λy:bool.x) 0 #t` ⟶ `0`

`(λx:bool.λy:int.y) #t 0` ⟶ `0`

`(λx:int.λy:int.x) 0 1` ⟶ `0`

`(λx:int.λy:int.y) 0 1` ⟶ `1`

`(λx:bool.x) 0` ⟶ `type error`

`0!` ⟶ `type error`

`{0}!` ⟶ `0`

`and #t #f` ⟶ `false`

`or #t #f` ⟶ `true`

`1+2` ⟶ `3`

`2-1` ⟶ `1`

`2*3` ⟶ `6`

`zero? 0` ⟶ `true`

`zero? 1` ⟶ `false`

`eq? 1 7` ⟶ `false`

`eq? 7 7` ⟶ `true`

`leq? 1 7` ⟶ `true`

`leq? 7 7` ⟶ `true`

`leq? 8 7` ⟶ `false`

`succ 2` ⟶ `3`

`pred 2` ⟶ `1`


## Examples for `flambda`
`(λx:int.x) 0` ⟶ `0`

`(ΛT.λx:T.x) [int] 0` ⟶ `0`

`(ΛT.λx:T.x) [bool] 0` ⟶ `type error`

`(λf:∀T.T->T.f [int] 0) (ΛA.λa:A.a)` ⟶ `0`

`(λf:∀T.∀T.T->T.f [bool] [int] 0) (ΛB.ΛA.λa:A.a)` ⟶ `0`

`if [int] #t 0 1` ⟶ `0`

`if [int] #f 0 1` ⟶ `1`

`(if [{int}] #t {0} {⊥})!` ⟶ `0`

`(if [{int}] #f {⊥} {1})!` ⟶ `1`


