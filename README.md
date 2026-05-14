# SMAL RaTT
A proof-of-concept type checker implementation of a SMAL(ler) version – subset – of Async RaTT, a modal language for asynchronous functional reactive programming, named SMAL RaTT.

Made by Louise Andersen (LA) and Sofie Meta (SM)


## GHCi and demo
To interact with SMAL RaTT, run `cabal repl exe:smal-ratt` from the project root dir.
Inside the repl one can see a "pretty printed" (work-in-progress) version of the terms and types defined in SMAL RaTT. E.g. by writing mapTerm
```
smal-ratt> mapTerm
fix r .
  \f.
    \s.
      let x = head (s) in
        let xs = tail (s) in
          (unbox (f)) @
            (x) ::
            delay
              cl(xs)
              ((advAL r) @
                (f)) @
                (adv xs)
```

## Test
Unit tests can be run using the `cabal test` from the project root dir. 
Should produce the following: 

```
[...]
Test suite smal-ratt-test: RUNNING...

TypeChecker.hs
  Variable / lookup in context
    returns UnboundVar for missing variable [✔]
    infers variable in simple context [✔]
    infers variable in ticked context [✔]
    check defaults to infer on correct type [✔]
  Projection ()
    fst projects the first component of a product [✔]
    snd projects the second component of a product [✔]
    check fails when given wrong type [✔]
  Pair
    infers Prod type from context [✔]
    fails on looking up non-existent variable in context [✔]
    check fails when given wrong type [✔]
  Advance (EL)
    correctly extracts A from EL A when tick is in context [✔]
    fails when argument id not value [✔]
    correctly extracts nested EL A from EL (EL A) when tick is in context [✔]
    fails when no tick in context [✔]
    fails when tick is on wrong clock [✔]
    fails when v does not have a delayed type [✔]
  Advance AL
    fails when no tick in context [✔]
    correctly extracts A from AL A when tick is other clock [✔]
  Select EL EL
    correctly extracts three-way sum type when tick single variable [✔]
    correctly extracts three-way sum type when tick is on union of clocks [✔]
    fails when tick is on union of clocks in wrong order [✔]
    fails when variable not in latest ticked [✔]
  Delay EL
    pass in valid context [✔]
    pass with annotated EL type in valid context [✔]
    fails with wrong annotation: not EL type [✔]
    body is checked for wrong variable [✔]
    clock on different term fails [✘]
  Unbox
    infer passes when type is boxed [✔]
    infer fails when type is not boxed [✔]
    check passes when given Box type [✔]
    infer passes when given Box type from ticked context [✔]
    check fails when inferred type doesn't match checked type [✔]
    infer fails when variable not in context [✔]
    infer fails when type is a Fun, not Box [✔]
  Always (box)
    infer type when context is tick-free [✔]
    infer type when context is tick-free [✔]
    infer fails when variable has non-stable type [✔]
    infer fails for non-stable variable in ticked context [✔]
    check passes when checked against Box type [✔]
    check fails when checked against non-Box type [✔]
  Case
    infers type when both branches have the same type [✔]
    checks correctly when expected type matches both branches [✔]
    infers type when branches return a product [✔]
    fails when argument is not a sum type [✔]
    fails when argument variable is not in context [✔]
    fails when branches have different types [✔]
    fails when right branch has wrong type even though both branches are well-typed alone [✔]
    left branch variable is not in scope in right branch [✔]
    right branch variable is not in scope in left branch [✔]
    bound variables in each branch correctly shadow outer context [✔]
    infers correctly for nested case expressions [✔]
SignalCombinators.hs
  map
    map infers correctly to its type using annotation [✔]
    mapbad fails: unboxed f [✔]
    mapbad fails: unboxed f crosses tick boundary [✔]
    map with wrong clock [✔]
  switch
    switch typechecks with correct type [✔]
    infer failes with clock in other order [✔]

Failures:

  test/TypeCheckerSpec.hs:159:28: 
  1) TypeChecker.hs, Delay EL, clock on different term fails
       expected: Left Error: Clock mismatch
        but got: Right EL (Nat)
[...]
```