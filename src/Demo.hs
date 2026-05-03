module Demo where

import Syntax
import Terms
import Context
import ErrorTypes
import TypeChecker

-- to demonstrate why the typing annotation `Anno` is important,
-- we create a demo program that uses map to increment values
-- of a simple Sig Nat

-- let map = mapTerm : mapType in map box(\x.x+1) (0 : Never)
demo :: Term
demo =
  Let "map" (Anno mapTerm mapType) -- in
    (App
      (App
        (Variable "map")
        (Anno 
          (Always (Lam "x" (Suc (Variable "x")))) -- box(\x.x+1)
          (Box (Fun Nat Nat)))) -- has type ◻(Nat --> Nat)
      (Cons Zero Never) -- 0 : Never
    )

-- without annotation
demoBad :: Term
demoBad =
  Let "map" mapTerm -- in
    (App
      (App
        (Variable "map")
        (Anno 
          (Always (Lam "x" (Suc (Variable "x")))) -- box(\x.x+1)
          (Box (Fun Nat Nat)))) -- has type ◻(Nat --> Nat)
      (Cons Zero Never) -- 0 : Never
    )

emptyCtx :: Ctx
emptyCtx = Ctx ([], [])

-- infer should return Right (Sig Nat)
withAnno :: Either TypeError Type
withAnno = infer emptyCtx demo

-- infer should fail when trying to infer Fix
withoutAnno :: Either TypeError Type
withoutAnno = infer emptyCtx demoBad
