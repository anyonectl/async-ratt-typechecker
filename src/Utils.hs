module Utils where

import Syntax
import ErrorTypes
import Context
import TypeChecker


-- -- -- sigAwait_κ = fix x. delay_cl(wait_κ) (adv wait_κ :: adv_∀ x)
-- -- -- The signal sigAwait_k operates on a fixed clock {k}

sigAwaitType :: Type
sigAwaitType = EL (Sig Nat)

sigAwaitTerm :: Chan -> Term
sigAwaitTerm k = 
  Fix "x" -- the recursion variable x has type AL(EL(Sig A)).
    (Delay 
      (Clock (Wait k))
      (Cons
        (Adv (Wait k))
        (AdvAL (var "x"))
      )
    )

tailTerm :: Term
tailTerm = error "TODO"

tailTermType :: Type
tailTermType = error "TODO"

tailTermBad :: Term
tailTermBad = error "TODO"

tailTermBadType :: Type
tailTermBadType = error "TODO"

eliminateELType :: Type
eliminateELType = Nat

-- we name the variable "termToBeEliminated" because if it's simply "x" it will be confused with a potential (Variable "x") from the context
eliminateEL :: Term -> Term
eliminateEL t = Let "termToBeEliminated" t (Delay (Clock (Variable "termToBeEliminated")) (Adv (Variable "termToBeEliminated")))