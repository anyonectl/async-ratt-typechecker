module Terms where

import Syntax

-- map = fix r. λf. λs. let x = head s in let xs = tail s in
--         unbox f x :: delay_cl(xs) (adv_AL r (adv xs))
mapTerm :: Term
mapTerm =
  Fix "r"
    (Lam "f"
      (Lam "s"
        (Let "x"  (Head (Variable "s"))
        (Let "xs" (Tail (Variable "s"))
          (Cons
            (App (Unbox (Variable "f")) (Variable "x"))
            (Delay (Clock (Variable "xs"))
              (App
                (App (AdvAL (Variable "r")) (Variable "f"))
                (Adv (Variable "xs")))))))))

mapType :: Type
mapType = Fun (Box (Fun Nat Nat)) (Fun (Sig Nat) (Sig Nat))

mapTermBad :: Term
mapTermBad = error "TODO"

mapTermBadType :: Type
mapTermBadType = error "TODO"

-- zip
--
zipTerm :: Term
zipTerm = error "TODO"

zipTermType :: Type
zipTermType = error "TODO"

zipTermBad :: Term
zipTermBad = error "TODO"

zipTermBadType :: Type
zipTermBadType = error "TODO"

-- tail
--
tailTerm :: Term
tailTerm = error "TODO"

tailTermType :: Type
tailTermType = error "TODO"

tailTermBad :: Term
tailTermBad = error "TODO"

tailTermBadType :: Type
tailTermBadType = error "TODO"

-- switch
--
switch :: Term
switch = Anno switchTerm switchType

switchType :: Type
switchType = Fun (Sig Nat) (Fun (EL (Sig Nat)) (Sig Nat))

switchTerm :: Term
switchTerm =
  Fix "recurse"
    (Lam "s" -- original signal (first argument)
      (Lam "ys" -- delayed signal (second argument)
        (Let "x" -- head of original signal s
          (Head (Variable "s"))
          (Let "xs" -- tail of original signal s
            (Tail (Variable "s"))
            (Cons
              (Variable "x")
              (Delay
                (ClockUnion
                  (Clock (Variable "xs"))
                  (Clock (Variable "ys")))
                (Case
                  (Select (Variable "xs") (Variable "ys"))
                  
                  -- outer left: ((Sig A × EL (Sig A)) + (EL (Sig A) × Sig A))
                  "oneTicked"
                  (Case
                    (Variable "oneTicked")

                    -- xs ticked first (inner left)
                    "left"
                    (App
                      (App
                        (AdvAL (Variable "recurse"))
                        (Proj Fst (Variable "left")))
                      (Proj Snd (Variable "left")))
                    
                    -- ys ticked first (inner right) - switch!
                    "right"
                    (Proj Snd (Variable "right")))

                  -- both ticked (outer right) - also switch
                  "bothTicked"
                  (Proj Snd (Variable "bothTicked"))
                )
              )  
            )
          )
        )
      )
    )

-- swapped the clocks of xs and ys in ClockUnion
switchBad :: Term
switchBad =
  Fix "recurse"
    (Lam "s" -- original signal (first argument)
      (Lam "ys" -- delayed signal (second argument)
        (Let "x" -- head of original signal s
          (Head (Variable "s"))
          (Let "xs" -- tail of original signal s
            (Tail (Variable "s"))
            (Cons
              (Variable "x")
              (Delay
                (ClockUnion
                  (Clock (Variable "ys"))   -- should be xs
                  (Clock (Variable "xs")))  -- should be ys
                (Case
                  (Select (Variable "xs") (Variable "ys"))
                  
                  -- outer left: ((Sig A × EL (Sig A)) + (EL (Sig A) × Sig A))
                  "oneTicked"
                  (Case
                    (Variable "oneTicked")

                    -- xs ticked first (inner left)
                    "left"
                    (App
                      (App
                        (AdvAL (Variable "recurse"))
                        (Proj Fst (Variable "left")))
                      (Proj Snd (Variable "left")))
                    
                    -- ys ticked first (inner right) - switch!
                    "right"
                    (Proj Snd (Variable "right")))

                  -- both ticked (outer right) - also switch
                  "bothTicked"
                  (Proj Snd (Variable "bothTicked"))
                )
              )  
            )
          )
        )
      )
    )

-- bad bad = fix r . advAL r
-- 
badTermBad :: Term
badTermBad = error "TODO"

badTermBadType :: Empty
badTermBadType = error "TODO"