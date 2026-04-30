module Example where

import Syntax

-- map = fix r .λf .λs. let x = head s in let xs = tail s in
--         unbox f x :: delay_cl(xs) (adv_AL r f (adv xs))
mapTerm :: Term
mapTerm =
  Fix "r" -- r: recurse on map
    (Lam "f" -- f: function argument for map
      (Lam "s" -- s: the signal
        (Let "x"  (Head (Variable "s")) -- x: head of s
          (Let "xs" (Tail (Variable "s")) -- xs: tail of s
            (Cons -- construct a new signal
              (App -- apply f (which is stable) to the head
                (Unbox (Variable "f"))
                (Variable "x"))
              (Delay -- cast the tail of s to be EL
                (Clock (Variable "xs"))
                (App -- recurse with 
                  (App (AdvAL (Variable "r")) (Variable "f"))
                  (Adv (Variable "xs"))
                )
              )
            )
          )
        )
      )
    )

mapType :: Type
mapType = Box (Nat -->  Nat) --> Sig Nat --> Sig Nat

-- map bad does not take a stable function
mapTermBadType :: Type
mapTermBadType = (Nat --> Nat) --> Sig Nat --> Sig Nat

mapTermBad :: Term
mapTermBad =
  Fix "r" $ lam "f" $ lam "s" $
    let_ "x"  (Head (var "s")) $
    let_ "xs" (Tail (var "s")) $
      Cons
        (var "f" @@ var "x") -- f is not unboxed
        (Delay (Clock (var "xs"))
          (AdvAL (var "r") @@ var "f" @@ Adv (var "xs")))

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
switchType :: Type
switchType = Fun (Sig Nat) (Fun (EL (Sig Nat)) (Sig Nat))

switchType' :: Type
switchType' = Sig Nat --> EL (Sig Nat) --> Sig Nat

-- Are we lacking some type of boxing? Since the body of Fix must be typed in the stable context
-- Or if we're typing switchTerm in the empty context, then that context is already stable by definition...
switchTerm :: Term
switchTerm =
  Fix "recurse" -- type of "recurse" is `AL (Sig Nat -> (EL (Sig Nat) -> Sig Nat))`
    (Lam "s" -- original signal (first argument)
      (Lam "ys" -- delayed signal (second argument) (type: EL (Sig Nat))
        (Let "x" -- head of original signal s, `Nat`
          (Head (Variable "s"))
          (Let "xs" -- tail of original signal s (type: EL (Sig Nat))
            (Tail (Variable "s"))
            (Cons
              (Variable "x")
              (Delay -- result of delay is `EL (Sig Nat)` (on union clock), so `"x" : result of delay` has type Sig Nat (what switch returns)
                (ClockUnion
                  (Clock (Variable "xs"))
                  (Clock (Variable "ys")))
                (Case -- result of this Case has type `Sig Nat`
                  (Select (Variable "xs") (Variable "ys")) -- result has 3-way sum type: ((Sig Nat × EL (Sig Nat)) + (EL (Sig Nat) × Sig Nat)) + (Sig Nat × Sig Nat)
                  
                  -- outer left
                  -- (clock of xs ticked) + (clock of ys ticked)
                  "oneTicked" -- type: -- ((Sig Nat × EL (Sig Nat)) + (EL (Sig Nat) × Sig Nat))
                  (Case 
                    (Variable "oneTicked")

                    -- xs ticked first (inner left)
                    "left" -- type: Sig Nat × EL (Sig Nat)
                    (App
                      (App
                        (AdvAL (Variable "recurse")) -- type: Sig Nat -> (EL (Sig Nat) -> Sig Nat)) (advAL 'removes' AL)
                        (Proj Fst (Variable "left"))) -- xs, type: Sig Nat. We pass this to the function above and get type `EL (Sig Nat) -> Sig Nat`
                      (Proj Snd (Variable "left"))) -- ys, type: EL (Sig Nat). Pass this to the function above. The new argument s' is the previous s WITHOUT the head x, but ys is the same
                    
                    -- ys ticked first (inner right) - switch! (no recursion, just return ys)
                    "right" -- type: -- EL (Sig Nat) × Sig Nat
                    (Proj Snd (Variable "right"))) -- type: Sig Nat 

                  -- both ticked (outer right) - switch!
                  "bothTicked" -- type: Sig Nat × Sig Nat
                  (Proj Snd (Variable "bothTicked")) -- we choose Snd to switch to ys
                )
              )  
            )
          )
        )
      )
    )

-- swapped the clocks of xs and ys in ClockUnion
-- this should fail in preTickOnSpecificClock during infer on Select (with error 'NoTickInContext')
-- because ClockUnion (Clock ys) (Clock xs) == ClockUnion (Clock xs) (Clock ys) = false (they are not structurally equal)
switchBad :: Term
switchTermBad =
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

badTermBadType :: Type
badTermBadType = Empty



-- -- -- Trying it our as programs -- -- -- 

-- A nat channel to read from
natChan :: Chan
natChan = Push Nat

-- Increment function as a term: λx. suc x
incr :: Term
incr = Lam "x" (Suc (var "x"))

-- The full program:
-- map (box (λx. suc x)) (wait natChan)
mapProgram :: Term
mapProgram =
  Anno
    (mapTerm
      @@ Always incr          -- box (λx. suc x) : □(Nat → Nat)
      @@ Wait natChan)        -- wait_κ           : ⊙Nat
    mapProgramType            -- expected result type

mapProgramType :: Type
mapProgramType = Sig Nat

mkSigTerm :: Type
mkSigTerm = error "TODO"
