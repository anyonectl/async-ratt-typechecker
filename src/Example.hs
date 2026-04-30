module Example where

import Syntax

-- In haskell type annotation it would be
-- Box (a -> b) -> Sig a -> Sig b
mapTypeOld :: Type
mapTypeOld = Fun (Box (Fun Nat Nat)) (Fun (Sig Nat) (Sig Nat))

-- Feedback
-- It is missing the f in the recursive call.

-- map = fix r .λf .λs. let x = head s in let xs = tail s in
--         unbox f x :: delay_cl(xs) (adv_AL r f (adv xs))
mapTermOld :: Term
mapTermOld =
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


-- -- -- shorthand start -- -- -- 
-- TODO: Should we expand this?
-- TODO: Shorthand should be moved to Syntax.hs if we decide to use it

-- Function shorthand
(-->) :: Type -> Type -> Type
(-->) = Fun 
infixr 1 --> -- number is precidence over other operators, and r/l after infix makes it non-chainable

-- Variable shorthand
var :: Var -> Term
var = Variable

-- Application as operator (left-associative, like function application)
(@@) :: Term -> Term -> Term
(@@) = App
infixl 9 @@

-- Lambda shorthand
lam :: Var -> Term -> Term
lam = Lam

-- Let shorthand  
(.:) :: (Var, Term) -> Term -> Term
(.:) (x, t1) t2 = Let x t1 t2
infixr 1 .:

-- Anno shorthand
(:.:) :: Term -> Type -> Term
(:.:) t ty = Anno t ty
infixl 9 :.:

(.-.) :: Term -> Term -> Term -> Term 
(.-.) h t = Cons h t
infixl 9 .-.

-- -- -- shorthand stop -- -- -- 

-- map
--
let map = mapTerm :.: mapType in
  let mkSig chan = mkSigTerm :.: mkSigType in
    map Box(Lam (Variable x) ("x" + 1)) mkSig natChan

mkSigType :: Type
mkSigType = 

mapType :: Type
mapType = Box (Nat --> Nat) --> Sig Nat --> Sig Nat

-- map = fix r .λf .λs. let x = head s in let xs = tail s in
--       unbox f x :: delay_cl(xs) (adv_AL r f (adv xs))
mapTerm :: Term
mapTerm =
  Fix "r" $ lam "f" $ lam "s" $
    ("x",  Head (var "s")) .: -- let x = head s in
    ("xs", Tail (var "s")) .: -- let xs = tail s in
        (Unbox (var "f") @@ var "x") -- unbox f x
        .-. (Delay (Clock (var "xs")) -- delay_cl(xs)
          (AdvAL (var "r") @@ var "f" @@ Adv (var "xs"))) -- (adv_AL r (adv xs)

-- map bad does not take a stable function
mapTermBadType :: Type
mapTermBadType = (Nat --> Nat) --> Sig Nat --> Sig Nat

mapTermBad :: Term
mapTermBad =
  Fix "r" $ lam "f" $ lam "s" $
    ("x",  Head (var "s")) .:
    ("xs", Tail (var "s")) .:
      Cons
        (var "f" @@ var "x") -- f applied directly, not unboxed
        (Delay (Clock (var "xs"))
          (AdvAL (var "r") @@ var "f" @@ Adv (var "xs"))) -- f crosses tick here!

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