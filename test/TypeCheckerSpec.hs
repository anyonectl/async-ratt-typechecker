{-# LANGUAGE BlockArguments #-}
module TypeCheckerSpec where

import Test.Hspec
import TypeChecker
import Syntax
import Context
import ErrorTypes

spec :: Spec
spec = describe "TypeChecker.hs" $ do
  describe "Variable / lookup in context" $ do
    let simpleCtx = Ctx ([ ("x", Nat) ], [])
        x         = Variable "x"

    it "returns UnboundVar for missing variable" $
      infer simpleCtx (Variable "y") 
        `shouldBe` Left (CtxError (VariableNotFound "y"))

    it "infers variable in simple context" $
      infer simpleCtx x `shouldBe` Right Nat

    it "infers variable in ticked context" $
      let tickedCtx = Ctx ([], [( Clock Zero, [("x", Nat)] )])
      in infer tickedCtx x `shouldBe` Right Nat

    it "check defaults to infer on correct type" $ do
      check simpleCtx x Nat `shouldBe` Right Nat

  describe "Projection ()" $ do
    let projTickedCtx = Ctx ([], [(Clock Zero, [("x", Prod Nat Nat)])])
    it "fst projects the first component of a product" $
      infer projTickedCtx (Proj Fst (Variable "x")) `shouldBe` Right Nat

    it "snd projects the second component of a product" $
      infer projTickedCtx (Proj Snd (Variable ("x"))) `shouldBe` Right Nat

    it "check fails when given wrong type" $
      let t  = Proj Fst (Variable "x")
          ty = Sum Nat Nat 
      in check projTickedCtx t ty 
        `shouldBe` Left (TypeCheckFailed { term = t, checked = ty, inferred = Nat })

  describe "Pair" $ do
    let pairCtx = Ctx([ ("x", Nat)
                        , ("y", Nat)
                        , ("z", Prod Nat Nat)
                        , ("a", Sum Nat Nat)
                        , ("f", Fun Nat Nat)]
                        , []) -- no ticks

    it "infers Prod type from context" $
      infer pairCtx (Pair (Variable "x") (Variable "y")) 
        `shouldBe` Right (Prod Nat Nat)

    it "fails on looking up non-existent variable in context" $
      infer pairCtx (Pair (Variable "x") (Variable "notInContext")) 
        `shouldBe` Left (CtxError (VariableNotFound "notInContext"))

    it "check fails when given wrong type" $
      check pairCtx (Pair (Variable "x") (Variable "y")) (Sum Nat Nat) 
        `shouldBe` Left (IncompatibleType { term = Pair (Variable "x") (Variable "y"), expected = EProd, got = Sum Nat Nat})
  
  describe "Advance (EL)" $ do
    let advCtx = Ctx ([("x", EL Nat)], [(Clock (Variable "x"), [("x", EL Nat)])])

    it "correctly extracts A from EL A when tick is in context" $
      infer advCtx (Adv (Variable "x")) `shouldBe` Right Nat

    it "fails when argument id not value" $
      let v = App (Variable "x") UnitVal
          t = Adv v
      in infer advCtx t
        `shouldBe` Left IncompatibleTerm { term = t , expected = EValue, gotT = v}

    it "correctly extracts nested EL A from EL (EL A) when tick is in context" $
      let t            = Adv (Variable "e")
          advNestedCtx = Ctx ([], ([(Clock (Variable "e"), [("e", EL (EL Nat))])]))
      in infer advNestedCtx t `shouldBe` Right (EL Nat)

    it "fails when no tick in context" $
      let t         = Adv (Variable "x")
          noTickCtx = Ctx ([("x", EL Nat)], [])
      in infer noTickCtx t
        `shouldBe` Left (CtxError NoTickInContext)

    it "fails when tick is on wrong clock" $
      let wrongClockCtx = Ctx ([("x", EL Nat)], ([(Clock (Variable "y"), [("x", EL Nat)])]))
      in infer wrongClockCtx (Adv (Variable "x"))
        `shouldBe` Left (CtxError NoTickInContext)

    it "fails when v does not have a delayed type" $
      let t = Adv (Variable "x")
          wrongTypeCtx = Ctx ([("x", Nat)], ([(Clock (Variable "x"), [("x", Nat)])]))
      in infer wrongTypeCtx t
        `shouldBe` Left (IncompatibleType { term = t, expected = EEL , got = Nat })

  describe "Advance AL" $ do
    it "fails when no tick in context" $
      let noTickCtx = Ctx ([("x", EL Nat)], [])
      in infer noTickCtx (AdvAL (Variable "x"))
        `shouldBe` Left (CtxError NoTickInContext)

    it "correctly extracts A from AL A when tick is other clock" $
      let otherClockCtx = Ctx ([("x", EL Nat)], ([(Clock (Variable "other"), [("x", AL Nat)])]))
      in infer otherClockCtx (AdvAL (Variable "x"))
        `shouldBe` Right Nat

  describe "Select EL EL" $ do
    let selectTy = Right (Sum (Sum (Prod Nat (EL Nat)) 
                                            (Prod (EL Nat) Nat)) 
                                       (Prod Nat Nat))

    it "correctly extracts three-way sum type when tick single variable"
      let ctx = Ctx ([], ([(Clock (Variable "a"), [("a", EL Nat), ("b", EL Nat)])]))
      in infer ctx (Select (Variable "a") (Variable "b")) 
      `shouldBe` selectTy

    it "correctly extracts three-way sum type when tick is on union of clocks"
      let ctx = Ctx ([], [ ((ClockUnion (Clock (Variable "a")) (Clock (Variable "b")))
                                   , [("a", EL Nat), ("b", EL Nat)])])      
      in infer ctx (Select (Variable "a") (Variable "b")) 
      `shouldBe` selectTy

    it "fails when tick is on union of clocks in wrong order"
      let ctx = Ctx ([], [ ((ClockUnion (Clock (Variable "b")) (Clock (Variable "a")))
                                   , [("a", EL Nat), ("b", EL Nat)])])      
      in infer ctx (Select (Variable "a") (Variable "b")) 
      `shouldBe` Left (CtxError NoTickInContext)

    it "fails when variable not in latest ticked"
      let ctx = Ctx ([], [ (Clock Zero, [("a", EL Nat)])
                         , (Clock (Variable "a"),  [("a", EL Nat), ("b", EL Nat)]) ])      
      in infer ctx (Select (Variable "a") (Variable "b")) 
      `shouldBe` Left (CtxError NoTickInContext)

  describe "Delay EL" $ do
    let ctx  = Ctx ([("x", Nat)], [])      -- addTick cl("x") ctx = Ctx ([],[((Clock (Variable "x")), [("x", Nat), ])])
        x    = (Variable "x")
    it "pass in valid context" $ do
      let t = Delay (Clock x) x
      infer ctx t `shouldBe` Right (EL Nat)
      
    it "pass with annotated EL type in valid context" $ do
      let t = Anno (Delay (Clock x) x) (EL Nat)
      infer ctx t `shouldBe` Right (EL Nat)

    it "fails with wrong annotation: not EL type" $ do
      let t = Delay (Clock x) x
      check ctx t Nat `shouldBe` Left (IncompatibleType { term = t, expected = EEL, got = Nat})
  
    it "body is checked for wrong variable" $ do
      let t = Delay (Clock (Variable "y")) (Variable "y")
      check ctx t (EL Nat) `shouldBe` Left (CtxError (VariableNotFound "y"))

    it "clock on different term fails" $ do
      let w = Wait (inputChans !! 0)
          t = Delay (Clock (w)) x 
      check ctx t (EL Nat) `shouldBe` Left (OtherError "Clock mismatch")
      -- ctx' = Ctx ([],[((Clock (k)), [("x", EL Nat)])])
      -- check ctx' (Variable "x") Nat
      -- lookupType "x" ctx'
      -- Unstable? 

  describe "Unbox" $ do
    let simpleCtx     = Ctx ( [("b", Box Nat), ("n", Nat), ("f", Fun Nat Nat)], [])
    let tickedCtx     = Ctx ( [], [ ((Clock Zero), []) 
                                  , ((Clock Zero), [])
                                  , ((Clock Zero), [("last", Box Nat)])
                                  ])
    
    it "infer passes when type is boxed" $ do
      infer simpleCtx (Unbox (Variable "b")) `shouldBe` Right Nat 
    
    it "infer fails when type is not boxed" $ do
      let t = Unbox (Variable "n")
      infer simpleCtx t `shouldBe` Left (IncompatibleType { term = t, expected = EBox, got = Nat })
    
    it "check passes when given Box type" $ do
      let t = Unbox (Variable "b")
      check simpleCtx t Nat `shouldBe` Right Nat
    
    it "infer passes when given Box type from ticked context" $ do
      let t = Unbox (Variable "last")
      infer tickedCtx t `shouldBe` Right Nat
    
    it "check fails when inferred type doesn't match checked type" $ do
      let t = Unbox (Variable "b")  -- infers to Nat
      check simpleCtx t (Fun Nat Nat)
        `shouldBe` Left (TypeCheckFailed { term = t, checked = Fun Nat Nat, inferred = Nat })
    
    it "infer fails when variable not in context" $ do
      infer simpleCtx (Unbox (Variable "z"))
        `shouldBe` Left (CtxError (VariableNotFound "z"))
    
    it "infer fails when type is a Fun, not Box" $ do
      let t = Unbox (Variable "f")
      infer simpleCtx t
        `shouldBe` Left (IncompatibleType { term = t, expected = EBox, got = Fun Nat Nat })


  describe "Always (box)" $ do 
    let simpleCtx     = Ctx ( [("x", Box Nat), ("y", Nat), ("f", Fun Nat Nat)], [])
    let tickedCtx     = Ctx ( [], [ ((Clock (Variable "x")), [("x", Box Nat), ("y", Nat), ("f", Fun Nat Nat)]) 
                                  , ((Clock Zero),           [])
                                  , ((Clock Zero),           [("x", Nat)])
                                  ])
    it "infer type when context is tick-free" $
      let t = Always (Variable "y")
      in infer simpleCtx t `shouldBe` Right (Box Nat)
    
    it "infer type when context is tick-free" $
      let t = Always (Variable "x")
      in infer simpleCtx t `shouldBe` Right (Box (Box Nat))
    
    it "infer fails when variable has non-stable type" $
      let t = Always (Variable "f")
      in infer simpleCtx t
        `shouldBe` Left (CtxError (VariableNotFound "f"))

    it "infer fails for non-stable variable in ticked context" $
      let t = Always (Variable "f") 
      in infer tickedCtx t
        `shouldBe` Left (CtxError (VariableNotFound "f")) -- Fun Nat Nat is stripped from context

    it "check passes when checked against Box type" $
      let t = Always (Variable "x")
      in check simpleCtx t (Box (Box Nat)) `shouldBe` Right (Box (Box Nat)) -- super boxed

    it "check fails when checked against non-Box type" $
      let t = Always (Variable "y")
      in check simpleCtx t Nat
        `shouldBe` Left (IncompatibleType { term = t, expected = EBox, got = Nat })

  describe "Case" $ do
    let sumTy     = Sum Nat Unit
        inlZero   = Anno (Inj InLeft  Zero)    sumTy  -- left  0  : Nat + Unit
        inrUnit   = Anno (Inj InRight UnitVal) sumTy  -- right () : Nat + Unit
        emptyCtx  = Ctx ([], [])

    it "infers type when both branches have the same type" $
      -- case (inl 0) of left x -> x | right y -> 0
      -- left  branch: x : Nat  |- x => Nat
      -- right branch: y : Unit |- 0 => Nat
      infer emptyCtx (Case inlZero "x" (Variable "x") "y" Zero)
        `shouldBe` Right Nat

    it "checks correctly when expected type matches both branches" $
      check emptyCtx (Case inlZero "x" (Variable "x") "y" Zero) Nat
        `shouldBe` Right Nat

    it "infers type when branches return a product" $
      -- left  branch: x : Nat  |- (x, 0) => Nat x Nat
      -- right branch: y : Unit |- (0, 0) => Nat x Nat
      infer emptyCtx (Case inrUnit "x" (Pair (Variable "x") Zero) "y" (Pair Zero Zero))
        `shouldBe` Right (Prod Nat Nat)

    it "fails when argument is not a sum type" $
      infer emptyCtx (Case Zero "x" (Variable "x") "y" Zero)
        `shouldBe` Left (IncompatibleType { term = Zero, expected = ESum, got = Nat })

    it "fails when argument variable is not in context" $
      infer emptyCtx (Case (Variable "z") "x" Zero "y" Zero)
        `shouldBe` Left (CtxError (VariableNotFound "z"))

    it "fails when branches have different types" $
      -- left  branch: x : Nat  |- x => Nat
      -- right branch: y : Unit |- y => Unit   (mismatch!)
      let t = Case inlZero "x" (Variable "x") "y" (Variable "y")
      in infer emptyCtx t
        `shouldBe` Left (TypeCheckFailed
                          { term     = Variable "y"
                          , checked  = Nat
                          , inferred = Unit
                          })

    it "fails when right branch has wrong type even though both branches are well-typed alone" $
      -- left  branch: x : Nat  |- 0       => Nat
      -- right branch: y : Unit |- UnitVal => Unit  (well-typed, but mismatches Nat)
      let t = Case inlZero "x" Zero "y" UnitVal
      in infer emptyCtx t
        `shouldBe` Left (TypeCheckFailed
                          { term     = UnitVal
                          , checked  = Nat
                          , inferred = Unit
                          })

    it "left branch variable is not in scope in right branch" $
      -- left  branch: x : Nat  |- x   => Nat
      -- right branch: y : Unit |- x   => error: x not in scope
      let t = Case inlZero "x" (Variable "x") "y" (Variable "x")
      in infer emptyCtx t
        `shouldBe` Left (CtxError (VariableNotFound "x"))

    it "right branch variable is not in scope in left branch" $
      let t = Case inlZero "x" (Variable "y") "y" (Variable "y")
      in infer emptyCtx t
        `shouldBe` Left (CtxError (VariableNotFound "y"))

    it "bound variables in each branch correctly shadow outer context" $
      -- outer ctx has x : Unit; branch re-binds x : Nat from the sum
      let ctx = Ctx ([("x", Unit), ("y", Unit)], [])
          t   = Case inlZero "x" (Variable "x") "y" Zero
      in infer ctx t `shouldBe` Right Nat 

    it "infers correctly for nested case expressions" $
      -- case (inl 0) of
      --   left  x -> case (inl 0) of
      --                left  a -> a
      --                right b -> 0 
      --   right y -> 0 
      let inner = Case inlZero "a" (Variable "a") "b" Zero
          outer = Case inlZero "x" inner "y" Zero
      in infer emptyCtx outer `shouldBe` Right Nat