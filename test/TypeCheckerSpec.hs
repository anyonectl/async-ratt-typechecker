{-# LANGUAGE BlockArguments #-}
module TypeCheckerSpec where

import Test.Hspec
import TypeChecker
import Syntax
import Context
import ErrorTypes

spec :: Spec
spec = describe "TypeChecker" $ do

  let tickedCtx = Ctx ([], [( Clock Zero, [("x", Nat)] )])
  let simpleCtx = Ctx ([ ("x", Nat) ], [])
  let emptyCtx  = Ctx ([], [])

  describe "Variables" $ do
    it "infers variable in simple context" $
      infer simpleCtx (Variable "x") `shouldBe` Right Nat

    it "infers variable in ticked context" $
      infer tickedCtx (Variable "x") `shouldBe` Right Nat

    it "returns UnboundVar for missing variable" $
      infer simpleCtx (Variable "y") `shouldBe` Left (CtxError (VariableNotFound "y"))

    it "returns EmptyContext error on empty context" $
      infer emptyCtx (Variable "x") `shouldBe` Left (CtxError (ContextIsEmpty "cannot lookup in empty context"))

  let projTickedCtx = Ctx ([], [(Clock Zero, [("x", Prod Nat Nat)])])
  let projSimpleCtx = Ctx ([("x", Prod Nat Nat)], [])

  describe "Projection" $ do
    it "Fst projects the first component of a product" $
      infer projTickedCtx (Proj Fst (Variable "x")) `shouldBe` Right Nat

    it "Snd projects the second component of a product" $
      infer projTickedCtx (Proj Snd (Variable ("x"))) `shouldBe` Right Nat

    it "Proj fails on a non-product type" $
      infer projTickedCtx (Proj Fst (Variable ("y"))) `shouldBe` Left (CtxError (VariableNotFound "y"))


  describe "Pair" $ do
    let populatedCtx = 
                                              Ctx([
                                                ("x", Nat)
                                              , ("y", Nat)
                                              , ("z", Prod Nat Nat)
                                              , ("a", Sum Nat Nat)
                                              , ("f", Fun Nat Nat)]
                                              , [])

    it "Pair infers Prod type from context" $
      infer populatedCtx (Pair (Variable "x") (Variable "y")) 
        `shouldBe` Right (Prod Nat Nat)

    it "Pair fails on looking up variable in context" $
      infer populatedCtx (Pair (Variable "x") (Variable "notInContext")) 
        `shouldBe` Left (CtxError (VariableNotFound "notInContext"))

    it "Pair check fails because given wrong type" $
      check populatedCtx (Pair (Variable "x") (Variable "y")) (Sum Nat Nat) 
        `shouldBe` Left (TypeMismatch { term = (Pair (Variable "x") (Variable "y")), expected = (Sum Nat Nat), got = (Prod Nat Nat)})
  
  describe "Adv" $ do
    let advCtx = Ctx ([("x", EL Nat)], ([(Clock (Variable "x"), [("x", EL Nat)])]))

    it "Adv extracts A from EL A when tick is in context" $
      infer advCtx (Adv (Variable "x")) `shouldBe` Right Nat

    it "Adv fails when argument id not value" $
      let t = Adv (App (Variable "x") UnitVal) 
      in infer advCtx t
        `shouldBe` Left (ExpectedValue t)

    it "Adv extracts nested EL A from EL (EL A) when tick is in context" $
      let t            = Adv (Variable "e")
          advNestedCtx = Ctx ([], ([(Clock (Variable "e"), [("e", EL (EL Nat))])]))
      in infer advNestedCtx t `shouldBe` Right (EL Nat)

    it "Adv fails when no tick in context" $
      let t         = Adv (Variable "x")
          noTickCtx = Ctx ([("x", EL Nat)], [])
      in infer noTickCtx t
        `shouldBe` Left (CtxError NoTickInContext)

    it "Adv fails when tick is on wrong clock" $
      let wrongClockCtx = Ctx ([("x", EL Nat)], ([(Clock (Variable "y"), [("x", EL Nat)])]))
      in infer wrongClockCtx (Adv (Variable "x"))
        `shouldBe` Left (CtxError NoTickInContext)

    it "Adv fails when v does not have a delayed type" $
      let wrongTypeCtx = Ctx ([("x", Nat)], ([(Clock (Variable "x"), [("x", Nat)])]))
      in infer wrongTypeCtx (Adv (Variable "x"))
        `shouldBe` Left (ExpectedEL { term = Adv (Variable "x"), got = Nat })

  describe "AdvAL" $ do
    it "AdvAL fails when no tick in context" $
      let noTickCtx = Ctx ([("x", EL Nat)], [])
      in infer noTickCtx (AdvAL (Variable "x"))
        `shouldBe` Left (CtxError NoTickInContext)

    it "AdvAL extracts A from AL A when tick is other clock" $
      let otherClockCtx = Ctx ([("x", EL Nat)], ([(Clock (Variable "other"), [("x", AL Nat)])]))
      in infer otherClockCtx (AdvAL (Variable "x"))
        `shouldBe` Right Nat

  describe "Select" $ do

    let selectCorrect = Right (Sum (Sum (Prod Nat (EL Nat)) (Prod (EL Nat) Nat)) (Prod Nat Nat))

    it "Select extracts (A1 x EL A2) + (EL A1 x A2) + (A1 x A2) when tick single variable"
      let ctx = Ctx ([], ([(Clock (Variable "a"), [("a", EL Nat), ("b", EL Nat)])]))
      in infer ctx (Select (Variable "a") (Variable "b")) 
      `shouldBe` selectCorrect

    it "Select extracts (A1 x EL A2) + (EL A1 x A2) + (A1 x A2) when tick is on union of clocks"
      let ctx = Ctx ([], [ ((ClockUnion (Clock (Variable "a")) (Clock (Variable "b")))
                                   , [("a", EL Nat), ("b", EL Nat)])])      
      in infer ctx (Select (Variable "a") (Variable "b")) 
      `shouldBe` selectCorrect

    it "Select fails when variable not in latest ticked"
      let ctx = Ctx ([], [ (Clock Zero,            [("a", EL Nat)])
                                 , (Clock (Variable "a"),  [("a", EL Nat), ("b", EL Nat)]) ])      
      in infer ctx (Select (Variable "a") (Variable "b")) 
      `shouldBe` Left (CtxError NoTickInContext)

