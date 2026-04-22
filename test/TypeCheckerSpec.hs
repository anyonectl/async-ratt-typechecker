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

  describe "Adv" $ do

    let advCtx = Ctx ([("x", EL Nat)], [(Clock Zero, [("x", EL Nat)])])

    it "adv extracts A from EL A when tick is in context" $
      infer advCtx (Adv (Variable "x")) `shouldBe` Right Nat

    it "adv fails when v is not a value" $
      infer advCtx (Adv (App (Variable "x") UnitVal)) `shouldBe` Left (ExpectedValue (Adv (App (Variable "x") UnitVal)))

    let noTickCtx = Ctx ([("x", EL Nat)], [])
    it "adv fails when no tick in context" $
      infer noTickCtx (Adv (Variable "x")) `shouldBe` Left (CtxError NoTickInContext)

    let wrongTypeCtx = Ctx ([("x", Nat)], [(Clock Zero, [("x", Nat)])])
    it "adv fails when v does not have a delayed type" $
      infer wrongTypeCtx (Adv (Variable "x"))
        `shouldBe` Left (ExpectedEL { term = Adv (Variable "x"), got = Nat })