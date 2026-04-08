module TypeCheckerSpec where

import Test.Hspec
import TypeChecker
import Syntax
import Context

spec :: Spec
spec = describe "TypeChecker" $ do

  let tickedCtx = Ctx ([], [([ (Var "x", Nat) ], Clock Zero)])
  let simpleCtx = Ctx ([ (Var "x", Nat) ], [])
  let emptyCtx  = EmptyCtx

  -- infer
  describe "infer" $ do

    -- variables
    it "infers variable in simple context" $
      infer simpleCtx (Variable (Var "x")) `shouldBe` Right Nat

    it "infers variable in ticked context" $
      infer tickedCtx (Variable (Var "x")) `shouldBe` Right Nat

    it "returns UnboundVar for missing variable" $
      infer simpleCtx (Variable (Var "y")) `shouldBe` Left (ContextError (VariableNotFound (Var "y")))

    it "returns EmptyContext error on empty context" $
      infer emptyCtx (Variable (Var "x")) `shouldBe` Left (ContextError (ContextIsEmpty "cannot lookup in empty context"))

  -- check
  describe "check" $ do

    -- variables
    it "checks variable in simple context against correct type" $
      check simpleCtx (Variable (Var "x")) Nat `shouldBe` Right Nat

    it "fails when checking variable against wrong type" $
      check simpleCtx (Variable (Var "x")) bool `shouldBe` Left (TypeMismatch { expected = bool, got = Nat })

    it "checks variable in ticked context against correct type" $
      check tickedCtx (Variable (Var "x")) Nat `shouldBe` Right Nat