{-# LANGUAGE BlockArguments #-}
module UtilsSpec where

import Test.Hspec

import TypeChecker
import Syntax
import SignalCombinators
import Context
import ErrorTypes
import Utils

spec :: Spec
spec = describe "Utils" $ do

  let emptyCtx = Ctx ([], [])

  it "sigAwait typechecks" $
    check emptyCtx (sigAwaitTerm (Push Nat)) sigAwaitType
      `shouldBe` Right sigAwaitType

  describe "eliminateEL" $ do
    let t          = Anno (eliminateEL (Variable "x")) eliminateELType
        simpleCtx  = Ctx ([("x", EL Nat)], [])
        simpleCtx' = Ctx ([("x", Nat)], [])

    -- well-typed: x : EL Nat in context, eliminateEL extracts the Nat
    it "infers Nat when x : EL Nat is in context" $
      infer simpleCtx t `shouldBe` Left (IncompatibleType { term = Delay (Clock (Variable "x")) (Adv (Variable "x")), expected = EEL, got = Nat})

    -- ill-typed: x : Nat is not a delayed computation, Adv cannot consume it
    it "fails when x : Nat (not EL Nat) is in context" $
      infer simpleCtx' t `shouldBe` Left (IncompatibleType { term = Delay (Clock (Variable "x")) (Adv (Variable "x")), expected = EEL, got = Nat})

    -- ill-typed: x is not bound at all
    it "fails when x is not in context" $
      infer emptyCtx t `shouldBe` Left (CtxError (VariableNotFound ("x")))