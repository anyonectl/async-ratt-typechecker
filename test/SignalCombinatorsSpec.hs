module SignalCombinatorsSpec where

import Test.Hspec

import TypeChecker
import Syntax
import SignalCombinators
import Context
import ErrorTypes

spec :: Spec
spec = describe "SignalCombinators.hs" $ do
  let ctx = Ctx ([], [])
  describe "map" $ do
    it "map infers correctly to its type using annotation" $
      infer ctx (Anno mapTerm mapType) `shouldBe` Right mapType
    it "mapbad fails: unboxed f" $
      infer ctx (Anno mapTermBad mapType) 
        `shouldBe` Left (IncompatibleType 
          { term     = App (Variable "f") (Variable "x"), 
            expected = EFun, 
            got      = Box(Fun Nat Nat)})
    it "mapbad fails: unboxed f crosses tick boundary" $
      check ctx mapTermBad mapTermBadType
        `shouldBe` Left (CtxError (VariableNotFound "f"))
    it "map with wrong clock" $
      infer ctx (Anno mapTermWrongClock mapType) 
        `shouldBe` Left (CtxError (NoTickInContext))
    it "map original" $
      infer ctx (Anno mapTermOrg mapType) `shouldBe` Right mapType

  describe "switch" $ do
    it "switch typechecks with correct type" $
      infer ctx (Anno switchTerm switchType) `shouldBe` Right switchType
    it "infer failes with clock in other order" $
      infer ctx (Anno switchTermBad switchType) `shouldBe` Left (CtxError (NoTickInContext))