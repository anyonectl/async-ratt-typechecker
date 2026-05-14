module SignalCombinatorsSpec where

import Test.Hspec

import TypeChecker
import Syntax
import SignalCombinators
import Context
import ErrorTypes

spec :: Spec
spec = describe "SignalCombinators" $ do

  let ctx = Ctx ([], [])

  describe "map" $ do

    it "map typechecks with boxed f" $
      check ctx mapTerm mapType `shouldBe` Right mapType

    it "map infers correctly to its type using annotation" $
      infer ctx (Anno mapTerm mapType) `shouldBe` Right mapType

    it "mapbad fails: unboxed f crosses tick boundary" $
      check ctx mapTermBad mapTermBadType
        `shouldBe` Left (CtxError (VariableNotFound "f"))


    --- ... 