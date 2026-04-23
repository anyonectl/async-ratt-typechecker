module ExampleSpec where

import Test.Hspec

import TypeChecker
import Syntax
import Example
import Context
import ErrorTypes

spec :: Spec
spec = describe "Examples" $ do

  let ctx = (Ctx ([], []))

  describe "map" $ do

    it "map typechecks with boxed f" $
      check ctx mapTerm mapType
        `shouldBe` Right mapType

    it "mapbad fails: unboxed f crosses tick boundary" $
      check ctx mapTermBad mapTermBadType
        `shouldBe` Left (CtxError (UnstableType "f" (Nat --> Nat)))

    --- ... 