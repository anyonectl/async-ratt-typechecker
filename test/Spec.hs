import Test.Hspec
import TypeCheckerSpec
import SignalCombinatorsSpec

main :: IO ()
main = hspec $ do
    TypeCheckerSpec.spec
    SignalCombinatorsSpec.spec
    -- add more test files (remember to also add imports)