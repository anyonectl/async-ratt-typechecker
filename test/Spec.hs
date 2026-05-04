import Test.Hspec
import TypeCheckerSpec
import ContextSpec
import SignalCombinatorsSpec

main :: IO ()
main = hspec $ do
    TypeCheckerSpec.spec
    ContextSpec.spec
    SignalCombinatorsSpec.spec
    -- add more test files (remember to also add imports)