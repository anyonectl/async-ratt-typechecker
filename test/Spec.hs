import Test.Hspec
import TypeCheckerSpec
import ContextSpec

main :: IO ()
main = hspec $ do
    TypeCheckerSpec.spec
    ContextSpec.spec
    -- add more test files (remember to also add imports)