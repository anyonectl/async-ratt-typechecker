import Test.Hspec
import TypeCheckerSpec
import ContextSpec
import ExampleSpec

main :: IO ()
main = hspec $ do
    TypeCheckerSpec.spec
    ContextSpec.spec
    ExampleSpec.spec
    -- add more test files (remember to also add imports)