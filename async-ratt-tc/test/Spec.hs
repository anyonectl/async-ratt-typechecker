import Test.Hspec
import TypeCheckerSpec

main :: IO ()
main = hspec $ do
    TypeCheckerSpec.spec
    -- add more test files (remember to also add imports)