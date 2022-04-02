import Test.Tasty
import Parsing.Tests (parserTests)
import Evaluation.Tests (evalTests)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests, evalTests]
