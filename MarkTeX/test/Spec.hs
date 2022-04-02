import Test.Tasty
import Parsing.Tests (parserTests)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests]
