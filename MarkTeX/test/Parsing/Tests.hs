module Parsing.Tests where
import Parsing.Predicates

import MarkTeX.Parsing.Expression (Expr(..), RootExpr(..))
import MarkTeX.Parsing.Parser (parseMd)

import Test.Tasty
import Test.Tasty.HUnit
import GHC.IO (unsafePerformIO)
import System.FilePath ((</>))

data ParseTest = ParseTest String FilePath [(String, ParserPredicate)]

parserTests :: TestTree
parserTests = testGroup "Parser" (map parseTestToTestTree parseTests)

parseTests :: [ParseTest]
parseTests = [
        ParseTest "Basic tags" "basic_tags.md" [
            ("Bold", containsEPredicate (Bold (Text "Bold"))),
            ("Italic", containsEPredicate (Italic (Text "Italic"))),
            ("Header", containsREPredicate (Heading 1 (Text "Header"))),
            ("Header2", containsREPredicate (Heading 2 (Text "Header2"))),
            ("Hyperlink", containsEPredicate (Hyperlink (Text "LinkText") (Text "LinkUrl"))),
            ("Image", containsEPredicate (Image (Text "AltText") (Text "ImageUrl")))
        ], ParseTest "Code tags" "code_tags.md" [
            ("Simple command", containsEPredicate (CommandCode "SimpleCommand")),
            ("Block command", containsREPredicate (CommandBlockCode "BlockCommand" emptyRootExpr))
        ], ParseTest "Lists" "lists.md" [
            ("Unordered", containsREPredicate (UnorderedList [Text "ULItem1", Text "ULItem2"])),
            ("Ordered", containsREPredicate (OrderedList [Text "OLItem1", Text "OLItem2"]))
        ]
    ]

parseTestToTestTree :: ParseTest -> TestTree
parseTestToTestTree (ParseTest str file ps) = testGroup str tests where
    expr :: RootExpr
    expr = parseMd $ unsafePerformIO (readFile ("test/Parsing/inputs/" </> file))
    tests :: [TestTree]
    tests = map (\(str', p) -> testCase str' (True @=? p expr)) ps

emptyExpr :: Expr
emptyExpr = Seq []

emptyRootExpr :: RootExpr
emptyRootExpr = RootSeq []
