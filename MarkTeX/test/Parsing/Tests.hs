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
            ("Bold", ContainsExpr (Bold (Text "Bold"))),
            ("Italic", ContainsExpr (Italic (Text "Italic"))),
            ("Header", ContainsRootExpr (Heading 1 (Text "Header"))),
            ("Header2", ContainsRootExpr (Heading 2 (Text "Header2"))),
            ("Hyperlink", ContainsExpr (Hyperlink (Text "LinkText") (Text "LinkUrl"))),
            ("Image", ContainsExpr (Image (Text "AltText") (Text "ImageUrl")))
        ], ParseTest "Code tags" "code_tags.md" [
            ("Simple command", ContainsExpr (CommandCode "SimpleCommand")),
            ("Inline simple commands", ContainsExpr (Seq [CommandCode "SimpleCommand1", CommandCode "SimpleCommand2"])),
            ("Block command", ContainsRootExpr (CommandBlockCode "BlockCommand" emptyRootExpr)),
            ("Inline block command", ContainsRootExpr (CommandBlockCode "BlockCommand2" emptyRootExpr))
        ], ParseTest "Code command blocks" "code_blocks.md" [
            ("Block with inner inline", ContainsExpr (Seq [Italic $ CommandCode "Command1In", CommandCode "Command2In"])),
            ("Block with inner newline", ContainsExpr (Seq [Italic $ CommandCode "Command1New", CommandCode "Command2New"]))
        ], ParseTest "Lists" "lists.md" [
            ("Unordered", ContainsRootExpr (UnorderedList [Text "ULItem1", Text "ULItem2"])),
            ("Ordered", ContainsRootExpr (OrderedList [Text "OLItem1", Text "OLItem2"]))
        ], ParseTest "Tags in lists" "tags_in_lists.md" [
            ("Tags in lists", ContainsRootExpr (UnorderedList [
                Bold (Text "Bold"), 
                Italic (Text "Italic"),
                Hyperlink (Text "LinkText") (Text "LinkUrl"),
                Image (Text "AltText") (Text "ImageUrl"),
                CommandCode "SimpleCommand"
            ]))
        ]
    ]

parseTestToTestTree :: ParseTest -> TestTree
parseTestToTestTree (ParseTest str file ps) = testGroup str tests where
    expr :: RootExpr
    expr = parseMd $ unsafePerformIO (readFile ("test/Parsing/inputs/" </> file))
    testToAssertion :: ParserPredicate -> Assertion
    testToAssertion (ContainsExpr e)     = assertBool ("Cannot find " ++ show e ++ ", got " ++ show expr) (containsEPredicate e expr)
    testToAssertion (ContainsRootExpr e) = assertBool ("Cannot find " ++ show e ++ ", got " ++ show expr) (containsREPredicate e expr)
    tests :: [TestTree]
    tests = map (\(str', p) -> testCase str' (testToAssertion p)) ps

emptyExpr :: Expr
emptyExpr = Seq []

emptyRootExpr :: RootExpr
emptyRootExpr = RootSeq []
