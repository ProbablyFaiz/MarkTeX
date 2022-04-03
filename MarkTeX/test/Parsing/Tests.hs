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
            ("Inline simple commands 1", ContainsExpr (CommandCode "SimpleCommand1")),
            ("Inline simple commands 2", ContainsExpr (CommandCode "SimpleCommand2")),
            ("Block command", ContainsRootExpr (CommandBlockCode "BlockCommand" emptyRootExpr)),
            ("Inline block command", ContainsRootExpr (CommandBlockCode "BlockCommand2" emptyRootExpr))
        ], ParseTest "Code command blocks" "code_blocks.md" [
            ("Block with inner inline 1", ContainsExpr (Italic $ CommandCode "Command1In")),
            ("Block with inner inline 2", ContainsExpr (CommandCode "Command2In")),
            ("Block with inner newline 1", ContainsExpr (Italic $ CommandCode "Command1New")),
            ("Block with inner newline 2", ContainsExpr (CommandCode "Command2New"))
        ], ParseTest "Lists" "lists.md" [
            ("Unordered", ContainsRootExpr (UnorderedList [Text "ULItem1", Text "ULItem2"])),
            ("Ordered", ContainsRootExpr (OrderedList [Text "OLItem1", Text "OLItem2"]))
        ], ParseTest "Tags in lists" "tags_in_lists.md" [
            ("Tags in lists", ContainsRootExpr (UnorderedList [
                Bold (Text "bold"), 
                Italic (Text "italic"),
                Hyperlink (Text "LinkText") (Text "LinkUrl"),
                Image (Text "AltText") (Text "ImageUrl"),
                CommandCode "SimpleCommand"
            ]))
        ]
    ]

parseTestToTestTree :: ParseTest -> TestTree
parseTestToTestTree (ParseTest str file ps) = testGroup str tests where
    expr :: RootExpr
    expr = case parseMd $ unsafePerformIO (readFile ("test/Parsing/inputs/" </> file)) of
      Left s -> error "Parse failed"
      Right re -> re
    testToAssertion :: ParserPredicate -> Assertion
    testToAssertion (ContainsExpr e)     = assertBool ("Cannot find " ++ show e ++ ", got " ++ show expr) (containsEPredicate e expr)
    testToAssertion (ContainsRootExpr e) = assertBool ("Cannot find " ++ show e ++ ", got " ++ show expr) (containsREPredicate e expr)
    tests :: [TestTree]
    tests = map (\(str', p) -> testCase str' (testToAssertion p)) ps

emptyExpr :: Expr
emptyExpr = Seq []

emptyRootExpr :: RootExpr
emptyRootExpr = RootSeq []
