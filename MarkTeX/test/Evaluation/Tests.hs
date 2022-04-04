module Evaluation.Tests where

import Evaluation.Predicates
import MarkTeX.TemplateLang.Expression
import MarkTeX.Parsing.Expression (RootExpr)
import MarkTeX.Parsing.Parser (parseMd)

import Test.Tasty
import Test.Tasty.HUnit
import GHC.IO (unsafePerformIO)
import System.FilePath ((</>), takeDirectory, dropFileName)
import MarkTeX.TemplateLang (TData, toString, TValue (TString))
import qualified Data.Map as M
import MarkTeX (readJson, runEvaluation, EvaluationError, State (State), Information (settings), Environment)
import MarkTeX.Evaluation.MetaEvaluator (State)
import Control.Arrow (right)
import Data.Either (isLeft, fromRight)
import Test.Tasty.Runners

type MDFilePath = FilePath
type JSONFilePath = FilePath

data EvalTest = EvalTest String MDFilePath JSONFilePath [(String, EvalPredicate)]

evalTests :: TestTree
evalTests = testGroup "Evaluation" (evalTestsSeq $ map evalTestToTestTree evalTests')

-- | Wait for earlier tests to finish since the eval doesn't work well in parallel
evalTestsSeq :: [TestTree] -> [TestTree]
evalTestsSeq = evalTestsSeq' Nothing where
    evalTestsSeq' :: Maybe String -> [TestTree] -> [TestTree]
    evalTestsSeq' lastName []     = []
    evalTestsSeq' lastName (t:ts) = case lastName of
        Nothing  -> t : evalTestsSeq' (Just $ getTestName t) ts
        Just str -> after AllFinish str t : evalTestsSeq' (Just $ getTestName t) ts

getTestName :: TestTree -> String
getTestName (TestGroup str _) = str
getTestName _ = undefined

evalTests' :: [EvalTest]
evalTests' = [
        EvalTest "Insert statement" "insert.md" "insert.json" [
            ("Bool", ContainsExpr (Text "True")),
            ("String", ContainsExpr (Text "String")),
            ("Int", ContainsExpr (Text "1")),
            ("Float", ContainsExpr (Text "1.5")),
            ("List", ContainsExpr (Text "[1, 2, 3]")),
            ("Map", ContainsExpr (Text "map_beforeitem1item2map_end")),
            ("Null", ContainsExpr (Text "null_beforenull_end"))
        ], EvalTest "If statement" "if.md" "if.json" [
            ("If true", ContainsExpr (Bold $ Text "Show")),
            ("If false", NotContainsExpr (Bold $ Text "NotShow")),
            ("IfVar true", ContainsExpr (Bold $ Text "ShowVar")),
            ("IfVar false", NotContainsExpr (Bold $ Text "NotShowVar"))
        ], EvalTest "For statement" "for.md" "for.json" [
            ("For 1 to 3", ContainsExpr (Seq [Italic $ Text "1", Italic $ Text "2", Italic $ Text "3"])),
            ("For 1 to 3, if > 1", ContainsExpr (Seq [Bold $ Text "2", Bold $ Text "3"]))
        ], EvalTest "SetVar statement" "setvar.md" "empty.json" [
            ("SetVar overwrites", ContainsExpr (Seq [Bold $ Text "5", Bold $ Text "1337"]))
        ], EvalTest "While statement" "while.md" "empty.json" [
            ("While 1 to 3", ContainsExpr (Seq [Italic $ Text "1", Italic $ Text "2", Italic $ Text "3"]))
        ], EvalTest "Import statements" "imports.md" "empty.json" [
            ("Import Data.Char", ContainsExpr (Bold $ Text "5"))
        ], EvalTest "Import file statements" "importfile.md" "empty.json" [
            ("Import test_import.hs", ContainsExpr (Text "somestring"))
        ], EvalTest "Include statements" "include.md" "empty.json" [
            ("Include without data", ContainsExpr (Text "Val=Inc")),
            ("Include with data", ContainsExpr (Text "Val=Inc1337"))
        ], EvalTest "Custom hs component" "custom_component.md" "empty.json" [
            ("Range component", ContainsExpr (UnorderedList [Text "1", Text "2", Text "3"]))
        ], EvalTest "Document settings" "docsetting.md" "empty.json" [
            ("DocSetting", DocSettingEquals "Setting1" (TString "Value1")),
            ("DocSettings 2", DocSettingEquals "Setting2" (TString "Value2")),
            ("DocSettings 3", DocSettingEquals "Setting3" (TString "Value3"))
        ]
    ]

evalTestToTestTree :: EvalTest -> TestTree
evalTestToTestTree (EvalTest str mdFile jsonFile ps) = testGroup str tests where
    rootExpr :: RootExpr
    rootExpr = case parseMd $ unsafePerformIO (readFile ("test/Evaluation/inputs/md/" </> mdFile)) of
        Left err -> error $ "Error parsing " ++ mdFile ++ ": " ++ show err
        Right r -> r
    jsonData :: TData
    jsonData = case unsafePerformIO (readJson ("test/Evaluation/inputs/json/" </> jsonFile)) of
        Left err  -> error $ show err
        Right dat -> dat
    runEvalResult = unsafePerformIO $! runEvaluation "test/Evaluation/inputs/md/" rootExpr jsonData
    (State env info) = fst runEvalResult
    evalResult = snd runEvalResult
    testToAssertion :: EvalPredicate -> Assertion
    testToAssertion ReturnsError = assertBool "No error was returned" (isLeft evalResult)
    testToAssertion p = if isLeft evalResult
        then assertBool ("An error was returned: " ++ show evalResult) False
        else case p of
            (ContainsExpr e) -> assertBool ("Cannot find " ++ show e ++ ", got " ++ show evalResult) (containsPredicate e (fromRight emptyExpr evalResult))
            (NotContainsExpr e) -> assertBool ("Found " ++ show e) (not $ containsPredicate e (fromRight emptyExpr evalResult))
            (DocSettingEquals str val) -> assertBool ("Docsetting " ++ str ++ " does not equal " ++ toString val) (
                case M.lookup str (settings info) of
                    Nothing   -> False
                    Just val' -> val' == val
                )
            _ -> error "Not implemented"
    tests :: [TestTree]
    tests = map (\(str', p) -> testCase str' (testToAssertion p)) ps

emptyExpr :: Expr
emptyExpr = Seq []