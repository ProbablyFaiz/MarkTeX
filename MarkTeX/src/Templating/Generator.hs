{-# LANGUAGE RankNTypes #-}
module Templating.Generator where

import qualified Data.Map as M
import qualified Language.Haskell.Interpreter as I

import TemplateLang hiding ((++))
import Templating.Parser
import Text.Read (readMaybe)
import Language.Haskell.Interpreter (loadModules)
import GHC.IO (unsafePerformIO)
import Data.Bifunctor
import Data.Foldable (foldrM, foldlM)
import System.IO.Temp
import GHC.IO.Handle
import Control.Monad (unless)

data GeneratorState = Error String | State {
  outputText :: String,
  documentSettings :: TData,
  fileImports :: [String],
  imports :: [String],
  importsQ :: [(String, String)],
  env :: TData
}

initialState :: TData -> GeneratorState
initialState idata = State {outputText="", documentSettings=M.empty, env=idata, fileImports=[], imports=[], importsQ=[]}

evalTExpr :: TExpr -> TData -> IO GeneratorState
evalTExpr expr idata = evalTExpr' expr (initialState idata)

evalTExpr' :: TExpr -> GeneratorState -> IO GeneratorState
evalTExpr' _ (Error str) = return $ Error str
evalTExpr' (Seq exprs) gs = foldrM evalTExpr' gs (reverse exprs)
evalTExpr' (Text str)  gs@State {outputText=currText} = return gs{outputText=currText ++ str}
evalTExpr' (Command str) gs = do
    result <- interpretCommand str gs;
    case result of
      (Left err)          -> return $ Error (show err);
      (Right metaCommand) -> evalMetaCommand metaCommand gs;
evalTExpr' (Block str expr) gs = do
    result <- interpretCommand str gs;
    case result of
      (Left err)          -> return $ Error (show err);
      (Right metaCommand) -> evalMetaBlock metaCommand expr gs;

evalMetaCommand :: MetaCommand -> GeneratorState -> IO GeneratorState
evalMetaCommand _ (Error str) = return $ Error str
evalMetaCommand (Insert val) gs@State {outputText=currText} = return gs{outputText=currText ++ toString val}
evalMetaCommand (InsertVar str) gs@State {outputText=currText, env=env}   = return $ gs{outputText=currText ++ toString (lookupTData str env)}
evalMetaCommand (DocSetting str val) gs@State {documentSettings=settings} = return gs{documentSettings=M.insert str val settings}
evalMetaCommand (DocSettings tdata) gs@State {documentSettings=settings}  = return gs{documentSettings=M.union tdata settings}
evalMetaCommand (LoadHsFile str) gs       = return gs{fileImports=fileImports gs ++ [str]}
evalMetaCommand (Import str) gs           = return gs{imports=imports gs ++ [str]}
evalMetaCommand (ImportQ strMod strAs) gs = return gs{importsQ=importsQ gs ++ [(strMod, strAs)]}
evalMetaCommand _ _ = return $ Error "Input is not a metacommand"

evalMetaBlock :: MetaCommand -> TExpr -> GeneratorState -> IO GeneratorState
evalMetaBlock _ _ (Error str) = return $ Error str
evalMetaBlock (If b)     expr   gs                = if b then evalTExpr' expr gs else return gs
evalMetaBlock (IfVar str) expr gs@State {env=env} = evalMetaBlock (If $ toBool (lookupTData str env)) expr gs
evalMetaBlock _ _ _ = return $ Error "Input is not a metablock"

interpretCommand :: String -> GeneratorState -> IO (Either I.InterpreterError MetaCommand)
interpretCommand _ (Error str) = error str
interpretCommand commandStr gs = withHsEnvModule gs (runInterpreter commandStr) where
  runInterpreter :: String -> String -> IO (Either I.InterpreterError MetaCommand)
  runInterpreter commandStr env_path = do I.runInterpreter (createInterpreter commandStr env_path)
  createInterpreter :: String -> String -> I.Interpreter MetaCommand
  createInterpreter commandStr env_path = do
    I.loadModules (env_path : fileImports gs);
    I.setTopLevelModules ("TEnv" : imports gs);
    I.setImportsQ (("Prelude", Just "P") : map (second Just) (importsQ gs));
    I.interpret commandStr (I.as::MetaCommand);

withHsEnvModule :: GeneratorState -> (String -> IO a) -> IO a
withHsEnvModule (Error str) _ = error "Cannot create hs module when an error was thrown"
withHsEnvModule gs@State {env=env} f = do withTempFile "." ".hs" fileHandler; where
  fileHandler path hFile = do
    isReadable <- hIsReadable hFile;
    unless isReadable (return $ error "Cannot read env file");
    isWriteable <- hIsWritable hFile;
    unless isWriteable (return $ error "Cannot write env file");
    hPutStr hFile (createEnvDefinition env);
    hFlushAll hFile;
    hClose hFile;
    f path;

createEnvDefinition :: TData -> String
createEnvDefinition dat = "module TEnv where\r\n" ++
  "{-# LANGUAGE OverloadedLists #-}" ++
  "import qualified Prelude as P\r\n" ++
  "import Prelude hiding " ++ hidePreludeString ++ "\r\n" ++
  "import qualified Data.Map as M\r\n" ++
  "import TemplateLang\r\n" ++
  "fromList = M.fromList\r\n" ++
  "env :: TData\r\n" ++
  "env = " ++ show dat ++ "\r\n" ++
  "get :: P.String -> TValue" ++ "\r\n" ++
  "get s = lookupTData s env \r\n"

-- SMALL VERIFICATION TESTS:
testData :: TData
testData = M.fromList [
    ("product", TData (M.fromList [
      ("name", TString "Bike"),
      ("price", TNumber 500)
    ])),
    ("exists", TNumber 1),
    ("Price", TNumber 20),
    ("Strings",
    TList [TString "S1", TString "S2"])
  ]

testExpr :: TExpr
testExpr = Seq [
    Text "This is some test input\n",
    Block "tIf (get \"doesntexist\")" (Text "Not shown because variable doesn't exist\n"),
    Block "tIfNot (get \"doesntexist\")" (Text "Shown because we negate it\n"),
    Block "tIf (get \"Price\" > 10)" (Text "More than 10"),
    Text "\n",
    Block "IfVar \"product.name\"" (Seq [Text "Productname exists!: ", Command "InsertVar \"product.name\""]),
    Command "Insert (get \"Strings\" ++ [\"S3\"])"
  ]

runGeneratorTest :: IO ()
runGeneratorTest = do
  gs <- evalTExpr testExpr testData;
  case gs of
    Error s              -> print s
    State {outputText=s} -> print s
