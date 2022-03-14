{-# LANGUAGE RankNTypes #-}
module Templating.Generator where

import qualified Data.Map as M
import qualified Language.Haskell.Interpreter as I

import Templating.Commands
import Templating.Parser
import Text.Read (readMaybe)
import Language.Haskell.Interpreter (loadModules)
import GHC.IO (unsafePerformIO)
import Data.Foldable (foldrM, foldlM)
import System.IO.Temp
import GHC.IO.Handle
import Control.Monad (unless)

data GeneratorState = Error String | State {
  outputText :: String,
  documentSettings :: IMap,
  env :: IMap
}

initialState :: IMap -> GeneratorState
initialState idata = State {outputText="", documentSettings=M.empty, env=idata}

evalTExpr :: TExpr -> IMap -> IO GeneratorState
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
evalMetaCommand (InsertVar str) gs@State {outputText=currText, env=env} = case M.lookup str env of
  Just val -> return $ gs{outputText=currText ++ toString val}
  Nothing  -> return $ Error ("Variable not in scope: " ++ str)
evalMetaCommand _ _ = return $ Error "Input is not a metacommand"

evalMetaBlock :: MetaCommand -> TExpr -> GeneratorState -> IO GeneratorState
evalMetaBlock _ _ (Error str) = return $ Error str
evalMetaBlock (IfE b)     expr   gs               = if b then evalTExpr' expr gs else return gs
evalMetaBlock (If val)    expr   gs               = evalMetaBlock (IfE $ toBool val) expr gs
evalMetaBlock (IfVar str) expr gs@State {env=env} = evalMetaBlock (IfE $ toBool (lookupIMap str env)) expr gs
evalMetaBlock _ _ _ = return $ Error "Input is not a metablock"

interpretCommand :: String -> GeneratorState -> IO (Either I.InterpreterError MetaCommand)
interpretCommand _ (Error str) = error str
interpretCommand commandStr gs = withHsEnvModule gs (runInterpreter commandStr) where
  runInterpreter :: String -> String -> IO (Either I.InterpreterError MetaCommand)
  runInterpreter commandStr env_path = do I.runInterpreter (createInterpreter commandStr env_path)
  createInterpreter :: String -> String -> I.Interpreter MetaCommand 
  createInterpreter commandStr env_path = do
    I.loadModules ["src/Templating/Commands.hs", env_path];
    I.setTopLevelModules ["Templating.Commands", "IEnv"];
    I.interpret commandStr (I.as::MetaCommand);

withHsEnvModule :: GeneratorState -> (String -> IO a) -> IO a
withHsEnvModule (Error str) _ = error "Cannot create hs module when an error was thrown"
withHsEnvModule gs@State {env=env} f = do
  let fileHandler path hFile = do
      isReadable <- hIsReadable hFile;
      unless isReadable (return $ error "Cannot read env file");
      isWriteable <- hIsWritable hFile;
      unless isWriteable (return $ error "Cannot write env file");
      hPutStr hFile (createEnvDefinition env);
      hFlushAll hFile;
      hClose hFile;
      f path;
  withTempFile "." ".hs" fileHandler;

createEnvDefinition :: IMap -> String 
createEnvDefinition imap = "module IEnv where\r\n" ++
  "import qualified Data.Map as M\r\n" ++
  "import Templating.Commands\r\n" ++
  "env :: IMap\r\n" ++
  "env = M.fromList " ++ show (M.toList imap) ++ "\r\n" ++
  "get :: String -> InputValue" ++ "\r\n" ++
  "get s = lookupIMap s env \r\n"

-- SMALL VERIFICATION TESTS:
testData :: IMap 
testData = M.fromList [("productname", IString "Bike"), ("exists", INumber 1)]

testExpr :: TExpr 
testExpr = Seq [
    Text "This is some test input\n",
    Block "If (get \"doesntexist\")" (Text "Not shown because variable doesn't exist\n"),
    Block "If $ notB(get \"doesntexist\")" (Text "Shown because we negate it\n"),
    Text "\n",
    Block "IfVar \"productname\"" (Seq $ [Text "Productname exists!: ", Command "InsertVar \"productname\""])
  ]

runGeneratorTest :: IO ()
runGeneratorTest = do
  gs <- evalTExpr testExpr testData;
  case gs of
    Error s -> print s
    State s map map' -> print s
