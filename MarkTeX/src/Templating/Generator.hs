{-# LANGUAGE RankNTypes #-}
module Templating.Generator where

import qualified Data.Map as M
import qualified Language.Haskell.Interpreter as I

import TemplateLang hiding ((++))
import Text.Read (readMaybe)
import Language.Haskell.Interpreter (loadModules)
import GHC.IO (unsafePerformIO)
import Data.Bifunctor
import Data.Foldable (foldrM, foldlM)
import System.IO.Temp
import GHC.IO.Handle
import Control.Monad (unless)
import Language
import Data.Either

type EvalResult = Either RootExpr Expr

getExpr :: EvalResult -> Expr 
getExpr (Right x) = x
getExpr (Left x) = error ("Not an Expr: " ++ show x)

getRootExpr :: EvalResult -> RootExpr 
getRootExpr (Left x) = x
getRootExpr (Right x) = Body x

data GeneratorState = Error String | State {
  returnVal :: EvalResult,
  documentSettings :: TData,
  fileImports :: [String],
  imports :: [String],
  importsQ :: [(String, String)],
  env :: TData
}

initialState :: TData -> GeneratorState
initialState idata = State {returnVal = Left $ RootSeq [], documentSettings=M.empty, env=idata, fileImports=[], imports=[], importsQ=[]}

evalRootExpr :: RootExpr -> TData -> IO GeneratorState
evalRootExpr expr idata = evalRootExpr' expr (initialState idata)

evalRootExpr' :: RootExpr -> GeneratorState -> IO GeneratorState
evalRootExpr' _ (Error str) = return $ Error str
evalRootExpr' (RootSeq exprs) gs = evalRootExprsSeq (Left . RootSeq) exprs gs
evalRootExpr' (OrderedList exprs) gs = evalExprsSeq (Left . OrderedList) exprs gs
evalRootExpr' (UnorderedList exprs) gs = evalExprsSeq (Left . UnorderedList) exprs gs
evalRootExpr' (Heading x expr) gs = evalExprInside (Left . Heading x) expr gs
evalRootExpr' (Body expr) gs      = evalExprInside (Left . Body) expr gs
evalRootExpr' NewLine gs          = return gs{returnVal = Left NewLine}
evalRootExpr' (TemplateBlock str expr) gs = do
    result <- interpretCommand str gs;
    case result of
      (Left err)          -> return $ Error (show err);
      (Right (While b))   -> 
        if toBool b 
          then do gs'  <- evalRootExpr' expr gs
                  gs'' <- evalRootExpr' (TemplateBlock str expr) gs'
                  return gs'' {returnVal = combineResults (returnVal gs') (returnVal gs'')}
          else return gs
      (Right metaCommand) -> evalMetaBlock metaCommand expr gs;

evalExpr' ::  Expr -> GeneratorState -> IO GeneratorState
evalExpr' _ (Error str) = return $ Error str
evalExpr' (Seq exprs) gs = evalExprsSeq (Right . Seq) exprs gs
evalExpr' (Text str) gs = return gs{returnVal = Right $ Text str}
evalExpr' (Bold expr) gs   = evalExprInside (Right . Bold) expr gs
evalExpr' (Italic expr) gs = evalExprInside (Right . Italic) expr gs
evalExpr' (Hyperlink e1 e2) gs = evalExprInsideBi (\e1' e2' -> Right $ Hyperlink e1' e2') e1 e2 gs
evalExpr' (Image e1 e2) gs = evalExprInsideBi (\e1' e2' -> Right $ Image e1' e2') e1 e2 gs
evalExpr' (Template str) gs = do
  result <- interpretCommand str gs;
  case result of
    (Left err)          -> return $ Error (show err)
    (Right metaCommand) -> evalMetaCommand metaCommand gs

evalExprInside :: (Expr -> EvalResult) -> Expr -> GeneratorState -> IO GeneratorState
evalExprInside f expr gs = do
  next_gs <- evalExpr' expr gs;
  return next_gs{returnVal = f $ getExpr $ returnVal next_gs};

evalExprInsideBi :: (Expr -> Expr -> EvalResult) -> Expr -> Expr -> GeneratorState -> IO GeneratorState
evalExprInsideBi f expr1 expr2 gs = do
  next_gs <- evalExpr' expr1 gs;
  next_next_gs <- evalExpr' expr2 gs;
  return next_next_gs{returnVal = f (getExpr $ returnVal next_gs) (getExpr $ returnVal next_next_gs)}

evalRootExprsSeq :: ([RootExpr] -> EvalResult) -> [RootExpr] -> GeneratorState -> IO GeneratorState
evalRootExprsSeq f exprs gs = evalExprsSeq' f exprs [] gs where
  evalExprsSeq' :: ([RootExpr] -> EvalResult) -> [RootExpr] -> [RootExpr] -> GeneratorState -> IO GeneratorState
  evalExprsSeq' f es results (Error str) = return $ Error str
  evalExprsSeq' f [] results gs = do
    return gs{returnVal = f results}
  evalExprsSeq' f (e : es) results gs = do
    next_gs <- evalRootExpr' e gs;
    evalExprsSeq' f es (results ++ [getRootExpr $ returnVal next_gs]) next_gs

evalExprsSeq :: ([Expr] -> EvalResult) -> [Expr] -> GeneratorState -> IO GeneratorState
evalExprsSeq f exprs gs = evalExprsSeq' f exprs [] gs where
  evalExprsSeq' :: ([Expr] -> EvalResult) -> [Expr] -> [Expr] -> GeneratorState -> IO GeneratorState
  evalExprsSeq' f es results (Error str) = return $ Error str
  evalExprsSeq' f [] results gs = do
    return gs{returnVal = f results}
  evalExprsSeq' f (e : es) results gs = do
    next_gs <- evalExpr' e gs;
    evalExprsSeq' f es (results ++ [getExpr $ returnVal next_gs]) next_gs

evalMetaCommand :: MetaCommand -> GeneratorState -> IO GeneratorState
evalMetaCommand _ (Error str) = return $ Error str
evalMetaCommand (Insert val) gs@State {returnVal=ret} = return gs{returnVal = Right $ Text $ toString val}
evalMetaCommand (InsertVar str) gs@State {returnVal=ret, env=env} = return $ gs{returnVal = Right $ Text $ toString (lookupTData str env)}
evalMetaCommand (DocSetting str val) gs@State {documentSettings=settings} = return gs{documentSettings=M.insert str val settings}
evalMetaCommand (DocSettings tdata) gs@State {documentSettings=settings}  = return gs{documentSettings=M.union tdata settings}
evalMetaCommand (LoadHsFile str) gs       = return gs{fileImports=fileImports gs ++ [str]}
evalMetaCommand (Import str) gs           = return gs{imports=imports gs ++ [str]}
evalMetaCommand (ImportQ strMod strAs) gs = return gs{importsQ=importsQ gs ++ [(strMod, strAs)]}
evalMetaCommand (SetVar x val) gs@State {env = oldData} = let newData = M.insert x val oldData in return gs {env = newData, returnVal = Right $ Seq []}
evalMetaCommand _ _ = return $ Error "Input is not a metacommand"

evalMetaBlock :: MetaCommand -> RootExpr -> GeneratorState -> IO GeneratorState
evalMetaBlock _ _ (Error str) = return $ Error str
evalMetaBlock (If b)     expr   gs                = if toBool b then evalRootExpr' expr gs else return gs
evalMetaBlock (IfVar str) expr gs@State {env=env} = evalMetaBlock (If (lookupTData str env)) expr gs
evalMetaBlock (For x val) expr gs                 = evalForEachList x (listFromTValue val) expr gs
evalMetaBlock _ _ _ = return $ Error "Input is not a metablock"

evalForEachList :: String -> [TValue] -> RootExpr -> GeneratorState -> IO GeneratorState
evalForEachList x (v:vs) expr gs@State {env = oldData} =
    let newData = M.insert x v oldData
     in do gs'  <- evalRootExpr' expr gs {env = newData}
           gs'' <- evalForEachList x vs expr gs'
           return gs'' {returnVal = combineResults (returnVal gs') (returnVal gs'')}
evalForEachList _ _ _ gs = return gs

listFromTValue :: TValue -> [TValue]
listFromTValue (TList xs) = xs
listFromTValue tVal       = [tVal]

combineResults :: EvalResult -> EvalResult -> EvalResult
combineResults (Left (RootSeq v1)) (Left (RootSeq v2)) = Left $ RootSeq (v1 ++ v2)
combineResults (Left (RootSeq v1)) (Left v2)           = Left $ RootSeq (v1 ++ [v2])
combineResults (Left v1)           (Left (RootSeq v2)) = Left $ RootSeq (v1 : v2)
combineResults (Left v1)           (Left v2)           = Left $ RootSeq [v1, v2]
combineResults (Right v1)          (Left (RootSeq v2)) = Left $ RootSeq $ Body v1 : v2
combineResults (Right v1)          (Left v2)           = Left $ RootSeq [Body v1, v2]
combineResults (Left (RootSeq v1)) (Right v2)          = Left $ RootSeq (v1 ++ [Body v2])
combineResults (Left v1)           (Right v2)          = Left $ RootSeq [v1, Body v2]
combineResults (Right (Seq v1))    (Right (Seq v2))    = Right $ Seq (v1 ++ v2)
combineResults (Right (Seq v1))    (Right v2)          = Right $ Seq (v1 ++ [v2])
combineResults (Right v1)          (Right (Seq v2))    = Right $ Seq (v1 : v2)
combineResults (Right v1)          (Right v2)          = Right $ Seq [v1, v2]

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
  "{-# LANGUAGE FlexibleInstances #-}\r\n" ++
  "{-# LANGUAGE OverloadedLists #-}\r\n" ++
  "{-# LANGUAGE UndecidableInstances #-}\r\n" ++
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

testExpr :: RootExpr 
testExpr = RootSeq [
    Body $ Text "This is an initial test!",
    TemplateBlock "tIf (get \"product.name\")" (Body $ Text "Product name exists!")
  ]

testExpr2 :: RootExpr
testExpr2 = RootSeq [
    TemplateBlock "tFor \"x\" ([1, 2, 3, 10] :: [Int])" (Body $ Template "tInsert (get \"x\")")
    , 
    Body $ Template "SetVar \"m\" 0"
    , TemplateBlock "tWhile (get \"m\" < 5)" (RootSeq [Body $ Template "tInsert (get \"m\")", Body $ Template "SetVar \"m\" (get \"m\" + 1)"])
  ]

runGeneratorTest :: IO ()
runGeneratorTest = do
  gs <- evalRootExpr testExpr testData;
  case gs of
    Error s              -> print s
    State {returnVal=s} -> print (show s)
