{-# LANGUAGE RankNTypes #-}

-- | The module 'MarkTeX.Evaluation.MetaEvaluator' evaluates all template commands and template blocks of the input 'MarkTeX' file.
-- The function 'runEvaluation' outputs the fully evaluated expression given the input expression together with some input data. It also takes in a relative file directory argument.
module MarkTeX.Evaluation.MetaEvaluator (runEvaluation, Environment(..), Information(..), State(..), EvaluationError(..)) where

import qualified Data.Map as M
import qualified Language.Haskell.Interpreter as I
import qualified MarkTeX.Parsing.Expression as P
import qualified MarkTeX.TemplateLang.Expression as E

import MarkTeX.TemplateLang hiding ((++))
import MarkTeX.Parsing.Parser (parseMd)
import MarkTeX.ReadJson (readJson, readOptionalJson, ReadJsonError(..))

import Data.Bifunctor (Bifunctor(..))
import System.IO.Temp (withTempFile)
import System.Directory (makeAbsolute)
import System.FilePath ((</>), takeDirectory)
import GHC.IO.Handle (hClose, hFlushAll, hIsReadable, hIsWritable, hPutStr)
import Data.Either (fromLeft)
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import GHC.IO (unsafePerformIO)


----- Types & Instances -----


-- Simple type synonyms
type Environment = TData
type Settings = TData
type RelativeDir = FilePath

-- | The 'State' datatype contains the current state during the evaluation of the MarkTeX language.
-- It contains the environment data as well as some other meta information.
data State = State Environment Information
    deriving (Show)

-- | The 'Information' datatype contains the meta information about import statements and document settings as well as the relative directory path.
data Information = Information {
  fileDir :: RelativeDir,
  settings :: Settings,
  fileImports :: [String],
  imports :: [String],
  importsQ :: [(String, String)]
}
    deriving (Show)

-- | The 'EvaluationError' datatype contains the different kinds of errors which can occur while evaluating the template language code snippets.
data EvaluationError = MetaCommandError String
                     | ExpectedWhile String
                     | ParseKeyError String
                     | ReadDataError ReadJsonError
                     | InterpreterError I.InterpreterError
    deriving (Show)

-- | The 'Eval' datatype is the leading datatype of the template evaluation which contains the information about evaluation an expression based on the current state.
-- The evaluation will either result in an 'EvaluationError' or it will succesfully evaluate all templates in the MarkTeX AST.
newtype Eval a = Eval (State -> IO (State, Either EvaluationError a))

-- Functor, Applicative and Monad instances of the 'Eval' datatype.
instance Functor Eval where
    fmap f (Eval g) = Eval $
        \s -> do
            (s', v) <- g s
            return (s', f <$> v)

instance Applicative Eval where
    pure v = Eval $
        \s ->
            return (s, Right v)

    (Eval f) <*> (Eval x) = Eval $
        \s -> do
            (s', g) <- f s
            case g of
                Left err -> do
                    return (s', Left err)
                Right g' -> do
                    (s'', v) <- x s'
                    return (s'', g' <$> v)

instance Monad Eval where
    (Eval x) >>= h = Eval $
        \s -> do
            (s', v) <- x s
            case v of
                Left err -> do
                    return (s', Left err)
                Right v' -> do
                    let (Eval k) = h v'
                    k s'


----- Main functionality -----


-- | The function 'runEvaluation' evaluates the Markdown AST with templates to a Markdown AST without templates.
-- First it determines the computation to run on the given expression, and then it runs this computation on the given data.
runEvaluation :: RelativeDir -> P.RootExpr -> TData -> IO (State, Either EvaluationError E.Expr)
runEvaluation dir e d =
    let (Eval evaluation) = cleanExpr <$> evalRootExpr e
     in do
        absDir <- makeAbsolute dir;
        evaluation (State d (emptySettings absDir));

-- | 'cleanExpr' removes empty Seq from the expression tree, it removes unneccesary nesting and it merges sequential text expressions.
cleanExpr :: E.Expr -> E.Expr
cleanExpr (E.Seq es)           = E.Seq $ mergeText $ unpackSeq $ map cleanExpr es
cleanExpr (E.Heading i e)      = E.Heading i (cleanExpr e)
cleanExpr (E.OrderedList es)   = E.OrderedList $ unpackSeq $ map cleanExpr es
cleanExpr (E.UnorderedList es) = E.UnorderedList $ unpackSeq $ map cleanExpr es
cleanExpr E.NewLine            = E.NewLine
cleanExpr (E.Text str)         = E.Text str
cleanExpr (E.Bold e)           = E.Bold (cleanExpr e)
cleanExpr (E.Italic e)         = E.Italic (cleanExpr e)
cleanExpr (E.Hyperlink e1 e2)  = E.Hyperlink (cleanExpr e1) (cleanExpr e2)
cleanExpr (E.Image e1 e2)      = E.Image (cleanExpr e1) (cleanExpr e2)
cleanExpr (E.CodeSnippet s)    = E.CodeSnippet s

-- | 'unpackSeq' unpacks a Seq in a list so that there's no unneccesary nesting.
unpackSeq :: [E.Expr] -> [E.Expr] 
unpackSeq []                = []
unpackSeq ((E.Seq ys) : xs) = ys ++ unpackSeq xs
unpackSeq (x : xs)          = x : unpackSeq xs

-- | Merge sequential text expressions with 'mergeText'.
mergeText :: [E.Expr] -> [E.Expr]
mergeText []                         = []
mergeText (E.Text x : E.Text y : xs) = mergeText (E.Text (x ++ y) : xs)
mergeText (x : xs)                   = x : mergeText xs

-- | The 'evalRootExpr' function determines what evaluation to do for evaluating the different 'RootExpr' expressions.
-- For the subexpressions 'Expr' it calls the 'evalExpr' function.
-- When a 'CommandBlockCode' is encountered the code string is evaluated and the 'evalMetaBlock' function is called with the resulting 'MetaCommand'.
-- Other constructors can be mapped to the evaluated 'Expr' datatype directly, where possibly subexpressions have to be evaluated first.
evalRootExpr :: P.RootExpr -> Eval E.Expr
evalRootExpr (P.Heading n e)            = E.Heading n <$> evalExpr e
evalRootExpr (P.Body e)                 = evalExpr e
evalRootExpr (P.OrderedList es)         = E.OrderedList <$> traverse evalExpr es
evalRootExpr (P.UnorderedList es)       = E.UnorderedList <$> traverse evalExpr es
evalRootExpr P.NewLine                  = pure E.NewLine
evalRootExpr (P.CommandBlockCode str e) = evalCommandCode str >>= \cmd -> evalMetaBlock cmd e str
evalRootExpr (P.RootSeq es)             = E.Seq <$> traverse evalRootExpr es
evalRootExpr (P.CodeSnippet s)          = pure $ E.CodeSnippet s

-- | The 'evalExpr' function says what computation to do for evaluating the different 'Expr' expressions.
-- When a 'CommandCode' is encountered the code string is evaluated and the 'evalMetaCommand' function is called with the resulting 'MetaCommand'.
-- Other constructors can be mapped to the 'EvalExpr' datatype directly, where possibly subexpressions have to be evaluated first of course.
evalExpr :: P.Expr -> Eval E.Expr
evalExpr (P.Seq es)          = E.Seq <$> traverse evalExpr es
evalExpr (P.Text s)          = pure (E.Text s)
evalExpr (P.Bold e)          = E.Bold <$> evalExpr e
evalExpr (P.Italic e)        = E.Italic <$> evalExpr e
evalExpr (P.Hyperlink e1 e2) = E.Hyperlink <$> evalExpr e1 <*> evalExpr e2
evalExpr (P.Image e1 e2)     = E.Image <$> evalExpr e1 <*> evalExpr e2
evalExpr (P.CommandCode str) = evalCommandCode str >>= evalMetaCommand

-- | The 'evalMetaBlock' function evaluates the metacommands which expect a 'RootExpr' expression inside this block.
-- When a 'simple' metacommand is encountered an error is raised.
-- The third argument is the code string, which is needed to keep evaluating in the while loop.
evalMetaBlock :: MetaCommand -> P.RootExpr -> String -> Eval E.Expr
evalMetaBlock (If b)      e _   = evalIf (toBool b) e
evalMetaBlock (IfVar str) e _   = evalLookupTValue str >>= \b -> evalIf (toBool b) e
evalMetaBlock (For x val) e _   = E.Seq <$> evalForList x (toListTValue val) e
evalMetaBlock (While b)   e str = E.Seq <$> evalWhile (toBool b) e str
evalMetaBlock m           _ _   = raiseError $ MetaCommandError $
    "Input is not a metablock!\nReceived the following metacommand:\n" ++ show m

-- | The 'evalMetaCommand' function evaluates the simple metacommands.
-- When the argument is a metablock an error is raised.
evalMetaCommand :: MetaCommand -> Eval E.Expr
evalMetaCommand (Insert val)           = pure $ E.Text (toString val)
evalMetaCommand (InsertVar str)        = E.Text . toString <$> evalLookupTValue str
evalMetaCommand (InsertExpr e)         = pure e
evalMetaCommand (DocSetting str val)   = emptyExpr <$ insertSetting str val
evalMetaCommand (DocSettings tdata)    = emptyExpr <$ insertSettings tdata
evalMetaCommand (LoadHsFile str)       = emptyExpr <$ addFileImport str
evalMetaCommand (Import str)           = emptyExpr <$ addImport str
evalMetaCommand (ImportQ strMod strAs) = emptyExpr <$ addQImport strMod strAs
evalMetaCommand (SetVar str val)       = emptyExpr <$ insertTValue str val
evalMetaCommand (Include str)          = evalInclude str Nothing
evalMetaCommand (IncludeWith str dat)  = evalInclude str (Just dat)
evalMetaCommand (ReadJson path)        = emptyExpr <$ (readJsonData path >>= insertTValues)    
evalMetaCommand (ReadJsonQ path qName) = emptyExpr <$ (readJsonData path >>= insertTValue qName . TData)
evalMetaCommand m                      = raiseError $ MetaCommandError $
    "Input is not a simple metacommand!\nReceived the following metacommand:\n" ++ show m

-- | 'evalIf' evaluates the given expression based on the interpreted condition.
-- If the condition is false, it return an empty root expression.
-- If the condition is true, the expression is evaluated as normal.
evalIf :: Bool -> P.RootExpr -> Eval E.Expr
evalIf True  e = evalRootExpr e
evalIf False _ = pure emptyExpr

-- | 'evalForList' evaluates the given expression for all 'TValue's in the argument list in order and returns the list of resulting expressions.
evalForList :: String -> [TValue] -> P.RootExpr -> Eval [E.Expr]
evalForList str xs e = traverse setVarAndEval xs
    where
        setVarAndEval :: TValue -> Eval E.Expr
        setVarAndEval x = evalMetaCommand (SetVar str x) *> evalRootExpr e

-- | 'evalWhile' keeps evaluating the given expression while the code string keeps evaluating to true.
evalWhile :: Bool -> P.RootExpr -> String -> Eval [E.Expr]
evalWhile False _ _   = pure []
evalWhile True  e str = (:) <$> evalRootExpr e <*> (evalCommandCode str >>= continueWhile)
    where
        continueWhile :: MetaCommand -> Eval [E.Expr]
        continueWhile (While b) = evalWhile (toBool b) e str
        continueWhile m         = pure [] <$ raiseError $ ExpectedWhile $
            "The code evaluated to a While metacommand in a previous iteration, but in this iteration the code evaluated to: " ++ show m

-- | 'evalCommandCode' defines how to interpret a code string inside the 'Eval' datatype.
evalCommandCode :: String -> Eval MetaCommand
evalCommandCode str = Eval $
    \s -> do
        result <- interpretCommand str s
        case result of
            Left interpErr -> pure (s, Left $ InterpreterError interpErr)
            Right cmd      -> pure (s, Right cmd)

-- | 'evalInclude' evaluates an external file with the given environment as input data
evalInclude :: String -> Maybe TData -> Eval E.Expr
evalInclude str dat = Eval $
    \(State env info) -> do
        let filePath = fileDir info </> str
        inputMd <- readFile filePath;
        let evalDat = fromMaybe env dat;
        let newDir = takeDirectory filePath;
        case parseMd inputMd of
          Left s -> error "Parsing included markdown file failed."
          Right evalExpr -> do
            (State env' info', result) <- runEvaluation newDir evalExpr evalDat;
            return (State (env `M.union` env') info {settings = settings info `M.union` settings info'}, result)

-- | The 'interpretCommand' function interprets the code string as a 'MetaCommand' in the 'IO' monad.
-- The result is either a valid 'MetaCommand' or an 'InterpreterError' if the interpreter failed to interpret the code string.
interpretCommand :: String -> State -> IO (Either I.InterpreterError MetaCommand)
interpretCommand str (State env info) = withHsEnvModule env (runInterpreter info str)
    where
        runInterpreter :: Information -> String -> String -> IO (Either I.InterpreterError MetaCommand)
        runInterpreter info str path = I.runInterpreter $ createInterpreter info str path

        createInterpreter :: Information -> String -> String -> I.Interpreter MetaCommand
        createInterpreter info str path = do
            I.set [I.searchPath I.:= ["."]];
            I.loadModules (path : map (fileDir info </>) (fileImports info));
            I.setImportsQ (("Prelude", Just "P") : map (second Just) (importsQ info) ++ zip (imports info) (repeat Nothing));
            I.setTopLevelModules ("TEnv" : imports info);
            I.interpret str (I.as :: MetaCommand);

        withHsEnvModule :: Environment -> (String -> IO a) -> IO a
        withHsEnvModule env f = withTempFile "." ".hs" fileHandler
            where
                fileHandler path hFile = do
                    isReadable <- hIsReadable hFile
                    unless isReadable (return $ error "Cannot read env file")
                    isWriteable <- hIsWritable hFile
                    unless isWriteable (return $ error "Cannot write env file")
                    hPutStr hFile (createEnvDefinition env)
                    hFlushAll hFile
                    hClose hFile
                    f path

        createEnvDefinition :: TData -> String
        createEnvDefinition dat = "module TEnv where\r\n" ++
            "{-# LANGUAGE FlexibleInstances #-}\r\n" ++
            "{-# LANGUAGE OverloadedLists #-}\r\n" ++
            "{-# LANGUAGE UndecidableInstances #-}\r\n" ++
            "import qualified Prelude as P\r\n" ++
            "import Prelude hiding " ++ hidePreludeString ++ "\r\n" ++
            "import qualified Data.Map as M\r\n" ++
            "import MarkTeX.TemplateLang\r\n" ++
            "fromList = M.fromList\r\n" ++
            "env :: TData\r\n" ++
            "env = " ++ show dat ++ "\r\n" ++
            "get :: P.String -> TValue" ++ "\r\n" ++
            "get s = lookupTData s env \r\n"


----- Helper functions for interacting with the state -----


-- | The function 'lookupTValue' looks up the value for the given key in the environment data.
evalLookupTValue :: String -> Eval TValue
evalLookupTValue k = Eval $
    \s@(State env _) ->
        case parseLookup k of
            Left ()       -> return (s, Left $ ParseKeyError $ "Could not correctly parse the lookup path \"" ++ show k ++ "\"!")
            Right lookups -> return (s, Right $ lookupsInTData lookups env)

-- | The function 'insertTValue' inserts a value for a the given key into the environment data.
insertTValue :: String -> TValue -> Eval ()
insertTValue k v = Eval $
    \(State env info) ->
        let newenv = M.insert k v env
        in return (State newenv info, Right ())

-- | The function 'insertTValue' inserts a value for a the given key into the environment data.
insertTValues :: TData -> Eval ()
insertTValues newData = Eval $
    \(State env info) ->
        let newenv = env `M.union` newData
        in return (State newenv info, Right ())

-- | The 'insertSetting' function adds a document setting to the current document settings.
insertSetting :: String -> TValue -> Eval ()
insertSetting k v = Eval $
    \(State env info@Information{settings = docSettings}) ->
        let newDocSettings = M.insert k v docSettings
        in return (State env info{settings = newDocSettings}, Right ())

-- | The 'insertSettings' function adds a map of document settings to the current document settings.
insertSettings :: TData -> Eval ()
insertSettings newData = Eval $
    \(State env info@Information{settings = docSettings}) ->
        let newDocSettings = docSettings `M.union` newData
        in return (State env info{settings = newDocSettings}, Right ())

-- | The function 'addFileImport' is used to add a file to import to the current import list.
addFileImport :: String -> Eval ()
addFileImport str = Eval $
    \(State env info@Information{fileImports = fileImports}) ->
        return (State env info{fileImports = fileImports ++ [str]}, Right ())

-- | 'addImport' is used to add an import statement.
addImport :: String -> Eval ()
addImport str = Eval $
    \(State env info@Information{imports = imports}) ->
        return (State env info{imports = imports ++ [str]}, Right ())

-- | 'addQImport' is used to add a qualified import statement.
addQImport :: String -> String -> Eval ()
addQImport strMod strAs = Eval $
    \(State env info@Information{importsQ = importsQ}) ->
        return (State env info{importsQ = importsQ ++ [(strMod, strAs)]}, Right ())

-- | 'readJsonData' is used to read additional JSON data.
readJsonData :: String -> Eval TData
readJsonData path = Eval $ 
    \s@(State env info) -> do
        contents <- readJson path -- or readOptionalJson, to not fail when a file does not exist
        return (s, mapLeft ReadDataError contents)

-- | 'mapLeft' is used to map a function over the 'Left' constructor over a value of type 'Either'.
-- A 'Right' constructor is left untouched.
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right b) = Right b

----- Helper functions for raising an error and retrieving a TValue list, together with some empty data states -----


-- | This 'raiseError' puts any error of the 'Error' datatype as the current result.
-- Because of the applicative definition of the 'Eval' datatype no more computations will be done and this error will be returned as the final result. 
raiseError :: EvaluationError -> Eval a
raiseError err = Eval $ \s -> return (s, Left err)

-- | 'toListTValue' converts a 'TList' to a normal Haskell list.
-- Other values are embedded into a singleton list.
toListTValue :: TValue -> [TValue]
toListTValue (TList xs) = xs
toListTValue t          = [t]

-- | When an action does not return an expression, the empty expression 'emptyExpr' is returned.
emptyExpr :: E.Expr
emptyExpr = E.Seq []

-- | This state 'emptyState' is the initial empty state, which does not contain any data yet besides the relative file directory.
emptyState :: FilePath -> State
emptyState fileDir = State M.empty (emptySettings fileDir)

-- | The empty settings 'emptySettings' are used for initializing an empty state.
emptySettings :: FilePath -> Information
emptySettings fileDir = Information { settings = M.empty
                                    , fileImports = []
                                    , imports = []
                                    , importsQ = []
                                    , fileDir = fileDir
                                    }
