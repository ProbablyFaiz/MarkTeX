{-# LANGUAGE RankNTypes #-}
module Templating.Generator where

import qualified Data.Map as M
import qualified Language.Haskell.Interpreter as I

import TemplateLang hiding ((++))
import Data.Bifunctor (Bifunctor(..))
import System.IO.Temp (withTempFile)
import GHC.IO.Handle (hClose, hFlushAll, hIsReadable, hIsWritable, hPutStr)
import Language (Expr(..), Expr'(..), RootExpr(..), RootExpr'(..))
import Data.Either ()
import Control.Monad (unless)


----- Types & Instances -----


-- Simple type synonyms
type Environment = TData
type Settings = TData

-- | The `State` datatype contains the current environment data and other meta information.
data State = State Environment Information
    deriving (Show)

-- | The `Information` datatype contains the meta information about import statements and document settings.
data Information = Information {
  docSettings :: Settings,
  fileImports :: [String],
  imports :: [String],
  importsQ :: [(String, String)]
}
    deriving (Show)

-- | The `Error` datatype contains the different kinds of errors which can occur while evaluating the templates.
data Error = MetaCommandError String
           | LookupError String
           | InterpreterError I.InterpreterError
    deriving (Show)

-- | This `Eval` datatype is the main datatype which contains the information about evaluation an expression based on the current state.
-- The evaluation will either result in an `Error` or it will succesfully evaluate an expression such as `Expr'` or `RootExpr'`.
newtype Eval a = Eval (State -> IO (State, Either Error a))

-- Functor, Applicative and Monad instances of the `Eval` datatype.
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
            

----- Helper functions for interacting with the state information -----


-- | The function `lookupTValue` looks up the value for the given key in the environment data.
lookupTValue :: String -> Eval TValue
lookupTValue k = Eval $
    \s@(State env _) ->
        case M.lookup k env of
            Nothing -> return (s, Left $ LookupError $ "Could not find a value for the key " ++ show k ++ " in the environment data!")
            Just v  -> return (s, Right v)

-- | The function `insertTValue` inserts a value for a the given key into the environment data.
insertTValue :: String -> TValue -> Eval ()
insertTValue k v = Eval $
    \(State env info) ->
        let newenv = M.insert k v env
        in return (State newenv info, Right ())

-- | The `insertSetting` function adds a document setting to the current document settings.
insertSetting :: String -> TValue -> Eval ()
insertSetting k v = Eval $
    \(State env info@Information{docSettings = docSettings}) ->
        let newDocSettings = M.insert k v docSettings
        in return (State env info{docSettings = newDocSettings}, Right ())

-- | The `insertSettings` function adds a map of document settings to the current document settings.
insertSettings :: TData -> Eval ()
insertSettings newData = Eval $
    \(State env info@Information{docSettings = docSettings}) ->
        let newDocSettings = docSettings `M.union` newData
        in return (State env info{docSettings = newDocSettings}, Right ())

-- | The function `addFileImport` is used to add a file to import to the current import list.
addFileImport :: String -> Eval ()
addFileImport str = Eval $
    \(State env info@Information{fileImports = fileImports}) ->
        return (State env info{fileImports = fileImports ++ [str]}, Right ())

-- | Add an import statement
addImport :: String -> Eval ()
addImport str = Eval $
    \(State env info@Information{imports = imports}) ->
        return (State env info{imports = imports ++ [str]}, Right ())

-- | Add a qualified import statement
addQImport :: String -> String -> Eval ()
addQImport strMod strAs = Eval $
    \(State env info@Information{importsQ = importsQ}) ->
        return (State env info{importsQ = importsQ ++ [(strMod, strAs)]}, Right ())

-- | This `raiseError` puts any error of the `Error` datatype as the current result.
-- Because of the applicative definition of the `Eval` datatype no more computations will be done and this error will be returned as the final result. 
raiseError :: Error -> Eval a
raiseError err = Eval $ \s -> return (s, Left err)

-- | When an action does not return an expression, an empty expression is returned
emptyExpr :: Expr'
emptyExpr = Seq' []

emptyRootExpr :: RootExpr'
emptyRootExpr = RootSeq' []

-- | This state is the initial empty state, which contains no data yet
emptyState :: State
emptyState = State M.empty emptySettings

-- | Empty settings used for initializing an empty state
emptySettings :: Information
emptySettings = Information { docSettings = M.empty
                            , fileImports = []
                            , imports = []
                            , importsQ = []
                            }

-- | toListTValue
toListTValue :: TValue -> [TValue]
toListTValue (TList xs) = xs
toListTValue t          = [t]


----- Main functionality -----


-- | The function `runEvaluation` evaluates the MarkDown AST with templates to a MarkDown AST without templates.
-- First it determines the computation to run on the given expression, and then it runs this computation on the given data.
runEvaluation :: RootExpr -> TData -> IO (State, Either Error RootExpr')
runEvaluation e d = let (Eval f) = evalRootExpr e in f (State d emptySettings)

-- | The `evalRootExpr` function determines what computation to do for evaluating the different `RootExpr` expressions.
-- For the subexpressions `Expr` it calls the `evalExpr` function.
-- When a `TemplateBlock` is encountered the template string is evaluated and the `evalMetaBlock` function is called with the resulting `MetaCommand`.
-- Other constructors can be mapped to the `RootExpr'` datatype directly, where possibly subexpressions have to be evaluated first of course.
evalRootExpr :: RootExpr -> Eval RootExpr'
evalRootExpr (Heading n e)         = Heading' n <$> evalExpr e
evalRootExpr (Body e)              = Body' <$> evalExpr e
evalRootExpr (OrderedList es)      = OrderedList' <$> traverse evalExpr es
evalRootExpr (UnorderedList es)    = UnorderedList' <$> traverse evalExpr es
evalRootExpr NewLine               = pure NewLine'
evalRootExpr (TemplateBlock str e) = evalTemplate str >>= \cmd -> evalMetaBlock cmd e str
evalRootExpr (RootSeq es)          = RootSeq' <$> traverse evalRootExpr es

-- | The `evalExpr` function says what computation to do for evaluating the different `Expr` expressions.
-- When a `Template` is encountered the template string is evaluated and the `evalMetaCommand` function is called with the resulting `MetaCommand`.
-- Other constructors can be mapped to the `Expr'` datatype directly, where possibly subexpressions have to be evaluated first of course.
evalExpr :: Expr -> Eval Expr'
evalExpr (Seq es)          = Seq' <$> traverse evalExpr es
evalExpr (Text s)          = pure (Text' s)
evalExpr (Bold e)          = Bold' <$> evalExpr e 
evalExpr (Italic e)        = Italic' <$> evalExpr e 
evalExpr (Hyperlink e1 e2) = Hyperlink' <$> evalExpr e1 <*> evalExpr e2
evalExpr (Image e1 e2)     = Image' <$> evalExpr e1 <*> evalExpr e2
evalExpr (Template str)    = evalTemplate str >>= evalMetaCommand

-- | The `evalMetaBlock` function evaluates the metacommands which expect a `RootExpr` expression inside this block.
-- When a 'simple' metacommand is encountered an error is raised.
evalMetaBlock :: MetaCommand -> RootExpr -> String -> Eval RootExpr'
evalMetaBlock (If b)      e _   = evalIf (toBool b) e
evalMetaBlock (IfVar str) e _   = lookupTValue str >>= \b -> evalIf (toBool b) e
evalMetaBlock (For x val) e _   = RootSeq' <$> evalForList x (toListTValue val) e
evalMetaBlock (While b)   e cmd = evalWhile (toBool b) e cmd
evalMetaBlock m           _ _   = raiseError $ MetaCommandError $ 
                                                "Input is not a metablock!\nReceived the following metacommand:\n" ++ show m

-- | The `evalMetaCommand` function evaluates the simple metacommands.
-- When the argument is a metablock an error is raised.
evalMetaCommand :: MetaCommand -> Eval Expr'
evalMetaCommand (Insert val)           = pure $ (Text' . toString) val
evalMetaCommand (InsertVar str)        = Text' . toString <$> lookupTValue str
evalMetaCommand (DocSetting str val)   = emptyExpr <$ insertSetting str val
evalMetaCommand (DocSettings tdata)    = emptyExpr <$ insertSettings tdata
evalMetaCommand (LoadHsFile str)       = emptyExpr <$ addFileImport str
evalMetaCommand (Import str)           = emptyExpr <$ addImport str
evalMetaCommand (ImportQ strMod strAs) = emptyExpr <$ addQImport strMod strAs
evalMetaCommand (SetVar str val)       = emptyExpr <$ insertTValue str val
evalMetaCommand m                      = raiseError $ MetaCommandError $ 
                                                        "Input is not a simple metacommand!\nReceived the following metacommand:\n" ++ show m

-- | `evalIf` evaluates the given expression based on the interpreted condition.
-- If the condition is false, it return an empty root expression.
-- If the condition is true, the expression is evaluated as normal.
evalIf :: Bool -> RootExpr -> Eval RootExpr'
evalIf True  e = evalRootExpr e
evalIf False _ = pure emptyRootExpr

-- | `evalForList` evaluates the given expression for all `TValue`s in the argument list in order and returns the list of resulting expressions.
evalForList :: String -> [TValue] -> RootExpr -> Eval [RootExpr']
evalForList str xs e = traverse setVarAndEval xs
    where
        setVarAndEval :: TValue -> Eval RootExpr'
        setVarAndEval x = evalMetaCommand (SetVar str x) *> evalRootExpr e

-- | `evalWhile` keeps evaluating the given expression while the template string keeps evaluating to true.
evalWhile :: Bool -> RootExpr -> String -> Eval RootExpr'
evalWhile True  e str = (\x y -> RootSeq' [x, y]) <$> evalRootExpr e <*> evalRootExpr (TemplateBlock str e)
evalWhile False _ _   = pure emptyRootExpr
-- possibly flatten all RootSeq' s ?

-- | `evalTemplate` defines how to interpret a template string inside the `Eval` datatype.
evalTemplate :: String -> Eval MetaCommand
evalTemplate str = Eval $
    \s -> do 
        result <- interpretCommand str s
        case result of
            Left interpErr -> pure (s, Left $ InterpreterError interpErr)
            Right cmd      -> pure (s, Right cmd)

-- | The `interpretCommand` function interprets the template string as a `MetaCommand` in the `IO` monad.
-- The result is either a valid `MetaCommand` or an `InterpreterError` if the interpreter failed to interpret the template string.
interpretCommand :: String -> State -> IO (Either I.InterpreterError MetaCommand)
interpretCommand str (State env info) = withHsEnvModule env (runInterpreter info str)
    where
        runInterpreter :: Information -> String -> String -> IO (Either I.InterpreterError MetaCommand)
        runInterpreter info str path = I.runInterpreter $ createInterpreter info str path

        createInterpreter :: Information -> String -> String -> I.Interpreter MetaCommand
        createInterpreter info str path = do
            I.loadModules (path : fileImports info)
            I.setTopLevelModules ("TEnv" : imports info)
            I.setImportsQ (("Prelude", Just "P") : map (second Just) (importsQ info))
            I.interpret str (I.as :: MetaCommand)

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
            "import TemplateLang\r\n" ++
            "fromList = M.fromList\r\n" ++
            "env :: TData\r\n" ++
            "env = " ++ show dat ++ "\r\n" ++
            "get :: P.String -> TValue" ++ "\r\n" ++
            "get s = lookupTData s env \r\n"