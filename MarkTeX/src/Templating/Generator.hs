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

----- Types & Instances -----


-- Simple type synonyms
type Environment = TData
type Settings = TData

-- | The state contains the current environment and other information
data State = State Environment Information
    deriving (Show)

-- | Information about import statements and document settings
data Information = Information {
  docSettings :: Settings,
  fileImports :: [String],
  imports :: [String],
  importsQ :: [(String, String)]
}
    deriving (Show)

-- | Error type which contains the different kinds of errors
data Error = MetaCommandError String
           | LookupError String
           | InterpreterError I.InterpreterError
    deriving (Show)

-- | The main datatype is the eval datatype which transforms the state into an updated state
-- together with a return value, which can be either an error or an actual value
newtype Eval a = Eval (State -> IO (State, Either Error a))

-- Instances
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
            


----- Small helper evaluation functions and initial values -----


-- | Looks up the value for the given key in the environment data
lookupTValue :: String -> Eval TValue
lookupTValue k = Eval $
    \s@(State env _) ->
        case M.lookup k env of
            Nothing -> return (s, Left $ LookupError $ "Could not find a value for the key " ++ show k ++ " in the environment data!")
            Just v  -> return (s, Right v)

-- | Insert a key value pair into the environment data
insertTValue :: String -> TValue -> Eval ()
insertTValue k v = Eval $
    \(State env info) ->
        let newenv = M.insert k v env
        in return (State newenv info, Right ())

-- | Adds a document setting to the current document settings
insertSetting :: String -> TValue -> Eval ()
insertSetting k v = Eval $
    \(State env info@Information{docSettings = docSettings}) ->
        let newDocSettings = M.insert k v docSettings
        in return (State env info{docSettings = newDocSettings}, Right ())

-- | Unions a map of document settings to the current document settings
insertSettings :: TData -> Eval ()
insertSettings newData = Eval $
    \(State env info@Information{docSettings = docSettings}) ->
        let newDocSettings = docSettings `M.union` newData
        in return (State env info{docSettings = newDocSettings}, Right ())

-- | Add a file to import
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

-- | Helper function for raising an error state
raiseError :: Error -> Eval a
raiseError err = Eval $ \s -> return (s, Left err)

-- | When an action does not return an expression, an empty expression is returned
emptyExpr' :: Expr'
emptyExpr' = Seq' []

emptyRootExpr' :: RootExpr'
emptyRootExpr' = RootSeq' []

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


-- | Run the Eval computation
runEvaluation :: RootExpr -> TData -> IO (State, Either Error RootExpr')
runEvaluation e d = let (Eval f) = evalRootExpr e in f (State d emptySettings)

-- | The `evalRootExpr` function says what computation to do for evaluating the different root expressions under the given state
evalRootExpr :: RootExpr -> Eval RootExpr'
evalRootExpr (Heading n e)         = Heading' n <$> evalExpr e
evalRootExpr (Body e)              = Body' <$> evalExpr e
evalRootExpr (OrderedList es)      = OrderedList' <$> traverse evalExpr es
evalRootExpr (UnorderedList es)    = UnorderedList' <$> traverse evalExpr es
evalRootExpr NewLine               = pure NewLine'
evalRootExpr (TemplateBlock str e) = evalTemplate str >>= \cmd -> evalMetaBlock cmd e str
evalRootExpr (RootSeq es)          = RootSeq' <$> traverse evalRootExpr es

-- | The `evalExpr` function says what computation to do for evaluating the different expressions under the given state
evalExpr :: Expr -> Eval Expr'
evalExpr (Seq es)          = Seq' <$> traverse evalExpr es
evalExpr (Text s)          = pure (Text' s)
evalExpr (Bold e)          = Bold' <$> evalExpr e 
evalExpr (Italic e)        = Italic' <$> evalExpr e 
evalExpr (Hyperlink e1 e2) = Hyperlink' <$> evalExpr e1 <*> evalExpr e2
evalExpr (Image e1 e2)     = Image' <$> evalExpr e1 <*> evalExpr e2
evalExpr (Template str)    = evalTemplate str >>= evalMetaCommand

-- | evalMetaBlock
evalMetaBlock :: MetaCommand -> RootExpr -> String -> Eval RootExpr'
evalMetaBlock (If b)      e _   = evalIf (toBool b) e
evalMetaBlock (IfVar str) e _   = lookupTValue str >>= \b -> evalIf (toBool b) e
evalMetaBlock (For x val) e _   = RootSeq' <$> evalForList x (toListTValue val) e
evalMetaBlock (While b)   e cmd = 
    if toBool b
        then (\x y -> RootSeq' [x, y]) <$> evalRootExpr e <*> evalRootExpr (TemplateBlock cmd e)
        else pure emptyRootExpr'
evalMetaBlock m           _ _   = raiseError $ MetaCommandError $ 
                                                        "Input is not a metablock!\nReceived the following metacommand:\n" ++ show m

evalIf :: Bool -> RootExpr -> Eval RootExpr'
evalIf True  e = evalRootExpr e
evalIf False _ = pure emptyRootExpr'

-- | evalMetaCommand
evalMetaCommand :: MetaCommand -> Eval Expr'
evalMetaCommand (Insert val)           = pure $ (Text' . toString) val
evalMetaCommand (InsertVar str)        = Text' . toString <$> lookupTValue str
evalMetaCommand (DocSetting str val)   = emptyExpr' <$ insertSetting str val
evalMetaCommand (DocSettings tdata)    = emptyExpr' <$ insertSettings tdata
evalMetaCommand (LoadHsFile str)       = emptyExpr' <$ addFileImport str
evalMetaCommand (Import str)           = emptyExpr' <$ addImport str
evalMetaCommand (ImportQ strMod strAs) = emptyExpr' <$ addQImport strMod strAs
evalMetaCommand (SetVar str val)       = emptyExpr' <$ insertTValue str val
evalMetaCommand m                      = raiseError $ MetaCommandError $ 
                                                        "Input is not a simple metacommand!\nReceived the following metacommand:\n" ++ show m

-- | evalForList
evalForList :: String -> [TValue] -> RootExpr -> Eval [RootExpr']
evalForList str xs e = traverse setVarAndEval xs
    where
        setVarAndEval :: TValue -> Eval RootExpr'
        setVarAndEval x = evalMetaCommand (SetVar str x) *> evalRootExpr e

-- | evalTemplate
evalTemplate :: String -> Eval MetaCommand
evalTemplate str = Eval $
    \s -> do 
        result <- interpretCommand str s
        case result of
            Left interpErr -> pure (s, Left $ InterpreterError interpErr)
            Right cmd      -> pure (s, Right cmd)

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