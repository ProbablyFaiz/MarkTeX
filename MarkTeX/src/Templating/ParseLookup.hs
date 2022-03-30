module Templating.ParseLookup (parseLookup, nestedLookup, NestedLookup, Lookup(..)) where

import Control.Applicative

import Data.Char (digitToInt)

import GHC.Unicode (isDigit)

import TemplateLang ( TValue(TList, TData), TData )
import qualified Data.Map as M

----- Parser type definition -----

newtype Parser a = Parser (String -> Either () (a, String))


----- Parser type class instances -----

instance Functor Parser where
    -- Apply the function to the parsed value if the parser is succesful
    fmap f (Parser g) = 
        Parser 
            (\s ->
                case g s of 
                    Left  ()         -> Left  ()
                    Right (val, str) -> Right (f val, str)
            )

instance Applicative Parser where
    -- Parse the value without consuming any input
    pure v = Parser (\s -> Right (v, s))

    -- If the first parser fails return an error, otherwise run the second parser
    -- If the second parser fails return an error again, otherwise apply the first parsed value to the second parsed value 
    (Parser g) <*> p =
        Parser 
            (\s -> 
                case g s of 
                    Left  ()       -> Left ()
                    Right (gv, gs) -> let (Parser p') = gv <$> p in p' gs
            )

instance Monad Parser where
    -- For the return function, return = pure, which can be omitted

    -- If the parser fails return an error
    -- Otherwise bind the parsed value to the function
    -- Run the resulting parser function on the remaining input
    (Parser g) >>= f = 
        Parser 
            (\s -> 
                case g s of 
                    Left  err      -> Left err
                    Right (gv, gs) -> let (Parser h) = f gv in h gs
            )

instance Alternative Parser where
    -- The empty parser acts as a failure parser
    empty = Parser (\_ -> Left ())

    -- If the first parser fails, run the second parser
    -- Otherwise return the output of the first parser
    (Parser g) <|> (Parser h) = 
        Parser 
            (\s -> 
                case g s of 
                    Left  _        -> h s
                    Right (gv, gs) -> Right (gv, gs)
            )


----- Lookup Types -----

type NestedLookup = [Lookup]
type Name = String
type Index = Int

data Lookup = Name Name
            | Index Index
    deriving (Show)


----- Functions for doing the lookup -----

nestedLookup :: String -> NestedLookup -> TData -> Either String TValue
nestedLookup k lookups env = searchInTValue k lookups (TData env)

searchInTValue :: String -> NestedLookup -> TValue -> Either String TValue
searchInTValue k lookups tVal = case lookups of
    []     -> Left $ "Could not find a value in the environment for the path \"" ++ show k ++ "\" in the environment data!\nThe path is most likely an empty string!"
    [l]    -> lookupInTValue k l tVal
    (l:ls) -> lookupInTValue k l tVal >>= searchInTValue k ls -- in the either monad

lookupInTValue :: String -> Lookup -> TValue -> Either String TValue
lookupInTValue k (Name x) val = 
    case val of
        (TData tdata) -> 
            case M.lookup x tdata of
                Nothing -> Left $ "Could not find a value in the environment for the path \"" ++ show k ++ "\" in the environment data!\nCould not find the key \"" ++ show x ++ "\" in the current TData map!"
                Just v -> Right v
        _ -> Left $ "Could not find a value in the environment for the path \"" ++ show k ++ "\" in the environment data!\nCould not find the key \"" ++ show x ++ "\", because the current value is not a TData map!" 
lookupInTValue k (Index n) val = 
    case val of
        (TList vs) -> 
            if n >= length vs
                then Left $ "Could not find a value in the environment for the path \"" ++ show k ++ "\" in the environment data!\nThe specified index \"" ++ show n ++ "\" is out of bounds!"
                else Right $ vs !! n
        _ -> Left $ "Could not find a value in the environment for the path \"" ++ show k ++ "\" in the environment data!\nThe current value is indexed with the index \"" ++ show n ++ "\", but it is not a list!"


----- Parser for nested lookups ---

parseLookup :: String -> Either () NestedLookup
parseLookup = runParser pNestedLookup

pNestedLookup :: Parser NestedLookup
pNestedLookup =  (:) <$> pFirstName  <*> pMany pLookup
             <|> (:) <$> pFirstIndex <*> pMany pLookup

pFirstName :: Parser Lookup
pFirstName = Name <$> pName

pFirstIndex :: Parser Lookup
pFirstIndex = Name <$> pName

pLookup :: Parser Lookup
pLookup = Index <$> pIndex
        <|> Name <$> pChildName

pIndex :: Parser Index
pIndex = pSquareBrackets pPosInt

pName :: Parser Name
pName = pSome (pCondition validChar pAnyChar)
    where
        validChar :: Char -> Bool
        validChar '.' = False
        validChar '[' = False
        validChar ']' = False
        validChar _   = True

pChildName :: Parser Name
pChildName = pDot *> pName

pDot :: Parser Char
pDot = pChar '.'


----- Parser utilities -----


-- Running the parser
runParser :: Parser a -> String -> Either () a
runParser (Parser h) s = 
    case h s of
        Right (val, "") -> Right val
        _               -> Left ()

-- Parsing some and many and the fail/succeed parsers
pSome :: Parser a -> Parser [a]
pSome = some

pMany :: Parser a -> Parser [a]
pMany = many

pFail :: Parser a
pFail = empty

pSucceed :: a -> Parser a
pSucceed = pure

-- Parse a value and return it if it satisfies the condition, otherwise return an error message
pCondition :: (a -> Bool) -> Parser a -> Parser a
pCondition p g = g >>= \v -> if p v then pure v else pFail

-- Parse chars and strings
pAnyChar :: Parser Char
pAnyChar = 
    Parser 
        (\s ->
            case s of
                []     -> Left  ()
                (x:xs) -> Right (x, xs)
        )
    
pChar :: Char -> Parser Char
pChar x = pCondition (== x) pAnyChar

pString :: String -> Parser String
pString = traverse pChar

-- Parse enclosed expressions, in this case expression between square brackets
pEnclosed :: Char -> Parser a -> Char -> Parser a
pEnclosed c1 p c2 = pChar c1 *> p <* pChar c2

pSquareBrackets :: Parser a -> Parser a
pSquareBrackets p = pEnclosed '[' p ']'

-- Parse digits/integers
pDigit :: Parser Int 
pDigit = digitToInt <$> pCondition isDigit pAnyChar

pPosInt :: Parser Int
pPosInt = foldl (\xs' x -> 10 * xs' + x) 0 <$> pSome pDigit
        
