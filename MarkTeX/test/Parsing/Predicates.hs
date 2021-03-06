module Parsing.Predicates where

import MarkTeX.Parsing.Expression
import Data.Data (toConstr)
import Data.Either (rights, lefts)

type AnyExpr = Either Expr RootExpr

data ParserPredicate = ContainsExpr Expr | ContainsRootExpr RootExpr

-- | Searches for a `RootExpr` in a `RootExpr`
containsREPredicate :: RootExpr -> RootExpr -> Bool
containsREPredicate (RootSeq []) _ = True -- Empty RootSeq is used as a wildcard
containsREPredicate x y = if toConstr x == toConstr y 
    -- Match contents or look further down the AST
    then allMatch (innerExprsRE x) (innerExprsRE y) ||
         any (containsREPredicate x) (rights (innerExprsRE y)) 
    -- If not the same constructor look further down the AST
    else any (containsREPredicate x) (rights (innerExprsRE y))

-- | Searches for an `Expr` in a `RootExpr`
containsEPredicate :: Expr -> RootExpr -> Bool
containsEPredicate (Seq []) _ = True -- Empty Seq is used as a wildcard
-- Recursively check this property in case of RootExpr, go to isExprInExpr when an Expr is encountered
containsEPredicate x y = any (containsEPredicate x) (rights (innerExprsRE y)) ||
                         any (isExprInExpr x) (lefts (innerExprsRE y))

-- | Searches for an `Expr` in an `Expr`
isExprInExpr :: Expr -> Expr -> Bool 
isExprInExpr (Seq []) _ = True 
isExprInExpr x y = if toConstr x == toConstr y
    then allMatch (map Left $ innerExprsE x) (map Left $ innerExprsE y) ||
         any (isExprInExpr x) (innerExprsE y)
    else any (isExprInExpr x) (innerExprsE y)

-- | Matches all inner expressions of an expression
allMatch :: [AnyExpr] -> [AnyExpr] -> Bool
allMatch []       []       = True 
allMatch []       _        = False 
allMatch _        []       = False
allMatch (x : xs) (y : ys) = match x y && allMatch xs ys

match :: AnyExpr -> AnyExpr -> Bool 
match (Left (Seq []))      (Left _)  = True 
match (Left x)             (Left y)  = x == y
match (Right (RootSeq [])) (Right _) = True 
match (Right x)            (Right y) = x == y
match _                    _         = False

innerExprsRE :: RootExpr -> [AnyExpr]
innerExprsRE (Heading i e)             = [Left $ Text (show i), Left e]
innerExprsRE (Body e)                  = [Left e]
innerExprsRE (OrderedList es)          = map Left es
innerExprsRE (UnorderedList es)        = map Left es
innerExprsRE NewLine                   = []
innerExprsRE (CommandBlockCode str re) = [Left $ Text str, Right re]
innerExprsRE (RootSeq res)             = map Right res
innerExprsRE (CodeSnippet _)           = []

innerExprsE :: Expr -> [Expr]
innerExprsE (Seq es)          = es
innerExprsE (Text _)          = []
innerExprsE (Bold e)          = [e]
innerExprsE (Italic e)        = [e]
innerExprsE (Hyperlink e1 e2) = [e1, e2]
innerExprsE (Image e1 e2)     = [e1, e2]
innerExprsE (CommandCode _)   = []
