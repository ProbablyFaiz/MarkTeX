module Evaluation.Predicates where

import MarkTeX.Evaluation.Expression
import Data.Data (toConstr)

data EvalPredicate = ContainsExpr Expr | NotContainsExpr Expr | ReturnsError

-- | Searches for a `Expr` in a `Expr`, an empty Seq [] Expr is treated as a wildcard
containsPredicate :: Expr -> Expr -> Bool
containsPredicate (Seq []) _ = True -- Empty RootSeq is used as a wildcard
containsPredicate x y = if toConstr x == toConstr y 
    -- Match contents or look further down the AST
    then allMatch (innerExprs x) (innerExprs y) ||
         any (containsPredicate x) (innerExprs y)
    -- If not the same constructor look further down the AST
    else any (containsPredicate x) (innerExprs y)

-- | Matches all inner expressions of an expression
allMatch :: [Expr] -> [Expr] -> Bool
allMatch []       []             = True 
allMatch []       ys             = False 
allMatch xs       []             = False
-- Don't match on newline's
allMatch (NewLine : xs) (y : ys) = allMatch xs (y : ys)
allMatch (x : xs) (NewLine : ys) = allMatch (x : xs) ys
allMatch (x : xs) (y : ys)       = match x y && allMatch xs ys

match :: Expr -> Expr -> Bool 
match (Seq []) y  = True 
match x        y  = x == y

innerExprs :: Expr -> [Expr]
innerExprs (Heading i e)      = [Text (show i), e]
innerExprs (OrderedList es)   = es
innerExprs (UnorderedList es) = es
innerExprs NewLine            = []
innerExprs (Seq es)           = es
innerExprs (Text _)           = []
innerExprs (Bold e)           = [e]
innerExprs (Italic e)         = [e]
innerExprs (Hyperlink e1 e2)  = [e1, e2]
innerExprs (Image e1 e2)      = [e1, e2]
